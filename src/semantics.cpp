#include <utility>
#include <optional>

#include <mpark/patterns.hpp>

#include "semantics.h"
#include "sema_errors.h"

namespace Sema {

#define make_primative_type(type_tag) \
  static auto type_tag = std::make_shared<Type>(PrimativeType(PrimativeTypeTag::type_tag))

namespace Primative {
  make_primative_type(FLOAT32);
  make_primative_type(FLOAT64);
  make_primative_type(INTEGER);
  make_primative_type(STRING);
};

static bool match_types(Type* a, Type* b) {
  using namespace mpark::patterns;
  if (a == b) {
    return true;
  } else if (a && b) {
    match(a->v, b->v)(
      pattern(as<PrimativeType>(arg), as<PrimativeType>(arg)) =
        [](auto& a, auto& b) {
          return a.tag == b.tag;
        },
      pattern(_, _) = []{ return false; });
  }
  return false;
}

void analyse_file(Ast_File& file) {
  Scope& global_scope = file.scope;

  static std::array primative_types {
    std::make_pair("f32", &Primative::FLOAT32),
    std::make_pair("f64", &Primative::FLOAT64),
    std::make_pair("i32", &Primative::INTEGER),
    std::make_pair("string", &Primative::STRING),
  };

  for (auto [type_name, type] : primative_types) {
    global_scope.symbols.emplace_back(
      Symbol(SymbolName(type_name), *type, Symbol::TYPE));
  }

  /* Add function headers into scope, resolve function return/param types */
  for (auto& func_type: file.functions) {
    auto& func = std::get<Ast_Function_Declaration>(func_type->v);
    file.scope.symbols.emplace_back(
      Symbol(func.identifer, func_type, Symbol::FUNCTION));
    func.body.scope.parent = &global_scope;
    analyse_function_header(func);
  }

  /* Check functions */
  for (auto& func_type: file.functions) {
    auto& func = std::get<Ast_Function_Declaration>(func_type->v);
    analyse_function_body(func);
  }
}

static std::pair<Type_Ptr, std::optional<Ast_Identifier>>
  resolve_type(Scope& scope, Type& unresolved
) {
  using namespace mpark::patterns;
  return match(unresolved.v)(
    pattern(as<UncheckedType>(arg)) = [&](auto const & unchecked) {
      Symbol* symbol = scope.lookup_first_name(unchecked.identifer);
      auto resolved_type = symbol && symbol->kind == Symbol::TYPE
        ? symbol->type : nullptr;
      return std::make_pair(resolved_type, std::optional{unchecked.identifer});
    },
    pattern(_) = [&] {
      return std::make_pair(Type_Ptr(nullptr), std::optional<Ast_Identifier>{});
    });
}

template<typename T>
static void resolve_type_or_fail(Scope& scope, Type_Ptr& to_resolve, T error_format) {
  auto [ resolved_type, type_slot ] = resolve_type(scope, *to_resolve);
  if (resolved_type) {
    to_resolve = resolved_type;
  } else if (type_slot) {
    throw_sema_error_at(*type_slot, error_format);
  } else {
    IMPOSSIBLE();
  }
}

void analyse_function_header(Ast_Function_Declaration& func) {
  /* Try to resolve arg and return types */
  Scope& parent_scope = *func.body.scope.parent;
  if (!func.procedure) {
    resolve_type_or_fail(parent_scope, func.return_type, "undeclared return type {}");
  }
  for (auto& arg: func.arguments) {
    resolve_type_or_fail(parent_scope, arg.type, "undeclared arg type {}");
  }
}

void analyse_function_body(Ast_Function_Declaration& func) {
  for (auto& arg: func.arguments) {
    func.body.scope.symbols.emplace_back(
      Symbol(arg.name, arg.type, Symbol::LOCAL));
  }
  for (auto& stmt: func.body.statements) {
    analyse_statement(*stmt, func.body.scope, func.return_type.get());
  }
}

static bool expression_may_have_side_effects(Ast_Expression& expr) {
  using namespace mpark::patterns;
  /*
    bit dumb because a function call could not have side effects making
    the expression still useless (sorry haskell)*/
  return match(expr.v)(
    pattern(as<Ast_Unary_Operation>(arg)) = [](auto const & unary) {
      return expression_may_have_side_effects(*unary.operand);
    },
    pattern(as<Ast_Binary_Operation>(arg)) = [](auto const & binop) {
      return expression_may_have_side_effects(*binop.left)
        || expression_may_have_side_effects(*binop.right);
    },
    pattern(as<Ast_Call>(_)) = []{ return true; },
    pattern(_) = []() { return false; }
  );
}

static void analyse_expression_statement(Ast_Expression_Statement& expr_stmt, Scope& scope) {
  if (!expression_may_have_side_effects(*expr_stmt.expression)) {
    throw_sema_error_at(expr_stmt.expression, "statement has no effect");
  }
  auto expression_type = analyse_expression(*expr_stmt.expression, scope);
  if (expression_type) {
    throw_sema_error_at(expr_stmt.expression, "return value discarded");
  }
}

void analyse_statement(Ast_Statement& stmt, Scope& scope, Type* return_type) {
  using namespace mpark::patterns;
  match(stmt.v)(
    pattern(as<Ast_Block>(arg)) = [&](auto& block) {
      block.scope.parent = &scope;
      for (auto stmt: block.statements) {
        analyse_statement(*stmt, block.scope, return_type);
      }
    },
    pattern(as<Ast_Expression_Statement>(arg)) = [&](auto& expr_stmt){
      analyse_expression_statement(expr_stmt, scope);
    }/*,
    pattern(as<Ast_Return_Statement>(arg)) = [&](auto& return_stmt){
      auto expr_type = analyse_expression(*return_stmt.expression, scope);

    },
    pattern(as<Ast_If_Statement>(arg)) = [&](auto& if_stmt){


    }*/
  );
}

Type_Ptr analyse_expression(Ast_Expression& expr, Scope& scope) {
  using namespace mpark::patterns;
  return match(expr.v)(
    pattern(as<Ast_Literal>(arg)) = [&](auto& lit) {
      switch (lit.literal_type) {
        case PrimativeTypeTag::INTEGER:
          return Primative::INTEGER;
        case PrimativeTypeTag::FLOAT32:
          return Primative::FLOAT32;
        case PrimativeTypeTag::FLOAT64:
          return Primative::FLOAT64;
        case PrimativeTypeTag::STRING:
          return Primative::STRING;
      }
    },
    pattern(as<Ast_Identifier>(arg)) = [&](auto& ident) {
      Symbol* symbol = scope.lookup_first_name(ident);
      if (!symbol || symbol->kind != Symbol::LOCAL) {
        throw_sema_error_at(ident, "{} not declared", ident.name);
      }
      return symbol->type;
    },
    pattern(as<Ast_Call>(arg)) = [&](auto& call) {
      return analyse_call(call, scope);
    },
    pattern(as<Ast_Binary_Operation>(arg)) = [&](auto& binop) {
      return analyse_binary_expression(binop, scope);
    }
  );
}

Type_Ptr analyse_binary_expression(Ast_Binary_Operation& expr, Scope& scope) {
  auto left_type = analyse_expression(*expr.left, scope);
  auto right_type = analyse_expression(*expr.right, scope);

  return nullptr;
}

Type_Ptr analyse_call(Ast_Call& expr, Scope& scope) {
  return nullptr;
}


}
