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
  make_primative_type(BOOL);
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
    std::make_pair("bool", &Primative::BOOL),
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
    },
    pattern(as<Ast_Return_Statement>(arg)) = [&](auto& return_stmt){
      auto expr_type = analyse_expression(*return_stmt.expression, scope);
      if (!match_types(expr_type.get(), return_type)) {
        throw_sema_error_at(return_stmt.expression,
          "invalid return type, expected {}, was {}",
          type_to_string(return_type), type_to_string(expr_type.get()));
      }
    },
    pattern(as<Ast_If_Statement>(arg)) = [&](auto& if_stmt){
      auto cond_type = analyse_expression(*if_stmt.cond, scope);
      if (!match_types(cond_type.get(), Primative::BOOL.get())) {
        throw_sema_error_at(if_stmt.cond,
          "if condition must be a {}", type_to_string(*Primative::BOOL));
      }
      analyse_statement(*if_stmt.then_block, scope, return_type);
      if (if_stmt.has_else) {
        analyse_statement(*if_stmt.else_block, scope, return_type);
      }
    }
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
        case PrimativeTypeTag::BOOL:
          return Primative::BOOL;
        default:
          throw_sema_error_at(lit, "fix me! unknown literal type!");
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
    pattern(as<Ast_Unary_Operation>(arg)) = [&](auto& unary) {
      return analyse_unary_expression(unary, scope);
    },
    pattern(as<Ast_Binary_Operation>(arg)) = [&](auto& binop) {
      return analyse_binary_expression(binop, scope);
    },
    pattern(_) = [&]{
      throw_sema_error_at(expr, "fix me! unknown expression type!");
      return Type_Ptr(nullptr);
    }
  );
}

Type_Ptr analyse_unary_expression(Ast_Unary_Operation& unary, Scope& scope) {
  unary.operand->type = analyse_expression(*unary.operand, scope);
  PrimativeType* primative_type = std::get_if<PrimativeType>(&unary.operand->type->v);

  if (primative_type)
  switch (unary.operation) {
    case Ast_Operator::PLUS:
    case Ast_Operator::MINUS: {
      if (numeric_type(primative_type->tag)) {
        return unary.operand->type;
      }
      break;
    }
    case Ast_Operator::BITWISE_NOT: {
      if (integer_type(primative_type->tag)) {
        return unary.operand->type;
      }
      break;
    }
    case Ast_Operator::LOGICAL_NOT: {
      if (boolean_type(primative_type->tag)) {
        return unary.operand->type;
      }
      break;
    }
    default: break;
  }

  throw_sema_error_at(unary, "invalid unary operation for {}",
    type_to_string(unary.operand->type.get()));
}


Type_Ptr analyse_binary_expression(Ast_Binary_Operation& binop, Scope& scope) {
  using namespace mpark::patterns;
  binop.left->type = analyse_expression(*binop.left, scope);
  binop.right->type = analyse_expression(*binop.right, scope);

  if (!match_types(binop.left->type.get(), binop.right->type.get())) {
    throw_sema_error_at(binop, "incompatible types {} and {}",
      type_to_string(binop.left->type.get()), type_to_string(binop.right->type.get()));
  }

  PrimativeType* primative_type = std::get_if<PrimativeType>(&binop.left->type->v);
  return match(primative_type, binop.operation)(
    pattern(some(_), anyof(
      Ast_Operator::PLUS, Ast_Operator::MINUS, Ast_Operator::TIMES,
      Ast_Operator::DIVIDE
    )) = [&]{
      WHEN(numeric_type(primative_type->tag)) {
        return binop.left->type;
      };
    },
    pattern(some(_), anyof(
      Ast_Operator::LEFT_SHIFT, Ast_Operator::RIGHT_SHIFT, Ast_Operator::BITWISE_AND,
      Ast_Operator::BITWISE_OR, Ast_Operator::BITWISE_XOR, Ast_Operator::MODULO
    )) = [&]{
      WHEN(integer_type(primative_type->tag)) {
        return binop.left->type;
      };
    },
    pattern(some(_), anyof(
      Ast_Operator::LESS_THAN, Ast_Operator::LESS_EQUAL, Ast_Operator::GREATER_THAN,
      Ast_Operator::GREATER_EQUAL, Ast_Operator::EQUAL_TO, Ast_Operator::NOT_EQUAL_TO
    )) = [&]{
      WHEN(numeric_type(primative_type->tag)) {
        return Primative::BOOL;
      };
    },
    pattern(_, _) = [&]{
      throw_sema_error_at(binop, "invalid operation for {}",
        type_to_string(binop.left->type.get()));
      return Type_Ptr(nullptr);
    }
  );
}

#define NOT_CALLABLE "{} is not callable"

Type_Ptr analyse_call(Ast_Call& call, Scope& scope) {
  //Symbol* called_function = scope.lookup_first_name(expr.callee)
  auto called_function_ident = std::get_if<Ast_Identifier>(&call.callee->v);
  if (!called_function_ident) {
    auto callee_type = analyse_expression(*call.callee, scope);
    throw_sema_error_at(call.callee, NOT_CALLABLE, type_to_string(callee_type.get()));
  }

  Symbol* called_function = scope.lookup_first_name(*called_function_ident);
  if (!called_function) {
    throw_sema_error_at(*called_function_ident, "attempting to call undefined function \"{}\"",
      called_function_ident->name);
  }

  if (called_function->kind != Symbol::FUNCTION) {
    throw_sema_error_at(*called_function_ident, NOT_CALLABLE,
      type_to_string(called_function->type.get()));
  }

  auto& function_type = std::get<Ast_Function_Declaration>(called_function->type->v);

  if (function_type.arguments.size() != call.arguments.size()) {
    throw_sema_error_at(call.callee, "{} expects {} arguments not {}",
      function_type.identifer.name, function_type.arguments.size(), call.arguments.size());
  }

  for (uint arg_idx = 0; arg_idx < function_type.arguments.size(); arg_idx++) {
    auto& expected_type = *function_type.arguments.at(arg_idx).type;
    auto& argument = *call.arguments.at(arg_idx);
    auto given_type = analyse_expression(argument, scope);
    if (!match_types(&expected_type, given_type.get())) {
      throw_sema_error_at(argument,
        "expected to be called with {} but found {}",
        type_to_string(expected_type), type_to_string(given_type.get()));
    }
  }

  call.callee->type = called_function->type;
  // FINALLY we've checked everything in the call!
  return function_type.return_type;
}

}
