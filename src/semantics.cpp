#include <utility>
#include <optional>

#include <mpark/patterns.hpp>

#include "semantics.h"
#include "compiler_errors.h"

namespace Sema {

#define make_primative_type(type_tag) \
  static auto type_tag = std::make_shared<Type>(PrimativeType(PrimativeTypeTag::type_tag))

#define IMPOSSIBLE() assert(false && "should be unreachable");

namespace Primative {
  make_primative_type(FLOAT32);
  make_primative_type(FLOAT64);
  make_primative_type(INTEGER);
  make_primative_type(STRING);
};

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

template<typename T>
[[ noreturn ]] void throw_sema_error_at(Ast_Identifier const & ident, T format_pattern) {
  throw_compile_error(ident.location, format_pattern, ident.name);
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

}

void analyse_statement(Ast_Statement& stmt, Scope* scope, Type* return_type) {

}

Type_Ptr analyse_expression(Ast_Expression& expr, Scope* scope) {

}

Type_Ptr analyse_binary_expression(Ast_Binary_Operation& expr, Scope* scope) {

}

Type_Ptr analyse_call(Ast_Call& expr, Scope* scope) {

}


}
