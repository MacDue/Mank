#include <utility>
#include <optional>

#include <mpark/patterns.hpp>

#include "ast/types.h"
#include "sema/semantics.h"
#include "sema/sema_errors.h"
#include "sema/const_propagate.h"
#include "sema/return_reachability.h"

#define make_primative_type(type_tag) \
  static auto type_tag = std::make_shared<Type>(PrimativeType(PrimativeType::Tag::type_tag))

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

void Semantics::analyse_file(Ast_File& file) {
  Scope& global_scope = file.scope;

  static std::array primative_types {
    std::make_pair("f32", &Primative::FLOAT32),
    std::make_pair("f64", &Primative::FLOAT64),
    std::make_pair("i32", &Primative::INTEGER),
    std::make_pair("string", &Primative::STRING),
    std::make_pair("bool", &Primative::BOOL),
  };

  for (auto [type_name, type] : primative_types) {
    global_scope.add(
      Symbol(SymbolName(type_name), *type, Symbol::TYPE));
  }

  /* Add symbols for (yet to be checked) pods */
  for (auto& pod_type: file.pods) {
    auto& pod = std::get<Ast_Pod_Declaration>(pod_type->v);
    file.scope.add(
      Symbol(pod.identifier, pod_type, Symbol::TYPE));
  }

  /* Check pods */
  for (auto& pod_type: file.pods) {
    analyse_pod(std::get<Ast_Pod_Declaration>(pod_type->v), global_scope);
  }

  /* Add function headers into scope, resolve function return/param types */
  for (auto& func_type: file.functions) {
    auto& func = std::get<Ast_Function_Declaration>(func_type->v);
    file.scope.add(
      Symbol(func.identifier, func_type, Symbol::FUNCTION));
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
      Symbol* symbol = scope.lookup_first_name(unchecked.identifier);
      auto resolved_type = symbol && symbol->kind == Symbol::TYPE
        ? symbol->type : nullptr;
      return std::make_pair(resolved_type, std::optional{unchecked.identifier});
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

void Semantics::analyse_pod(Ast_Pod_Declaration& pod, Scope& scope) {
  // TODO: Nested pods and recursive pod checking
  for (auto& field: pod.fields) {
    resolve_type_or_fail(scope, field.type, "undeclared field type {}");
    if (std::get_if<Ast_Pod_Declaration>(&field.type->v)) {
      throw_sema_error_at(field.name, "nested pods are not yet implemented");
      field.type = nullptr;
    }
  }
}

void Semantics::analyse_function_header(Ast_Function_Declaration& func) {
  /* Try to resolve arg and return types */
  Scope& parent_scope = *func.body.scope.parent;
  if (!func.procedure) {
    resolve_type_or_fail(parent_scope, func.return_type, "undeclared return type {}");
  }
  for (auto& arg: func.arguments) {
    resolve_type_or_fail(parent_scope, arg.type, "undeclared arg type {}");
  }
}

Type_Ptr Semantics::analyse_block(Ast_Block& block, Scope& scope) {
  block.scope.parent = &scope;
  for (auto& stmt: block.statements) {
    analyse_statement(*stmt, block.scope);
  }
  Type_Ptr block_type;
  if (auto final_expr = block.get_final_expr()) {
    block_type = extract_type_nullable(final_expr->meta.type);
  }
  block.scope.destroy_locals();
  return block_type;
}

void Semantics::analyse_function_body(Ast_Function_Declaration& func) {
  for (auto& arg: func.arguments) {
    func.body.scope.add(
      Symbol(arg.name, arg.type, Symbol::INPUT));
  }
  this->expected_return = func.return_type.get();
  auto body_type = analyse_block(func.body, *func.body.scope.parent);

  auto final_expr = func.body.get_final_expr();
  if (final_expr) {
    if (body_type && match_types(body_type.get(), func.return_type.get())) {
      /* Desugar implict return for codegen + return checking ease */
      func.body.has_final_expr = false;
      Ast_Return_Statement implicit_return;
      implicit_return.location
        = std::get<Ast_Expression_Statement>(
            func.body.statements.back()->v).location;
      implicit_return.expression = final_expr;
      func.body.statements.pop_back();
      func.body.statements.emplace_back(
        std::make_shared<Ast_Statement>(implicit_return));
    }
  }

  // Propagate constants before checking reachability so dead code can be marked
  AstHelper::constant_propagate(func.body);

  Ast_Statement* first_unreachable_stmt = nullptr;
  bool all_paths_return = AstHelper::all_paths_return(func.body, &first_unreachable_stmt);
  if (!func.procedure && !all_paths_return) {
    throw_sema_error_at(func.identifier, "function possibly fails to return a value");
  }

  if (func.body.has_final_expr && body_type) {
    throw_sema_error_at(final_expr,
      "implicit return of type {} does not match expected type {}",
      type_to_string(body_type.get()), type_to_string(func.return_type.get()));
  }

  if (first_unreachable_stmt) {
    emit_warning_at(*first_unreachable_stmt, "unreachable code");
  }
}

static bool expression_may_have_side_effects(Ast_Expression& expr) {
  using namespace mpark::patterns;
  /*
    bit dumb because a function call could not have side effects making
    the expression still useless (sorry haskell)*/
  return match(expr.v)(
    pattern(as<Ast_Unary_Operation>(arg)) = [](auto& unary) {
      return expression_may_have_side_effects(*unary.operand);
    },
    pattern(as<Ast_Binary_Operation>(arg)) = [](auto& binop) {
      return expression_may_have_side_effects(*binop.left)
        || expression_may_have_side_effects(*binop.right);
    },
    pattern(as<Ast_Call>(_)) = []{
      return true;
    },
    pattern(as<Ast_Block>(arg)) = [](auto& block) {
      // If the block is just a expression
      if (block.statements.size() == 1) {
        if (auto final_expr = block.get_final_expr()) {
          return expression_may_have_side_effects(*final_expr);
        }
      }
      return block.statements.size() > 0;
    },
    pattern(as<Ast_If_Expr>(arg)) = [](auto& if_expr) {
      bool then_has = expression_may_have_side_effects(*if_expr.then_block);
      if (if_expr.has_else) {
        bool else_has = expression_may_have_side_effects(*if_expr.else_block);
        return then_has || else_has;
      }
      return then_has;
    },
    pattern(_) = []() { return false; }
  );
}

void Semantics::analyse_expression_statement(Ast_Expression_Statement& expr_stmt, Scope& scope) {
  auto expression_type = analyse_expression(*expr_stmt.expression, scope);
  if (!expr_stmt.final_expr) {
    bool is_call = std::get_if<Ast_Call>(&expr_stmt.expression->v) != nullptr;
    if (!expression_may_have_side_effects(*expr_stmt.expression)) {
      emit_warning_at(expr_stmt.expression, "statement has no effect");
    } else if (!is_call && expression_type) {
      emit_warning_at(expr_stmt.expression, "result of expression discarded");
    } else if (expression_type) {
      throw_sema_error_at(expr_stmt.expression, "return value discarded");
    }
  }
}

void Semantics::analyse_assignment(Ast_Assign& assign, Scope& scope) {
  using namespace mpark::patterns;

  auto target_type = match(assign.target->v)(
    pattern(as<Ast_Identifier>(_)) = [&]{
      return analyse_expression(*assign.target, scope);
    },
    pattern(as<Ast_Field_Access>(arg)) = [&](auto access) {
      if (!std::get_if<Ast_Identifier>(&access.object->v)) {
        throw_sema_error_at(assign.target,
          "complex and nested field assigment is not implemented");
      }
      return analyse_expression(*assign.target, scope);
    },
    pattern(_) = [&]{
      throw_sema_error_at(assign.target, "assignment target is not assignable");
      return Type_Ptr(nullptr);
    }
  );

  auto expr_type = analyse_expression(*assign.expression, scope);

  if (!match_types(target_type.get(), expr_type.get())) {
    throw_sema_error_at(assign, "cannot assign variable of type {} to {}",
      type_to_string(target_type.get()), type_to_string(expr_type.get()));
  }
}

void Semantics::analyse_statement(Ast_Statement& stmt, Scope& scope) {
  using namespace mpark::patterns;
  match(stmt.v)(
    pattern(as<Ast_Expression_Statement>(arg)) = [&](auto& expr_stmt){
      analyse_expression_statement(expr_stmt, scope);
    },
    pattern(as<Ast_Assign>(arg)) = [&](auto& assign){
      analyse_assignment(assign, scope);
    },
    pattern(as<Ast_For_Loop>(arg)) = [&](auto& for_loop) {
      analyse_for_loop(for_loop, scope);
    },
    pattern(as<Ast_Return_Statement>(arg)) = [&](auto& return_stmt){
      Type_Ptr expr_type;
      if (return_stmt.expression) {
        expr_type = analyse_expression(*return_stmt.expression, scope);
      } else if (expected_return) {
        throw_sema_error_at(return_stmt, "a function needs to return a value");
      }
      if (!match_types(expr_type.get(), expected_return)) {
        throw_sema_error_at(return_stmt.expression,
          "invalid return type, expected {}, was {}",
          type_to_string(expected_return), type_to_string(expr_type.get()));
      }
    },
    pattern(as<Ast_Variable_Declaration>(arg)) = [&](auto& var_decl) {
      if (var_decl.type) {
        resolve_type_or_fail(scope, var_decl.type, "undeclared type {}");
      } else if (!var_decl.initializer) {
        throw_sema_error_at(var_decl, "cannot infer type of {} without initializer",
          var_decl.variable.name);
      }
      if (var_decl.initializer) {
        auto initializer_type = analyse_expression(*var_decl.initializer, scope);
        if (var_decl.type) {
          if (!match_types(var_decl.type.get(), initializer_type.get())) {
            throw_sema_error_at(var_decl.initializer,
              "initializer type {} does not match declaration type {}",
              type_to_string(initializer_type.get()), type_to_string(var_decl.type.get()));
          }
        } else {
          if (!initializer_type) {
            throw_sema_error_at(var_decl.initializer, "cannot initialize variable with type {}",
              type_to_string(initializer_type.get()));
          }
          var_decl.type = initializer_type;
        }
      } else {
        emit_warning_at(var_decl, "default initialization is currently unimplemented");
      }

      Symbol* pior_symbol = scope.lookup_first_name(var_decl.variable);
      if (pior_symbol) {
        emit_warning_at(var_decl.variable, "declaration shadows existing symbol");
      }

      scope.add(
        Symbol(var_decl.variable, var_decl.type, Symbol::LOCAL));
    }
  );
}

void Semantics::analyse_for_loop(Ast_For_Loop& for_loop, Scope& scope) {
  auto start_range_type = analyse_expression(*for_loop.start_range, scope);
  if (for_loop.value_type) {
    resolve_type_or_fail(scope, for_loop.value_type, "undeclared loop type {}");
    if (!match_types(for_loop.value_type.get(), start_range_type.get())) {
      throw_sema_error_at(for_loop.start_range,
        "start range type type {} does not match loop value type {}",
        type_to_string(start_range_type.get()), type_to_string(for_loop.value_type.get()));
    }
  } else {
    if (!start_range_type) {
      throw_sema_error_at(for_loop.start_range, "loop value cannot be type {}",
        type_to_string(start_range_type.get()));
    }
    for_loop.value_type = start_range_type;
  }

  auto end_range_type = analyse_expression(*for_loop.end_range, scope);
  for_loop.end_range->meta.type = end_range_type;
  if (!match_types(start_range_type.get(), end_range_type.get())) {
    throw_sema_error_at(for_loop.end_range,
      "end range type {} does not match start range type {}",
      type_to_string(end_range_type.get()), type_to_string(start_range_type.get()));
  }

  Symbol* pior_symbol = scope.lookup_first_name(for_loop.loop_value);
  if (pior_symbol) {
    emit_warning_at(for_loop.loop_value, "loop value shadows existing symbol");
  }

  auto& body = for_loop.body;
  body.scope.add(Symbol(
    for_loop.loop_value, for_loop.value_type, Symbol::LOCAL));

  auto body_type = analyse_block(body, scope);
  if (body_type) {
    throw_sema_error_at(body.get_final_expr(), "loop body should not evaluate to a value");
  }
}

Type_Ptr Semantics::analyse_expression(Ast_Expression& expr, Scope& scope) {
  using namespace mpark::patterns;
  auto expr_type = match(expr.v)(
    pattern(as<Ast_Literal>(arg)) = [&](auto& lit) {
      switch (lit.literal_type) {
        // Only want one type object per primative type
        case PrimativeType::INTEGER:
          return Primative::INTEGER;
        case PrimativeType::FLOAT32:
          return Primative::FLOAT32;
        case PrimativeType::FLOAT64:
          return Primative::FLOAT64;
        case PrimativeType::STRING:
          return Primative::STRING;
        case PrimativeType::BOOL:
          return Primative::BOOL;
        default:
          throw_sema_error_at(lit, "fix me! unknown literal type!");
      }
    },
    pattern(as<Ast_Block>(arg)) = [&](auto& block) {
      return analyse_block(block, scope);
    },
    pattern(as<Ast_If_Expr>(arg)) = [&](auto& if_expr){
      auto cond_type = analyse_expression(*if_expr.cond, scope);
      if (!match_types(cond_type.get(), Primative::BOOL.get())) {
        throw_sema_error_at(if_expr.cond,
          "if condition must be a {}", type_to_string(*Primative::BOOL));
      }
      auto then_type = analyse_expression(*if_expr.then_block, scope);
      if (if_expr.has_else) {
        auto else_type = analyse_expression(*if_expr.else_block, scope);
        if (!match_types(then_type.get(), else_type.get())) {
          throw_sema_error_at(if_expr,
            "type of then block {} does not match else block {}",
            type_to_string(then_type.get()), type_to_string(else_type.get()));
        }
      } else if (then_type) {
        auto final_expr = std::get<Ast_Block>(if_expr.then_block->v).get_final_expr();
        throw_sema_error_at(final_expr,
          "if expression cannot evaluate to {} without a matching else",
          type_to_string(then_type.get()));
      }
      return then_type;
    },
    pattern(as<Ast_Identifier>(arg)) = [&](auto& ident) {
      Symbol* symbol = scope.lookup_first_name(ident);
      if (!symbol || !symbol->is_local()) {
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
    pattern(as<Ast_Field_Access>(arg)) = [&](auto& access) {
      auto object_type = analyse_expression(*access.object, scope);
      if (object_type) {
        if (auto pod_type = std::get_if<Ast_Pod_Declaration>(&object_type->v)) {
          auto accessed = std::find_if(pod_type->fields.begin(), pod_type->fields.end(),
            [&](auto& field) {
              return field.name.name == access.field.name;
            });
          if (accessed == pod_type->fields.end()) {
            throw_sema_error_at(access.field, "{} has no field named \"{}\"",
              pod_type->identifier.name, access.field.name);
          }
          access.field_index = std::distance(pod_type->fields.begin(), accessed);
          return accessed->type;
        }
      }
      throw_sema_error_at(access.object, "not a pod type");
    },
    pattern(_) = [&]{
      throw_sema_error_at(expr, "fix me! unknown expression type!");
      return Type_Ptr(nullptr);
    }
  );
  expr.meta.type = expr_type;
  return expr_type;
}

Type_Ptr Semantics::analyse_unary_expression(Ast_Unary_Operation& unary, Scope& scope) {
  auto operand_type = analyse_expression(*unary.operand, scope);
  PrimativeType* primative_type = std::get_if<PrimativeType>(&operand_type->v);

  if (primative_type)
  switch (unary.operation) {
    case Ast_Operator::MINUS:
    case Ast_Operator::PLUS: {
      if (primative_type->is_numeric_type()) {
        return operand_type;
      }
      break;
    }
    case Ast_Operator::BITWISE_NOT: {
      if (primative_type->is_integer_type()) {
        return operand_type;
      }
      break;
    }
    case Ast_Operator::LOGICAL_NOT: {
      if (primative_type->is_boolean_type()) {
        return operand_type;
      }
      break;
    }
    default: break;
  }

  throw_sema_error_at(unary, "invalid unary operation for {}",
    type_to_string(operand_type.get()));
}

Type_Ptr Semantics::analyse_binary_expression(Ast_Binary_Operation& binop, Scope& scope) {
  using namespace mpark::patterns;
  auto left_type = analyse_expression(*binop.left, scope);
  auto right_type = analyse_expression(*binop.right, scope);

  if (!match_types(left_type.get(), right_type.get())) {
    throw_sema_error_at(binop, "incompatible types {} and {}",
      type_to_string(left_type.get()), type_to_string(right_type.get()));
  }

  PrimativeType* primative_type = std::get_if<PrimativeType>(&left_type->v);
  auto binop_type = match(primative_type, binop.operation)(
    pattern(some(_), anyof(
      Ast_Operator::PLUS, Ast_Operator::MINUS, Ast_Operator::TIMES,
      Ast_Operator::DIVIDE
    )) = [&]{
      WHEN(primative_type->is_numeric_type()) {
        return left_type;
      };
    },
    pattern(some(_), anyof(
      Ast_Operator::LEFT_SHIFT, Ast_Operator::RIGHT_SHIFT, Ast_Operator::BITWISE_AND,
      Ast_Operator::BITWISE_OR, Ast_Operator::BITWISE_XOR, Ast_Operator::MODULO
    )) = [&]{
      WHEN(primative_type->is_integer_type()) {
        return left_type;
      };
    },
    pattern(some(_), anyof(
      Ast_Operator::LESS_THAN, Ast_Operator::LESS_EQUAL, Ast_Operator::GREATER_THAN,
      Ast_Operator::GREATER_EQUAL, Ast_Operator::EQUAL_TO, Ast_Operator::NOT_EQUAL_TO
    )) = [&]{
      WHEN(primative_type->is_numeric_type()) {
        return Primative::BOOL;
      };
    },
    pattern(_, _) = [&]{
      throw_sema_error_at(binop, "invalid operation for {}",
        type_to_string(left_type.get()));
      return Type_Ptr(nullptr);
    }
  );
  return binop_type;
}

#define NOT_CALLABLE "{} is not callable"

Type_Ptr Semantics::analyse_call(Ast_Call& call, Scope& scope) {
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
      function_type.identifier.name, function_type.arguments.size(), call.arguments.size());
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

  call.callee->meta.type = called_function->type;
  // FINALLY we've checked everything in the call!
  return function_type.return_type;
}
