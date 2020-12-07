#include <utility>

#include <mpark/patterns.hpp>

#include "ast/expr_helpers.h"

#include "sema/types.h"
#include "sema/semantics.h"
#include "sema/sema_errors.h"
#include "sema/const_propagate.h"
#include "sema/return_reachability.h"

// Macros
#include "macros/bind_macro.h"
#include "macros/curry_macro.h"

Semantics::Semantics() {
  static bool macros_loaded = false;
  if (!macros_loaded) {
    Macros::register_macro("pbind", Macros::builtin_bind);
    Macros::register_macro("curry", Macros::builtin_curry);
    macros_loaded = true;
  }
}

Symbol* Semantics::emit_warning_if_shadows(
  Ast_Identifier& ident, Scope& scope, std::string warning
) {
  Symbol* pior_symbol = scope.lookup_first_name(ident);
  if (pior_symbol) {
    emit_warning_at(ident, warning);
  }
  return pior_symbol;
}

void Semantics::analyse_file(Ast_File& file) {
  // Setup the context
  this->ctx = &file.ctx;
  this->builder.emplace(AstBuilder(file));
  this->infer.emplace(Infer(*this->ctx, [&](CompilerMessage msg){
    warnings.emplace_back(msg);
  }));

  Scope& global_scope = file.scope;

  static std::array primative_types {
    std::make_pair("f32", PrimativeType::get(PrimativeType::FLOAT32)),
    std::make_pair("f64", PrimativeType::get(PrimativeType::FLOAT64)),
    std::make_pair("i32", PrimativeType::get(PrimativeType::INTEGER)),
    std::make_pair("str", PrimativeType::get(PrimativeType::STRING)),
    std::make_pair("bool",PrimativeType::get(PrimativeType::BOOL)),
  };

  for (auto [type_name, type] : primative_types) {
    global_scope.add(
      Symbol(SymbolName(type_name), type, Symbol::TYPE));
  }

  /* Add symbols for (yet to be checked) pods */
  for (auto& pod_type: file.pods) {
    auto& pod = std::get<Ast_Pod_Declaration>(pod_type->v);
    emit_warning_if_shadows(pod.identifier, global_scope,
      "pod declaration shadows existing symbol");
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
    // This is not very efficient, top level symbols could be placed in a hashmap
    if (auto symbol = global_scope.lookup_first_name(func.identifier)) {
      if (symbol->kind == Symbol::FUNCTION) {
        auto& pior_func = std::get<Ast_Function_Declaration>(symbol->type->v);
        throw_sema_error_at(func.identifier,
          "redeclaration of function (previously on line {})",
          pior_func.identifier.location.start_line + 1);
      } else {
        emit_warning_at(func.identifier,
          "function declaration shadows existing symbol");
      }
    }
    file.scope.add(
      Symbol(func.identifier, func_type, Symbol::FUNCTION));
    func.body.scope.set_parent(global_scope);
    analyse_function_header(func);
  }

  /* Check functions */
  for (auto& func_type: file.functions) {
    auto& func = std::get<Ast_Function_Declaration>(func_type->v);
    analyse_function_body(func);
  }
}

static int pod_is_recursive(Ast_Pod_Declaration& pod, Ast_Pod_Declaration& nested_field) {
  /*
    0 if not recursive, 1 if directly recursive, indirection steps otherwise
    Simply counts how many steps into the structure we go till the top level pod is reached.
  */
  if (&pod == &nested_field) {
    return 1;
  } else {
    for (auto& field: nested_field.fields) {
      if (auto pod_field = std::get_if<Ast_Pod_Declaration>(&field.type->v)) {
        if (auto steps = pod_is_recursive(pod, *pod_field)) {
          return 1 + steps;
        }
      }
    }
  }
  return 0;
}

void Semantics::analyse_pod(Ast_Pod_Declaration& pod, Scope& scope) {
  for (auto& field: pod.fields) {
    resolve_type_or_fail(scope, field.type, "undeclared field type {}");
    if (auto pod_field = std::get_if<Ast_Pod_Declaration>(&field.type->v)) {
      if (auto steps = pod_is_recursive(pod, *pod_field)) {
        if (steps == 1) {
          throw_sema_error_at(field.name, "directly recursive pods are not allowed");
        } else {
          throw_sema_error_at(field.name, "recursive cycle detected in pod declaration");
        }
        field.type = nullptr; // cycles bad
      }
    }
  }
}

void Semantics::analyse_function_header(Ast_Function_Declaration& func) {
  /* Try to resolve arg and return types */
  Scope& parent_scope = *func.body.scope.get_parent();
  if (!func.procedure) {
    resolve_type_or_fail(parent_scope, func.return_type, "undeclared return type {}");
  }
  for (auto& arg: func.arguments) {
    resolve_type_or_fail(parent_scope, arg.type, "undeclared arg type {}");
  }
}

Type_Ptr Semantics::analyse_block(Ast_Block& block, Scope& scope) {
  block.scope.set_parent(scope);
  for (auto& stmt: block.statements) {
    analyse_statement(*stmt, block.scope);
  }
  Type_Ptr block_type = Type::void_ty();
  if (auto final_expr = block.get_final_expr()) {
    block_type = final_expr->meta.type;
  }
  block.scope.destroy_locals();
  return block_type;
}

static Expr_Ptr make_void_expr(AstContext& ctx, SourceLocation loc) {
  Ast_Tuple_Literal void_tuple{};
  void_tuple.location = loc;
  // Make it point at just the last char '}' or ';'
  void_tuple.location.start_char_idx = loc.end_char_idx - 1;
  void_tuple.location.start_line = loc.end_line;
  void_tuple.location.start_column = loc.end_column - 1;
  auto ptr = ctx.new_expr(void_tuple);
  ptr->meta.type = Type::void_ty();
  return ptr;
}

Type_Ptr Semantics::analyse_function_body(Ast_Function_Declaration& func) {
  for (auto& arg: func.arguments) {
    func.body.scope.add(
      Symbol(arg.name, arg.type, Symbol::INPUT));
  }
  this->expected_returns.push(func.return_type);
  auto body_type = analyse_block(func.body, *func.body.scope.get_parent());
  auto expected_return = expected_returns.top();

  auto final_expr = func.body.get_final_expr();
  if (final_expr && !body_type->is_void()) {
    assert_valid_binding({}, expected_return, final_expr.get());
    /* Desugar implict return for codegen + return checking ease */
    func.body.has_final_expr = false;
    Ast_Return_Statement implicit_return;
    implicit_return.location
      = std::get<Ast_Expression_Statement>(
          func.body.statements.back()->v).location;
    implicit_return.expression = final_expr;
    func.body.statements.pop_back();
    func.body.statements.emplace_back(ctx->new_stmt(implicit_return));
  }

  // Propagate constants before checking reachability so dead code can be marked
  AstHelper::constant_propagate(func.body);
  Ast_Statement* first_unreachable_stmt = nullptr;
  bool all_paths_return = AstHelper::all_paths_return(func.body, &first_unreachable_stmt);

  if (!all_paths_return) {
    if (is_tvar(func.return_type)) { // allows void type to be infered
      Ast_Return_Statement void_return;
      void_return.expression = make_void_expr(*ctx, func.location);
      void_return.location = AstHelper::extract_location(void_return.expression);
      assert_valid_binding({}, expected_return, void_return.expression.get());
      func.body.statements.emplace_back(ctx->new_stmt(void_return));
    } else if (!func.return_type->is_void()) {
      throw_sema_error_at(func.identifier, "function possibly fails to return a value");
    }
  }

  if (first_unreachable_stmt) {
    emit_warning_at(*first_unreachable_stmt, "unreachable code");
  }

  // Infer local types
  if (!func.lambda && !this->disable_type_infer) {
    try {
      infer->unify_and_apply();
    } catch (Infer::UnifyError const & e) {
      throw_sema_error_at(func.identifier, "type inference failed ({})", e.what());
    }
  }

  this->expected_returns.pop();
  return func.return_type;
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
  bool void_expr = expression_type->is_void();
  if (!expr_stmt.final_expr) {
    bool is_call = std::get_if<Ast_Call>(&expr_stmt.expression->v) != nullptr;
    if (!expression_may_have_side_effects(*expr_stmt.expression)) {
      emit_warning_at(expr_stmt.expression, "statement has no effect");
    } else if (!is_call && !void_expr) {
      emit_warning_at(expr_stmt.expression, "result of expression discarded");
    } else if (!void_expr) {
      throw_sema_error_at(expr_stmt.expression, "return value discarded");
    }
  }
}

void Semantics::analyse_assignment(Ast_Assign& assign, Scope& scope) {
  // Note that when a tuple is used as a lhs the actual tuple type
  // is not used it's justed used as a (nested) list of lvalue idents.
  // (the types of the contained lvaues is what matters)
  auto target_type = analyse_expression(*assign.target, scope);
  if (!assign.target->is_lvalue()) {
    throw_sema_error_at(assign.target,
      "target is not an lvalue");
  }

  auto expr_type = analyse_expression(*assign.expression, scope);

  infer->match_or_constrain_types_at(assign, target_type, expr_type,
    "cannot assign variable of type {} to {}");
}

static bool is_explict_reference(Ast_Expression& expr) {
  // It is if the top level expression is a ref unary
  auto unary = std::get_if<Ast_Unary_Operation>(&expr.v);
  return unary && unary->operation == Ast_Operator::REF;
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
      auto& expected_return = expected_returns.top();
      Type_Ptr expr_type = Type::void_ty();
      if (return_stmt.expression) {
        expr_type = analyse_expression(*return_stmt.expression, scope);
      } else {
        return_stmt.expression = make_void_expr(*ctx, return_stmt.location);
      }
      if (expr_type->is_void() && !expected_return->is_void() && !is_tvar(expected_return)) {
        throw_sema_error_at(return_stmt, "a function needs to return a value");
      }
      assert_valid_binding({} /* won't be used */, expected_return, return_stmt.expression.get());
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
        // Don't implicitly duplicate references
        if (!is_explict_reference(*var_decl.initializer)) {
          initializer_type = remove_reference(initializer_type);
        }

        if (!var_decl.type) {
          var_decl.type = initializer_type;
        }
      } else {
        emit_warning_at(var_decl, "default initialization is currently unimplemented");
      }
      assert_valid_binding(var_decl.variable, var_decl.type, var_decl.initializer.get());
      emit_warning_if_shadows(var_decl.variable, scope, "declaration shadows existing symbol");
      scope.add(
        Symbol(var_decl.variable, var_decl.type, Symbol::LOCAL));
    },
    pattern(as<Ast_Tuple_Structural_Binding>(arg)) = [&](auto& binding) {
      analyse_tuple_binding_decl(binding, scope);
    }
  );
}

void Semantics::analyse_for_loop(Ast_For_Loop& for_loop, Scope& scope) {
  auto start_range_type = analyse_expression(*for_loop.start_range, scope);
  if (for_loop.type) {
    resolve_type_or_fail(scope, for_loop.type, "undeclared loop type {}");
    if (is_reference_type(for_loop.type)) {
      throw_sema_error_at(for_loop.loop_variable, "reference loop variables are not yet supported");
    }
    infer->match_or_constrain_types_at(for_loop.start_range, for_loop.type, start_range_type,
      "start range type type {1} does not match loop variable type {0}");
  } else {
    if (start_range_type->is_void()) {
      throw_sema_error_at(for_loop.start_range, "loop variable cannot be type {}",
        type_to_string(start_range_type.get()));
    }
    for_loop.type = remove_reference(start_range_type);
  }

  auto end_range_type = analyse_expression(*for_loop.end_range, scope);
  for_loop.end_range->meta.type = end_range_type;

  infer->match_or_constrain_types_at(for_loop.end_range, start_range_type, end_range_type,
    "end range type {1} does not match start range type {0}");

  emit_warning_if_shadows(for_loop.loop_variable, scope, "loop variable shadows existing symbol");
  auto& body = for_loop.body;
  body.scope.add(Symbol(for_loop.loop_variable, for_loop.type, Symbol::LOCAL));

  auto body_type = analyse_block(body, scope);
  if (!body_type->is_void()) {
    throw_sema_error_at(body.get_final_expr(), "loop body should not evaluate to a value");
  }
}

void Semantics::check_tuple_bindings(
  TupleBinding& bindings, Ast_Expression& init, Type_Ptr& init_type, Scope& scope
) {
  using namespace mpark::patterns;
  auto constraint = infer->generate_tuple_destructure_constraints(
    bindings, init_type, bindings.location);
  // to_infer = to_infer | gen_constraints;
  if (auto tuple_type = std::get_if<TupleType>(&init_type->v)) {
    if (tuple_type->element_types.size() != bindings.binds.size()) {
      throw_sema_error_at(init, "tuple not the right shape for binding");
    }
    uint bind_idx = 0;
    for (auto& el_type: tuple_type->element_types) {
      auto& binding = bindings.binds.at(bind_idx);
      match(binding)(
        pattern(as<Ast_Argument>(arg)) =
          [&](auto& bind) {
            resolve_type_or_fail(scope, bind.type, "undeclared bind type {}");
            assert_valid_binding(
              bind.name,
              bind.name.location,
              bind.type, el_type, &init);
              emit_warning_if_shadows(bind.name, scope, "tuple binding shadows existing symbol");
              scope.add(Symbol(bind.name, bind.type, Symbol::LOCAL));
          },
        pattern(as<TupleBinding>(arg)) =
          [&](auto& nested_binds) {
            check_tuple_bindings(nested_binds, init, el_type, scope);
          }
      );
      bind_idx += 1;
    }
  } else {
    throw_sema_error_at(init, "tuple bind initializer must be a tuple");
  }
  if (constraint) {
    infer->type_constraints.emplace_back(*constraint);
  }
}

void Semantics::analyse_tuple_binding_decl(
  Ast_Tuple_Structural_Binding& binding, Scope& scope
) {
  auto init_type = analyse_expression(*binding.initializer, scope);
  // auto pior_type = init_type.get();
  check_tuple_bindings(binding.bindings, *binding.initializer, init_type, scope);
  // if (pior_type != init_type.get()) {
  //   // Tuple type must have been replaced (to infered type)
  //   binding.initializer->meta.owned_type = init_type;
  // }
}

#define AUTO_LAMBDA "!auto_lambda"

static Ast_Lambda wrap_function_in_lambda(
  Ast_Function_Declaration& top_level_func,
  AstBuilder& builder
) {
  assert(!top_level_func.lambda);
  Ast_Lambda lambda_wrapper;
  // There only needs to be one auto lambda per top level function.
  lambda_wrapper.top_level_wrapper = true;
  // (top level wrappers must have unique names)
  lambda_wrapper.procedure = top_level_func.procedure;
  lambda_wrapper.identifier.name = formatxx::format_string(
    AUTO_LAMBDA "!{}", top_level_func.identifier.name);
  lambda_wrapper.return_type = top_level_func.return_type;
  lambda_wrapper.arguments = top_level_func.arguments;

  std::vector<Expr_Ptr> passthrough_args;
  std::transform(top_level_func.arguments.begin(), top_level_func.arguments.end(),
    std::back_inserter(passthrough_args),
    [&](Ast_Argument& arg) {
      return builder.make_ident(arg.name.name);
    });

  Ast_Call call;
  call.callee = builder.make_ident(top_level_func.identifier.name);
  call.arguments = passthrough_args;
  bool returns_value = !top_level_func.procedure;
  lambda_wrapper.body = builder.make_body(returns_value,
    builder.make_expr_stmt(builder.to_expr_ptr(call), returns_value));

  return lambda_wrapper;
}

void Semantics::expand_macro_expression(Ast_Expression& target, Ast_Call& macro_call, Scope& scope) {
  auto& macro_name = std::get<Ast_Macro_Identifier>(macro_call.callee->v);
  auto* expander = Macros::get_expr_macro_expander(macro_name);
  if (!expander) {
    throw_sema_error_at(macro_call, "expander for macro not found");
  }

  for (auto& arg: macro_call.arguments) {
    (void) analyse_expression(*arg, scope);
  }

  Ast_Expression_Type expansion = (*expander)(macro_call, *builder, *infer);
  AstHelper::rewrite_expr(target, expansion);
}

Type_Ptr Semantics::analyse_expression(Ast_Expression& expr, Scope& scope) {
  using namespace mpark::patterns;
  auto expr_type = match(expr.v)(
    pattern(as<Ast_Literal>(arg)) = [&](auto& lit) {
      return PrimativeType::get(lit.literal_type);
    },
    pattern(as<Ast_Block>(arg)) = [&](auto& block) {
      auto block_type = analyse_block(block, scope);
      if (is_reference_type(block_type)) {
        expr.set_value_type(Expression_Meta::LVALUE);
      }
      return block_type;
    },
    pattern(as<Ast_If_Expr>(arg)) = [&](auto& if_expr){
      auto cond_type = analyse_expression(*if_expr.cond, scope);
      infer->match_or_constrain_types_at(if_expr.cond, cond_type, PrimativeType::bool_ty(),
        "if condition must be a {1}");
      auto then_type = analyse_expression(*if_expr.then_block, scope);
      if (if_expr.has_else) {
        auto else_type = analyse_expression(*if_expr.else_block, scope);
        infer->match_or_constrain_types_at(if_expr, then_type, else_type,
          "type of then block {} does not match else block {}");
        if (is_reference_type(then_type) && is_reference_type(else_type)) {
          expr.set_value_type(Expression_Meta::LVALUE);
        }
      } else if (!then_type->is_void()) {
        auto final_expr = std::get<Ast_Block>(if_expr.then_block->v).get_final_expr();
        throw_sema_error_at(final_expr,
          "if expression cannot evaluate to {} without a matching else",
          type_to_string(then_type.get()));
      }

      return then_type;
    },
    pattern(as<Ast_Identifier>(arg)) = [&](auto& ident) {
      Symbol* symbol = scope.lookup_first_name(ident);
      if (!symbol) {
        throw_sema_error_at(ident, "{} not declared", ident.name);
      }
      if (symbol->kind == Symbol::FUNCTION) {
        // Auto box functions to rvalue lambdas
        auto wrapped_lambda = wrap_function_in_lambda(
          std::get<Ast_Function_Declaration>(symbol->type->v), *builder);
        wrapped_lambda.location = ident.location;
        AstHelper::rewrite_expr(expr, wrapped_lambda);
        expr.set_value_type(Expression_Meta::RVALUE);
        return analyse_expression(expr, scope); // should resolve lambda info
      } else if (symbol->is_local()) {
        expr.set_value_type(Expression_Meta::LVALUE);
        return symbol->type;
      } else {
        throw_sema_error_at(ident, "{} cannot be used in this context", ident.name);
      }
    },
    pattern(as<Ast_Call>(arg)) = [&](auto& call) {
      if (std::holds_alternative<Ast_Macro_Identifier>(call.callee->v)) {
        expand_macro_expression(expr, call, scope);
        // Expansion must then be checked
        return analyse_expression(expr, scope);
      }
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
        if (is_tvar(object_type)) {
          auto field_constraint = TypeFieldConstraint::get(*ctx, access);
          auto field_type = ctx->new_type(TypeVar());
          infer->add_constraint(AstHelper::extract_location(access),
            field_type, field_constraint);
          return field_type;
        } else {
          return get_field_type(object_type, access);
        }
      } else {
        throw_sema_error_at(access.object, "is void?");
      }
    },
    pattern(as<Ast_Array_Literal>(arg)) = [&](auto& array) {
      FixedSizeArrayType array_type;
      array_type.size = array.elements.size();
      if (array_type.size == 1) {
        // Arrays of references not supported
        array_type.element_type = remove_reference(
          analyse_expression(*array.elements.at(0), scope));
      } else if (array_type.size > 1) {
        std::adjacent_find(array.elements.begin(), array.elements.end(),
          [&](auto& prev, auto& next){
            array_type.element_type = remove_reference(analyse_expression(*prev, scope));
            auto next_type = analyse_expression(*next, scope);

            infer->match_or_constrain_types_at(next, next_type, array_type.element_type,
              "element type {} does not match array type of {}");
            return false;
          });
      }
      return ctx->new_type(array_type);
    },
    pattern(as<Ast_Index_Access>(arg)) = [&](auto& index) {
      auto object_type = analyse_expression(*index.object, scope);
      auto index_type = analyse_expression(*index.index, scope);
      if (auto array_type = get_if_dereferenced_type<FixedSizeArrayType>(object_type)) {
        infer->match_or_constrain_types_at(index.index, index_type, PrimativeType::int_ty(),
          "invalid index type {}");
        expr.inherit_value_type(*index.object);
        return array_type->element_type;
      } else {
        throw_sema_error_at(index.object, "not an array type (is {})",
          type_to_string(object_type.get()));
      }
    },
    pattern(as<Ast_Lambda>(arg)) = [&](auto& lambda) {
      lambda.body.scope.set_parent(scope);
      analyse_function_header(lambda);
      LambdaType lambda_type;
      lambda_type.return_type = analyse_function_body(lambda);
      lambda_type.lambda = &lambda;
      std::transform(lambda.arguments.begin(), lambda.arguments.end(),
        std::back_inserter(lambda_type.argument_types),
        [&](auto & arg){ return arg.type; });
      return ctx->new_type(lambda_type);
    },
    pattern(as<Ast_Tuple_Literal>(arg)) = [&](auto& tuple) {
      TupleType tuple_type;
      std::transform(tuple.elements.begin(), tuple.elements.end(),
        std::back_inserter(tuple_type.element_types),
        [&](auto& element){
          auto element_type = remove_reference(analyse_expression(*element, scope));
          // If all elements are lvalues then this tuple is & could be
          // used as a pattern
          expr.inherit_value_type(*element);
          return element_type;
        });
      return ctx->new_type(tuple_type);
    },
    pattern(_) = [&]{
      throw_sema_error_at(expr, "fix me! unknown expression type!");
      return Type_Ptr(nullptr);
    }
  );
  expr.meta.type = expr_type;
  return expr_type;
}

static bool match_special_constraint_at(SourceLocation loc,
  Type_Ptr type, TypeVar::Constraint constraint, Infer& infer
) {
  using namespace mpark::patterns;
  return match(type->v)(
    pattern(as<PrimativeType>(arg)) = [&](auto const & primative) {
      return primative.satisfies(constraint);
    },
    pattern(as<TypeVar>(_)) = [&] {
      if (!infer.type_constraints.empty()) {
        // No point adding these constraints if there's no type vars
        infer.add_constraint(loc, type, TypeVar::get(constraint));
      }
      return true;
    },
    pattern(_) = []{ return false; }
  );
}

Type_Ptr Semantics::analyse_unary_expression(Ast_Unary_Operation& unary, Scope& scope) {
  auto operand_type = remove_reference(analyse_expression(*unary.operand, scope));

  if (unary.operation == Ast_Operator::REF) {
    if (!operand_type || !unary.operand->is_lvalue()) {
      throw_sema_error_at(unary.operand, "cannot take reference to non-lvalue expression");
    }
    unary.get_meta().value_type = Expression_Meta::LVALUE;
    return builder->make_reference(operand_type);
  }

  auto loc = AstHelper::extract_location(unary);
  switch (unary.operation) {
    case Ast_Operator::MINUS:
    case Ast_Operator::PLUS: {
      if (match_special_constraint_at(loc, operand_type, TypeVar::NUMERIC, *infer)) {
        return operand_type;
      }
      break;
    }
    case Ast_Operator::BITWISE_NOT: {
      if (match_special_constraint_at(loc, operand_type, TypeVar::INTEGER, *infer)) {
        return operand_type;
      }
      break;
    }
    case Ast_Operator::LOGICAL_NOT: {
      if (infer->match_or_constrain_types_at(unary, operand_type, PrimativeType::bool_ty(),
          "cannot perform logical not on {}")) {
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
  auto left_type = remove_reference(analyse_expression(*binop.left, scope));
  auto right_type = analyse_expression(*binop.right, scope);

  infer->match_or_constrain_types_at(binop, left_type, right_type,
    "incompatible types {} and {}");

  left_type = min_type(left_type, right_type);
  auto loc = AstHelper::extract_location(binop);
  auto binop_type = match(binop.operation)(
    pattern(anyof(
      Ast_Operator::PLUS, Ast_Operator::MINUS, Ast_Operator::TIMES,
      Ast_Operator::DIVIDE
    )) = [&]{
      WHEN(match_special_constraint_at(loc, left_type, TypeVar::NUMERIC, *infer)) {
        return left_type;
      };
    },
    pattern(anyof(
      Ast_Operator::LEFT_SHIFT, Ast_Operator::RIGHT_SHIFT, Ast_Operator::BITWISE_AND,
      Ast_Operator::BITWISE_OR, Ast_Operator::BITWISE_XOR, Ast_Operator::MODULO
    )) = [&]{
      WHEN(match_special_constraint_at(loc, left_type, TypeVar::INTEGER, *infer)) {
        return left_type;
      };
    },
    pattern(anyof(
      Ast_Operator::LESS_THAN, Ast_Operator::LESS_EQUAL, Ast_Operator::GREATER_THAN,
      Ast_Operator::GREATER_EQUAL, Ast_Operator::EQUAL_TO, Ast_Operator::NOT_EQUAL_TO
    )) = [&]{
      WHEN(match_special_constraint_at(loc, left_type, TypeVar::NUMERIC, *infer)) {
        return PrimativeType::bool_ty();
      };
    },
    pattern(_) = [&]{
      throw_sema_error_at(binop, "invalid operation for {}",
        type_to_string(left_type.get()));
      return Type_Ptr(nullptr);
    }
  );
  return binop_type;
}

#define NOT_CALLABLE "{} is not callable"

Type_Ptr Semantics::analyse_call(Ast_Call& call, Scope& scope) {
  using namespace mpark::patterns;
  Type_Ptr callee_type;

  auto called_function_ident = std::get_if<Ast_Identifier>(&call.callee->v);
  if (called_function_ident) {
    Symbol* called_function = scope.lookup_first_name(*called_function_ident);
    if (!called_function) {
      throw_sema_error_at(*called_function_ident, "attempting to call undefined function \"{}\"",
        called_function_ident->name);
    }
    callee_type = called_function->type;
  } else {
    callee_type = analyse_expression(*call.callee, scope);
  }

  auto deref_callee_type = remove_reference(callee_type);
  infer->generate_call_constraints(deref_callee_type, call);

  // Fairly hackly updated to support lambdas (will need a rework)
  return match(deref_callee_type->v)(
    pattern(anyof(as<Ast_Function_Declaration>(arg), as<LambdaType>(arg))) =
    [&](auto& function_type) {
      using TFunc = std::decay_t<decltype(function_type)>;
      constexpr bool is_lambda = std::is_same_v<TFunc, LambdaType>;

      size_t expected_arg_count;
      std::string function_name;
      if constexpr (is_lambda) {
        expected_arg_count = function_type.argument_types.size();
        function_name = "\\lambda";
      } else {
        expected_arg_count = function_type.arguments.size();
        function_name = function_type.identifier.name;
      }

      if (expected_arg_count != call.arguments.size()) {
        throw_sema_error_at(call.callee, "{} expects {} arguments not {}",
          function_name, expected_arg_count, call.arguments.size());
      }

      for (uint arg_idx = 0; arg_idx < expected_arg_count; arg_idx++) {
        auto& argument = *call.arguments.at(arg_idx);
        (void) analyse_expression(argument, scope);

        if constexpr (!is_lambda) {
          auto& expected = function_type.arguments.at(arg_idx);
          assert_valid_binding(expected.name, expected.type, &argument);
        } else {
          // Lambda args are not named (will need a better error reporting method...)
          auto& expected = function_type.argument_types.at(arg_idx);
          bool tvar_param = is_tvar(expected);

          Infer::ConstraintOrigin infer_note;
          if (tvar_param && function_type.lambda) {
            auto arg_loc = function_type.lambda->arguments.at(arg_idx).name.location;
            // FIXME: is_empty() check as I've yet to fix this for macros
            if (!arg_loc.is_empty()) {
              infer_note = arg_loc;
            }
          }

          // FIXME: This lvalue is wrong (it points at the rvalue -- but it's something)
          Ast_Identifier lvalue;
          lvalue.location = AstHelper::extract_location(argument);

          assert_valid_binding(
            called_function_ident ? *called_function_ident : lvalue,
            lvalue.location,
            expected, argument.meta.type,
            &argument, infer_note);
        }
      }

      call.callee->meta.type = callee_type;

      if (is_reference_type(function_type.return_type)) {
        call.get_meta().value_type = Expression_Meta::LVALUE;
      }

      // FINALLY we've checked everything in the call!
      return function_type.return_type;
    },
    pattern(_) = [&]() -> Type_Ptr {
      throw_sema_error_at(call.callee, NOT_CALLABLE,
        type_to_string(callee_type.get()));
      return nullptr;
    }
  );
}
