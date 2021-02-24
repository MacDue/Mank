#include <utility>
#include <unordered_set>

#include <mpark/patterns.hpp>

#include "ast/expr_helpers.h"
#include "errors/compiler_errors.h"

#include "sema/types.h"
#include "sema/semantics.h"
#include "sema/const_propagate.h"
#include "sema/builtin_functions.h"
#include "sema/return_reachability.h"

// Macros
#include "macros/bind_macro.h"
#include "macros/curry_macro.h"
#include "macros/print_macro.h"
#include "macros/assert_macro.h"
#include "macros/vector_literal_macro.h"

Semantics::Semantics() {
  static bool macros_loaded = false;
  if (!macros_loaded) {
    Macros::register_macro("pbind", Macros::builtin_bind);
    Macros::register_macro("curry", Macros::builtin_curry);
    Macros::register_macro("print", Macros::builtin_print);
    Macros::register_macro("println", Macros::builtin_print);
    Macros::register_macro("eprint", Macros::builtin_print);
    Macros::register_macro("eprintln", Macros::builtin_print);
    Macros::register_macro("assert", Macros::builtin_assert);
    Macros::register_macro("vec", Macros::builtin_vec_literal);
    macros_loaded = true;
  }
}

Symbol* Semantics::emit_warning_if_shadows(
  Ast_Identifier& ident, Scope& scope, std::string warning
) {
  if (ident.name.at(0) == '^') {
    // HACK: Don't warn for things we add in macros & such by prefixing with '^'.
    return nullptr;
  }
  Symbol* pior_symbol = scope.lookup_first_name(ident);
  if (pior_symbol) {
    emit_warning_at(ident, warning);
  }
  return pior_symbol;
}

template<typename T>
Ast_Identifier* get_symbol_identifer_if_type(Symbol* symbol) {
  if (auto type = std::get_if<T>(&symbol->type->v)) {
    return type->identifier.get_raw_self();
  }
  return nullptr;
}

#define SKIP_TESTS_IF_NOT_TESTING()      \
  if (!building_tests() && func->test) { \
    continue;                            \
  }

#define MAIN_FUNCTION_IDENT "main"

void Semantics::analyse_file(Ast_File& file) {
  using namespace mpark::patterns;

  // Setup the context
  this->ctx = &file.ctx;
  this->builder.emplace(AstBuilder(file));
  this->infer.emplace(Infer(*this->ctx,
    [&](CompilerMessage msg){ warnings.emplace_back(msg); }));

  auto& global_scope = file.scope;

  // Add builtin types
  static std::array primative_types {
    std::make_pair("f32", PrimativeType::f32_ty()),
    std::make_pair("f64", PrimativeType::f64_ty()),
    std::make_pair("i32", PrimativeType::int_ty()),
    std::make_pair("str", PrimativeType::str_ty()),
    std::make_pair("bool", PrimativeType::bool_ty()),
    std::make_pair("char", PrimativeType::char_ty())
  };

  for (auto [type_name, type] : primative_types) {
    global_scope.add(
      Symbol(SymbolName(type_name), type, Symbol::TYPE));
  }

  // Add builtin function declartions
  Builtin::add_builtins_to_scope(global_scope, *ctx, *builder);

  auto check_top_level_decl = [&](
    char const * decl_name, Symbol::Kind sym_kind, Ast_Identifier& ident,
    auto get_previous_decl
  ) {
    if (auto symbol = global_scope.lookup_first_name(ident)) {
      Ast_Identifier* pior_ident = nullptr;
      if (symbol->kind == sym_kind && (pior_ident = get_previous_decl(symbol))) {
        if (auto func_decl = std::get_if<Ast_Function_Declaration>(&symbol->type->v)) {
          if (func_decl->external) {
            return;
          }
        }
        // If there's not a pior ident then it must be something else...
        throw_error_at(ident, "redeclaration of {} (previously on line {})",
          decl_name, pior_ident->location.start_line + 1);
      } else {
        // FIXME: a symbol of a different kind that shadows still overwrites the other
        emit_warning_at(ident, "{} declaration shadows existing symbol", decl_name);
      }
    }
  };

  // FIXME?: Shadowing does not follow declaration order (since)
  // globals -> pods -> functions are processed seperately

  /* Check global constants */
  {
    for (auto global_const: file.global_consts) {
      check_top_level_decl("global constant", Symbol::GLOBAL, global_const->constant,
        [](Symbol* sym){ return sym->name.get_raw_self(); });
      auto& sym = file.scope.add(
        Symbol(global_const->constant, global_const->type, Symbol::GLOBAL));
      sym.const_value = global_const->const_expression;
    }
    for (auto global_const: file.global_consts) {
      auto sym = file.scope.lookup_first_name(global_const->constant);
      if (!sym->const_value->meta.is_const()) {
        analyse_constant_decl(*global_const, file.scope);
      }
    }
    infer->unify_and_apply(); // allow inference between constants
  }

  /* Items/declarations */
  {
    for (auto item: file.items) {
      // Add (unresolved types into the global scope -- allows cross referencing)
      match(item->v)(
        pattern(as<Ast_Pod_Declaration>(arg)) = [&](auto& pod_decl){
          check_top_level_decl("pod", Symbol::TYPE, pod_decl.identifier,
            get_symbol_identifer_if_type<PodType>);
          ctx->new_identified_type<PodType>(pod_decl);
          global_scope.add(Symbol(pod_decl.identifier, item->declared_type, Symbol::TYPE));
        },
        pattern(as<Ast_Enum_Declaration>(arg)) = [&](auto& enum_decl){
          check_top_level_decl("enum", Symbol::TYPE, enum_decl.identifier,
            get_symbol_identifer_if_type<EnumType>);
          ctx->new_identified_type<EnumType>(enum_decl);
          global_scope.add(
            Symbol(enum_decl.identifier, item->declared_type, Symbol::TYPE));
        }
      );
    }

    for (auto item: file.items) {
      match(item->v)(
        pattern(as<Ast_Pod_Declaration>(arg)) = [&](auto& pod_decl){
          analyse_pod(pod_decl, global_scope);
        },
        pattern(as<Ast_Enum_Declaration>(arg)) = [&](auto& enum_decl){
          analyse_enum(enum_decl, global_scope);
        }
      );
    }
  }

  /* Process tests */
  {
    if (building_tests()) {
      Builtin::add_test_runner_main(file);
    } else {
      // Remove tests if not building with --tests
      std::erase_if(file.functions, [](auto func){ return func->test; });
    }
  }

  /* Functions/procedures */
  {
    /* Add function headers into scope, resolve function return/param types */
    for (auto func: file.functions) {
      check_top_level_decl("function", Symbol::FUNCTION, func->identifier,
        get_symbol_identifer_if_type<Ast_Function_Declaration>);
      global_scope.add(Symbol(func->identifier, func.class_ptr(), Symbol::FUNCTION));
      func->body.scope.set_parent(global_scope);
      analyse_function_header(*func);
    }

    /* Check functions */
    for (auto func: file.functions) {
      analyse_function_body(*func);
    }
  }
}

Type_Ptr Semantics::check_constant_initializer(
  Ast_Identifier constant, Ast_Expression& init, Scope& scope
) {
  if (global_eval.contains(constant)) {
    throw_error_at(init, "recursive constant initializer");
  }
  global_eval.insert(constant);
  auto init_type = analyse_expression(init, scope);
  AstHelper::constant_expr_eval(init);
  if (!init.meta.is_const()) {
    throw_error_at(init, "initializer not constant");
  }
  global_eval.erase(constant);
  return init_type;
}

void Semantics::analyse_constant_decl(Ast_Constant_Declaration& const_decl, Scope& scope) {
  resolve_type_or_fail(scope, const_decl.type, "undeclared constant type {}");
  auto init_type = check_constant_initializer(
    const_decl.constant, *const_decl.const_expression, scope);
  infer->match_or_constrain_types_at(const_decl.constant, const_decl.type, init_type,
    "constant type {} does not match initializer of {}");
}

static int pod_is_recursive(PodType& pod, PodType& nested_field) {
  /*
    0 if not recursive, 1 if directly recursive, indirection steps otherwise
    Simply counts how many steps into the structure we go till the top level pod is reached.
  */
  if (&pod == &nested_field) {
    return 1;
  } else {
    for (auto& [_,  field]: nested_field.fields) {
      if (auto pod_field = std::get_if<PodType>(&field.type->v)) {
        if (auto steps = pod_is_recursive(pod, *pod_field)) {
          return 1 + steps;
        }
      }
    }
  }
  return 0;
}

static void resolve_pod_field_types(
  PodType& pod_type, std::vector<Ast_Argument>& fields, Scope& scope
) {
  for (size_t field_index = 0; field_index < fields.size(); ++field_index) {
    auto& field = fields.at(field_index);
    if (pod_type.has_field(field.name)) {
      throw_error_at(field.name, "duplicate pod field");
    }
    resolve_type_or_fail(scope, field.type, "undeclared field type {}");
    pod_type.add_field(field.name, field.type, field_index);
  }
}

void Semantics::analyse_pod(Ast_Pod_Declaration& pod_decl, Scope& scope) {
  auto& pod_type = std::get<PodType>(pod_decl.get_self().class_ptr()->declared_type->v);
  resolve_pod_field_types(pod_type, pod_decl.fields, scope);
  for (auto& field: pod_decl.fields) {
    if (auto pod_field = std::get_if<PodType>(&field.type->v)) {
      if (auto steps = pod_is_recursive(pod_type, *pod_field)) {
        if (steps == 1) {
          throw_error_at(field.name, "directly recursive pods are not allowed");
        } else {
          throw_error_at(field.name, "recursive cycle detected in pod declaration");
        }
        field.type = nullptr; // cycles bad
      }
    }
  }
}

void Semantics::analyse_enum(Ast_Enum_Declaration& enum_decl, Scope& scope) {
  using namespace mpark::patterns;
  auto& enum_type = std::get<EnumType>(enum_decl.get_self().class_ptr()->declared_type->v);
  for (size_t enum_ordinal = 0; enum_ordinal < enum_decl.members.size(); ++enum_ordinal) {
    auto& member = enum_decl.members.at(enum_ordinal);
    if (enum_type.has_member(member.tag)) {
      throw_error_at(member.tag, "duplicate enum member");
    }
    Type_Ptr enum_data;
    if (member.data) {
      enum_data = match(*member.data)(
        pattern(as<Ast_Enum_Declaration::Member::TupleData>(arg)) =
          [&](auto& tuple_data) {
            for (auto& el_type: tuple_data.elements) {
              resolve_type_or_fail(scope, el_type, "undeclared element type {}");
            }
            TupleType enum_tuple;
            enum_tuple.element_types = tuple_data.elements;
            return ctx->new_type(enum_tuple);
          },
        pattern(as<Ast_Enum_Declaration::Member::PodData>(arg)) =
          [&](auto& pod_data) {
            PodType enum_pod;
            resolve_pod_field_types(enum_pod, pod_data.fields, scope);
            return ctx->new_type(enum_pod);
          }
      );
    }
    enum_type.add_member(member.tag, enum_ordinal, enum_data);
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
  // Special case main function
  if (func.identifier.name == MAIN_FUNCTION_IDENT) {
    ListType _args_type;
    _args_type.element_type = PrimativeType::str_ty();
    auto args_type = ctx->new_type(_args_type);

    auto args_count = func.arguments.size();
    if (args_count == 0 && !func.test) {
      // Add args parameter (for compatibility with C boostrap)
      func.arguments.push_back(builder->make_argument(args_type, "args"));
    } else if (args_count != 1 || !match_types(func.arguments.at(0).type, args_type)) {
      throw_error_at(func.identifier, "invalid main function declaration");
    }
  }
}

Type_Ptr Semantics::analyse_block(Ast_Block& block, Scope& scope) {
  block.scope.set_parent(scope);
  for (auto stmt: block.statements) {
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
    assert_valid_binding({}, expected_return, final_expr);
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
      assert_valid_binding({}, expected_return, void_return.expression);
      func.body.statements.emplace_back(ctx->new_stmt(void_return));
      func.body.has_final_expr = false;
      func.return_type = Type::void_ty(); // quick fix
    } else if (!func.return_type->is_void()) {
      throw_error_at(func.identifier, "function possibly fails to return a value");
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
      throw_error_at(func.identifier, "type inference failed ({})", e.what());
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
    pattern(as<Ast_Switch_Expr>(_)) = []{
      return true; // TODO/FIXME: properly check switch side effects
    },
    pattern(_) = []() { return false; }
  );
}

void Semantics::analyse_expression_statement(Ast_Expression_Statement& expr_stmt, Scope& scope) {
  auto expression_type = analyse_expression(*expr_stmt.expression, scope);
  bool void_expr = expression_type->is_void();
  if (!expr_stmt.final_expr) {
    auto call = std::get_if<Ast_Call>(&expr_stmt.expression->v);
    if (!expression_may_have_side_effects(*expr_stmt.expression)) {
      emit_warning_at(expr_stmt.expression, "statement has no effect");
    } else if (call == nullptr && !void_expr) {
      emit_warning_at(expr_stmt.expression, "result of expression discarded");
    } else if (!void_expr) {
      if (auto func_decl = std::get_if<Ast_Function_Declaration>(&call->callee->meta.type->v)) {
        if (func_decl->c_function) {
          // builtin c lib function -- allow ignore return (as returns are semi-pointless errors)
          // e.g. putchar returns an int.
          return;
        }
      }
      throw_error_at(expr_stmt.expression, "return value discarded");
    }
  }
}

void Semantics::analyse_assignment(Ast_Assign& assign, Scope& scope) {
  // Note that when a tuple is used as a lhs the actual tuple type
  // is not used it's justed used as a (nested) list of lvalue idents.
  // (the types of the contained lvaues is what matters)
  auto target_type = analyse_expression(*assign.target, scope);
  infer->assert_lvalue(assign.target, "target is not an lvalue");

  auto expr_type = analyse_expression(*assign.expression, scope);

  infer->match_or_constrain_types_at(assign, target_type, expr_type,
    "cannot assign variable of type {} to {}");
}

static bool is_explict_reference(Ast_Expression& expr) {
  // It is if the top level expression is a ref unary
  auto unary = std::get_if<Ast_Unary_Operation>(&expr.v);
  return unary && unary->operation == Ast_Operator::REF;
}

#define ANALYSE_LOOP_BODY(body) { \
  auto body_type = analyse_block(body, scope);     \
  if (!body_type->is_void()) {                     \
    throw_error_at(body.get_final_expr(),     \
      "loop body should not evaluate to a value"); \
  }                                                \
}

#define LOOP_ANALYSIS(loop,code) { enter_loop((&loop).class_ptr()); code; exit_loop(); }

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
      LOOP_ANALYSIS(for_loop, analyse_for_loop(for_loop, scope));
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
        throw_error_at(return_stmt, "a function needs to return a value");
      }
      assert_valid_binding({} /* won't be used */, expected_return, return_stmt.expression);
    },
    pattern(as<Ast_Variable_Declaration>(arg)) = [&](auto& var_decl) {
      if (var_decl.type) {
        resolve_type_or_fail(scope, var_decl.type, "undeclared type {}");
      } else if (!var_decl.initializer) {
        throw_error_at(var_decl, "cannot infer type of {} without initializer",
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
        throw_error_at(var_decl, "uninitialized variable declaration");
      }
      assert_valid_binding(var_decl.variable, var_decl.type, var_decl.initializer);
      emit_warning_if_shadows(var_decl.variable, scope, "declaration shadows existing symbol");
      scope.add(
        Symbol(var_decl.variable, var_decl.type, Symbol::LOCAL));
    },
    pattern(as<Ast_Structural_Binding>(arg)) = [&](auto& binding) {
      auto init_type = analyse_expression(*binding.initializer, scope);
      check_bindings(binding.bindings, binding.initializer, init_type, scope);
    },
    pattern(as<Ast_Loop>(arg)) = [&](auto& loop) {
      LOOP_ANALYSIS(loop, ANALYSE_LOOP_BODY(loop.body));
    },
    pattern(as<Ast_While_Loop>(arg)) = [&](auto& while_loop) {
      LOOP_ANALYSIS(while_loop, ({
        auto cond_type = analyse_expression(*while_loop.cond, scope);
        infer->match_or_constrain_types_at(while_loop.cond, cond_type, PrimativeType::bool_ty(),
          "while loop condition must be {1} (is {0})");
        ANALYSE_LOOP_BODY(while_loop.body);
      }));
    },
    pattern(as<Ast_Loop_Control>(arg)) = [&](auto& loop_control){
      SpAstPtr<Ast_Statement, Ast_Loop> ast_loop; // loop {} (not any loop)
      auto loop = in_loop();
      if (!loop) {
        throw_error_at(loop_control, "must be within a loop!");
      } else if ((ast_loop = loop)) {
        if (loop_control.type == Ast_Loop_Control::BREAK) {
          ast_loop->may_break = true;
        }
      }
    }
  );
}

void Semantics::analyse_for_loop(Ast_For_Loop& for_loop, Scope& scope) {
  auto start_range_type = analyse_expression(*for_loop.start_range, scope);
  if (for_loop.type) {
    resolve_type_or_fail(scope, for_loop.type, "undeclared loop type {}");
    if (is_reference_type(for_loop.type)) {
      throw_error_at(for_loop.loop_variable, "reference loop variables are not yet supported");
    }
    infer->match_or_constrain_types_at(for_loop.start_range, for_loop.type, start_range_type,
      "start range type type {1} does not match loop variable type {0}");
  } else {
    if (start_range_type->is_void()) {
      throw_error_at(for_loop.start_range, "loop variable cannot be type {}",
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

  ANALYSE_LOOP_BODY(body);
}

void Semantics::check_tuple_bindings(
  Ast_Tuple_Binds& bindings, Expr_Ptr init, Type_Ptr init_type, Scope& scope
) {
  using namespace mpark::patterns;
  /*
    TODO: Consider

    If the outer tuple is a rvalue, but inside is a reference, should we allow
    the inner reference to be extracted into bind reference?

    Currently my codegen (and sema) can't figure this out (not fine grained enough)

    e.g.

    pod Test {
      msg: ref str
    }

    # test is a Test
    bind ({msg: ref}) = (test,) # currently unsupported
  */
  init->fix_tuple_hack(); // Makes sure the bindings are correct
  auto constraint = infer->generate_tuple_destructure_constraints(
    bindings, init_type, bindings.location);
  if (auto tuple_type = std::get_if<TupleType>(&init_type->v)) {
    if (tuple_type->element_types.size() != bindings.binds.size()) {
      throw_error_at(init, "tuple not the right shape for binding");
    }
    uint bind_idx = 0;
    for (auto el_type: tuple_type->element_types) {
      auto& binding = bindings.binds.at(bind_idx);
      match(binding)(
        pattern(as<Ast_Bind>(arg)) = [&](auto& bind) {
          resolve_type_or_fail(scope, bind.type, "undeclared tuple bind type {}");
          assert_valid_binding(
            bind.name,
            bind.name.location,
            bind.type, el_type, init);
            emit_warning_if_shadows(bind.name, scope, "tuple binding shadows existing symbol");
            scope.add(Symbol(bind.name, bind.type, Symbol::LOCAL));
        },
        pattern(as<Ast_Tuple_Binds>(arg)) = [&](auto& nested_binds) {
          check_tuple_bindings(nested_binds, init, el_type, scope);
        },
        pattern(as<Ast_Pod_Binds>(arg)) = [&](auto& nested_binds) {
          check_pod_bindings(nested_binds, init, el_type, scope);
        }
      );
      bind_idx += 1;
    }
  } else {
    throw_error_at(init, "tuple bind initializer must be a tuple");
  }
  if (constraint) {
    infer->type_constraints.emplace_back(*constraint);
  }
}

void Semantics::check_pod_bindings(
  Ast_Pod_Binds& bindings, Expr_Ptr init, Type_Ptr init_type, Scope& scope
) {
  using namespace mpark::patterns;
  bindings.pod_type = init_type; // help for codegen
  for (auto& field_bind: bindings.binds) {
    Type_Ptr field_type;
    if (is_tvar(init_type)) {
      auto field_constraint = TypeFieldConstraint::get(
        *ctx, init, field_bind.field, field_bind.field_index);
      field_type = ctx->new_tvar();
      infer->add_constraint(field_bind.field.location, field_type, field_constraint);
    } else {
      field_type = get_field_type(field_bind, init, init_type);
    }
    match(field_bind.replacement)(
      pattern(as<Ast_Bind>(arg)) = [&](auto& bind){
        resolve_type_or_fail(scope, bind.type, "undeclared pod bind type {}");
        assert_valid_binding(
          field_bind.field,
          field_bind.field.location,
          bind.type, field_type, init);
        field_bind.bound_name = (!bind.name.empty() ? bind.name : field_bind.field).get_raw_self();
        emit_warning_if_shadows(*field_bind.bound_name, scope, "pod binding shadows existing symbol");
        scope.add(Symbol(*field_bind.bound_name, bind.type, Symbol::LOCAL));
      },
      pattern(as<Ast_Pod_Binds>(arg)) = [&](auto& nested_binds) {
        check_pod_bindings(nested_binds, init, field_type, scope);
      },
      pattern(as<Ast_Tuple_Binds>(arg)) = [&](auto& nested_binds) {
        check_tuple_bindings(nested_binds, init, field_type, scope);
      }
    );
  }
}

void Semantics::check_bindings(
  Ast_Binding& bindings, Expr_Ptr init, Type_Ptr init_type, Scope& scope
) {
  using namespace mpark::patterns;
  match(bindings)(
    pattern(as<Ast_Tuple_Binds>(arg)) = [&](auto& tuple_binds){
      check_tuple_bindings(tuple_binds, init, init_type, scope);
    },
    pattern(as<Ast_Pod_Binds>(arg)) = [&](auto& pod_binds){
      check_pod_bindings(pod_binds, init, init_type, scope);
    }
  );
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
    throw_error_at(macro_call, "expander for macro not found");
  }

  for (auto arg: macro_call.arguments) {
    (void) analyse_expression(*arg, scope, true);
  }

  Ast_Expression_Type expansion = (*expander)(macro_call, *builder, *infer, source_file);
  AstHelper::rewrite_expr(target, expansion);
}

Type_Ptr Semantics::analyse_expression(Ast_Expression& expr, Scope& scope, bool within_macro) {
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
        "if condition must be a {1} (is {0})");
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
        throw_error_at(final_expr,
          "if expression cannot evaluate to {} without a matching else",
          type_to_string(then_type.get()));
      }

      return then_type;
    },
    pattern(as<Ast_Identifier>(arg)) = [&](auto& ident) {
      Symbol* symbol = scope.lookup_first_name(ident);
      if (!symbol) {
        throw_error_at(ident, "{} not declared", ident.name);
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
      } else if (symbol->is_global()) {
        auto& global_meta = symbol->const_value->meta;
        if (!global_meta.is_const()) {
          check_constant_initializer(ident, *symbol->const_value, scope);
        }
        ident.update_const_value(*symbol->const_value->meta.get_const_value());
        return symbol->type; // Globals are constant.
      } else {
        throw_error_at(ident, "{} cannot be used in this context", ident.name);
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
          auto field_type = ctx->new_tvar();
          infer->add_constraint(AstHelper::extract_location(access),
            field_type, field_constraint);
          return field_type;
        } else {
          return get_field_type(access);
        }
      } else {
        throw_error_at(access.object, "is void?");
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

      infer->match_or_constrain_types_at(index.index, index_type, PrimativeType::int_ty(),
        "invalid index type {}");

      if (is_tvar(object_type)) {
        auto index_constraint = TypeIndexConstraint::get(*ctx, index);
        auto element_type = ctx->new_tvar();
        infer->add_constraint(AstHelper::extract_location(index),
          element_type, index_constraint);
        return element_type;
      } else {
        return get_element_type(object_type, index);
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
        [&](auto & arg){
          // Just makes sure there's always a constraint for a param even if unused.
          infer->match_or_constrain_types_at(arg.name.location, arg.type, arg.type, "unconstrained!");
          return arg.type;
        });
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
    pattern(as<Ast_Pod_Literal>(arg)) = [&](auto& pod_init) {
      auto type = scope.resolve_type_from_path(pod_init.pod);
      std::unordered_set<std::string_view> seen_fields;
      auto& pod_type = match(type->v)(
        pattern(as<PodType>(arg)) = [](auto& pod_type) -> PodType& { return pod_type; },
        pattern(as<EnumType>(arg)) = [&](auto& enum_type) -> PodType& {
          auto& member = enum_type.get_member(pod_init.pod);
          if (!member.data || !std::holds_alternative<PodType>(member.data->v)) {
            throw_error_at(pod_init.pod, "not a pod enum member");
          }
          return std::get<PodType>(member.data->v);
        });
      for (auto& init: pod_init.fields) {
        if (seen_fields.contains(init.field.name)) {
          throw_error_at(init.field, "repeated field in initializer");
        }
        auto field_info = pod_type.get_field(init.field);
        auto init_type = analyse_expression(*init.initializer, scope);
        assert_valid_binding({}, AstHelper::extract_location(init.initializer),
           field_info.type, init_type, init.initializer);

        init.field_index = field_info.index;
        seen_fields.insert(init.field.name);
      }
      if (seen_fields.size() != pod_type.fields.size()) {
        throw_error_at(pod_init, "incomplete pod initialization");
      }
      return type;
    },
    pattern(as<Ast_As_Cast>(arg)) = [&](auto& as_cast){
      return analyse_as_cast(as_cast, scope);
    },
    pattern(as<Ast_Array_Repeat>(arg)) = [&](auto& array_repeat){
      auto init_type = analyse_expression(*array_repeat.initializer, scope);
      auto repetitions_type = analyse_expression(*array_repeat.repetitions, scope);
      // FIXME: Const eval will happen twice (currently const eval needs types)
      AstHelper::constant_expr_eval(*array_repeat.repetitions);
      PrimativeValue* const_repetitions;
      if ((const_repetitions = array_repeat.repetitions->meta.get_const_value())
        || within_macro
      ) {
        FixedSizeArrayType array_type;
        infer->match_or_constrain_types_at(
          array_repeat.repetitions, repetitions_type, PrimativeType::int_ty(),
          "repetitions must be an integer amount");
        // Don't restrict repetitions to constants in macros (good for vec inits)
        if (!within_macro) {
          auto repetitions = std::get<int32_t>(*const_repetitions);
          if (repetitions < 0) {
            throw_error_at(array_repeat.repetitions, "cannot have negative repetitions");
          }
          array_type.size = repetitions;
        }
        array_type.element_type = init_type;
        return ctx->new_type(array_type);
      } else {
        throw_error_at(array_repeat.repetitions, "repetitions must be constant");
      }
    },
    pattern(as<Ast_Spawn>(arg)) = [&](auto& spawn){
      auto init_type = analyse_expression(*spawn.initializer, scope);
      CellType cell_type;
      cell_type.contains = remove_reference(init_type);
      cell_type.ref = builder->make_reference(cell_type.contains);
      expr.set_value_type(Expression_Meta::LVALUE);
      return ctx->new_type(cell_type);
    },
    pattern(as<Ast_Path>(arg)) = [&](auto& path){
      return match(scope.resolve_path(path))(
        pattern(as<Type_Ptr>(arg)) = [&](auto ty){
          if (!ty) {
            throw_error_at(path, "invalid path");
          }
          if (auto enum_type = std::get_if<EnumType>(&ty->v)) {
            auto& member = enum_type->get_member(path);
            if (member.data) {
              throw_error_at(path, "enum member missing initialization");
            }
          }
          return ty;
        },
        pattern(as<Expr_Ptr>(arg)) = [](auto constant) { return constant->meta.type; }
      );
    },
    pattern(as<Ast_Switch_Expr>(arg)) = [&](auto& switch_expr) {
      return analyse_switch_expr(switch_expr, scope);
    },
    pattern(anyof(as<Ast_Macro_Identifier>(_), as<Ast_Specialized_Identifier>(_))) = [&]{
      throw_error_at(expr, "this is not valid here!");
      return Type_Ptr(nullptr);
    },
    pattern(_) = [&]{
      throw_error_at(expr, "fix me! unknown expression type!");
      return Type_Ptr(nullptr);
    }
  );
  expr.meta.type = expr_type;
  return expr_type;
}

Type_Ptr Semantics::analyse_as_cast(Ast_As_Cast& as_cast, Scope& scope) {
  resolve_type_or_fail(scope, as_cast.type, "undeclared cast target {}");
  auto source_type = analyse_expression(*as_cast.object, scope);
  if (is_tvar(source_type)) {
    auto cast_constraint = TypeCastConstraint::get(*ctx, as_cast);
    infer->add_constraint(AstHelper::extract_location(as_cast),
      source_type, cast_constraint);
  } else {
    validate_type_cast(source_type, as_cast);
  }
  return as_cast.type;
}

Type_Ptr Semantics::analyse_switch_expr(Ast_Switch_Expr& switch_expr, Scope& scope) {
  // TODO: returing values in bodys (covering all cases), duplicate checking.
  using namespace mpark::patterns;
  auto switched_type = analyse_expression(*switch_expr.switched, scope);
  if (is_tvar(switched_type)) {
    auto switch_constraint = SwitchableConstraint::get(*ctx, switch_expr.switched);
    infer->add_constraint(AstHelper::extract_location(switch_expr.switched),
      switched_type, switch_constraint);
  } else {
    assert_has_switchable_type(switch_expr.switched);
  }
  Type_Ptr switch_return = Type::void_ty(); // Just void for now.
  for (auto& switch_case: switch_expr.cases) {
    // auto match_type = analyse_expression(switch_case.match, scope);
    auto match_type = match(switch_case.match->v)(
      pattern(as<Ast_Path>(arg)) = [&](auto& enum_path){
        auto [enum_type, member_info] = AstHelper::path_as_enum_member(enum_path, scope);
        if (switch_case.bindings) {
          if (!member_info->data) {
            throw_error_at(switch_case.match, "enum member does not have data to bind");
          }
          check_bindings(*switch_case.bindings, switch_case.match, member_info->data, scope);
        }
        return enum_type->get_self().class_ptr();
      },
      pattern(_) = [&]{
        if (switch_case.bindings) {
          throw_error_at(switch_case.match, "type does not support bindings");
        }
        auto match_type = analyse_expression(*switch_case.match, scope);
        AstHelper::constant_expr_eval(*switch_case.match);
        if (!switch_case.match->meta.is_const()) {
          throw_error_at(switch_case.match, "not a constant expression");
        }
        return match_type;
      }
    );
    // Case type
    infer->match_or_constrain_types_at(switch_case.match,
      match_type, switched_type, "case type {} does not match switched type {}");

    // Case body type
    auto body_type = analyse_block(switch_case.body, scope);
    infer->match_or_constrain_types_at(switch_case.body,
      body_type, switch_return, "[TODO] currently all cases must return void");
  }

  return switch_return;
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
    infer->assert_lvalue(unary.operand, "cannot take reference to non-lvalue expression");
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

  throw_error_at(unary, "invalid unary operation for {}",
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
      WHEN(match_special_constraint_at(loc, left_type,
        binop.operation == Ast_Operator::PLUS
          ? TypeVar::ADDABLE
          : TypeVar::NUMERIC, *infer)
      ) {
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
    pattern(anyof(
      Ast_Operator::LOGICAL_AND, Ast_Operator::LOGICAL_OR
    )) = [&]{
      WHEN(infer->match_or_constrain_types_at(binop, left_type, PrimativeType::bool_ty(),
        "both operands must be {1} (are {0}}")
      ) {
        return left_type;
      };
    },
    pattern(_) = [&]{
      throw_error_at(binop, "invalid operation for {}",
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

  Ast_Identifier* called_function_ident = nullptr;

  auto look_up_function = [&](auto& ident) {
    Symbol* called_function = scope.lookup_first_name(ident);
    if (!called_function) {
      throw_error_at(ident,
        "attempting to call undefined function \"{}\"", ident.name);
    }
    callee_type = called_function->type;
    called_function_ident = ident.get_raw_self();
  };

  match(call.callee->v)(
    pattern(as<Ast_Identifier>(arg)) = [&](auto& ident){
      look_up_function(ident);
    },
    pattern(as<Ast_Specialized_Identifier>(arg)) = [&](auto& specialized) {
      look_up_function(specialized);
      if (auto generic_func = std::get_if<Ast_Function_Declaration>(&callee_type->v)) {
        if (specialized.specializations.size() > generic_func->type_parameters.size()) {
          throw_error_at(specialized, "too many specializations for generic function");
        }
      } else {
        throw_error_at(specialized, "not a generic function");
      }
      for (auto& spec: specialized.specializations) {
        resolve_type_or_fail(scope, spec, "undeclared specialization type");
      }
    },
    pattern(as<Ast_Path>(arg)) = [&](auto& enum_path) {
      // FIXME: Needs work if paths to functions ever added
      auto [enum_type, _] = AstHelper::path_as_enum_member(enum_path, scope);
      callee_type = enum_type->get_self().class_ptr();
    },
    pattern(_) = [&]{
      callee_type = analyse_expression(*call.callee, scope);
    }
  );

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
        throw_error_at(call.callee, "{} expects {} arguments not {}",
          function_name, expected_arg_count, call.arguments.size());
      }

      for (uint arg_idx = 0; arg_idx < expected_arg_count; arg_idx++) {
        auto argument = call.arguments.at(arg_idx);
        (void) analyse_expression(*argument, scope);

        if constexpr (!is_lambda) {
          auto& expected = function_type.arguments.at(arg_idx);
          assert_valid_binding(expected.name, expected.type, argument);
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
            expected, argument->meta.type,
            argument, infer_note);
        }
      }

      call.callee->meta.type = callee_type;

      if (is_reference_type(function_type.return_type)) {
        call.get_meta().value_type = Expression_Meta::LVALUE;
      }

      // FINALLY we've checked everything in the call!
      return function_type.return_type;
    },
    pattern(as<EnumType>(arg)) = [&](auto& enum_type){
      auto& enum_path = std::get<Ast_Path>(call.callee->v);
      auto& member = enum_type.get_member(enum_path);
      if (!member.data || !std::holds_alternative<TupleType>(member.data->v)) {
        throw_error_at(call.callee, "not a tuple enum member");
      }
      auto& tuple_type = std::get<TupleType>(member.data->v);
      if (tuple_type.element_types.size() != call.arguments.size()) {
        throw_error_at(call, "incorrect number of elements for enum tuple");
      }
      for (size_t idx = 0; idx < tuple_type.element_types.size(); ++idx) {
        auto& element = call.arguments.at(idx);
        (void) analyse_expression(*element, scope);
        assert_valid_binding({}, tuple_type.element_types.at(idx), element);
      }
      return call.callee->meta.type = enum_type.get_self().class_ptr();
    },
    pattern(_) = [&]() -> Type_Ptr {
      throw_error_at(call.callee, NOT_CALLABLE,
        type_to_string(callee_type.get()));
      return nullptr;
    }
  );
}
