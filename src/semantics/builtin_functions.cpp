#include <vector>

#include "ast/ast_builder.h"
#include "sema/builtin_functions.h"

namespace Builtin {

static Symbol make_builtin_func(
  AstContext& ctx,
  std::string name,
  std::vector<Ast_Argument> args,
  Type_Ptr return_type = Type::void_ty(),
  bool c_decl = false,
  std::vector<TypeParam> type_params = {}
) {
  Ast_Function_Declaration func_decl;
  func_decl.return_type = return_type;
  func_decl.identifier.name = name;
  func_decl.arguments = args;
  func_decl.external = true;
  func_decl.c_function = c_decl;
  func_decl.generic = type_params.size() > 0;
  func_decl.type_parameters = type_params;
  return Symbol(
    SymbolName(name), ctx.new_type(func_decl), Symbol::FUNCTION);
}

static Type_Ptr make_generic_type(AstContext& ctx, std::string name) {
  GenericType generic;
  generic.identifier.name = name;
  return ctx.new_type(generic);
}

void add_builtins_to_scope(Scope& scope, AstContext& ctx, AstBuilder& builder) {
  /*
    Hacky way to register some builtin functions.
    Some of these are C stdlib functions, some are implemented in Mank,
    and some are implemented in codegen (with mank runtime functions).
  */

  TupleType parse_int_ret;
  parse_int_ret.element_types = {
    PrimativeType::int_ty(), PrimativeType::bool_ty()
  };

  auto vec_element_type = make_generic_type(ctx, "E");
  auto vec_type = [&]{
    ListType list_type;
    list_type.element_type = vec_element_type;
    return ctx.new_type(list_type);
  }();
  auto vec_ref = builder.make_argument(builder.make_reference(vec_type), "v");

  std::array builtin_funcs {
    // C stdlib putchar/getchar
    make_builtin_func(ctx, "putchar", builder.make_args(
      builder.make_argument(PrimativeType::char_ty(), "c")), PrimativeType::int_ty(), true),
    make_builtin_func(ctx, "stderr_putchar", builder.make_args(
      builder.make_argument(PrimativeType::char_ty(), "c")), PrimativeType::int_ty(), true),
    make_builtin_func(ctx, "getchar", {}, PrimativeType::int_ty(), true),
    make_builtin_func(ctx, "abort", {}, Type::void_ty(), true),
    make_builtin_func(ctx, "sqrt", builder.make_args(
      builder.make_argument(PrimativeType::f64_ty(), "f")), PrimativeType::f64_ty(), true),
    make_builtin_func(ctx, "pow", builder.make_args(
      builder.make_argument(PrimativeType::f64_ty(), "x"),
      builder.make_argument(PrimativeType::f64_ty(), "y")),
      PrimativeType::f64_ty(), true),
    make_builtin_func(ctx, "sin", builder.make_args(
      builder.make_argument(PrimativeType::f64_ty(), "f")), PrimativeType::f64_ty(), true),
    make_builtin_func(ctx, "cos", builder.make_args(
      builder.make_argument(PrimativeType::f64_ty(), "f")), PrimativeType::f64_ty(), true),
    make_builtin_func(ctx, "tan", builder.make_args(
      builder.make_argument(PrimativeType::f64_ty(), "f")), PrimativeType::f64_ty(), true),
    make_builtin_func(ctx, "asin", builder.make_args(
      builder.make_argument(PrimativeType::f64_ty(), "f")), PrimativeType::f64_ty(), true),
    make_builtin_func(ctx, "atan2", builder.make_args(
      builder.make_argument(PrimativeType::f64_ty(), "x"),
      builder.make_argument(PrimativeType::f64_ty(), "y")),
      PrimativeType::f64_ty(), true),
    // builtin mank functions
    make_builtin_func(ctx, "eprint", builder.make_args(
      builder.make_argument(PrimativeType::str_ty(), "s"))),
    make_builtin_func(ctx, "eprintln", builder.make_args(
      builder.make_argument(PrimativeType::str_ty(), "s"))),
    make_builtin_func(ctx, "print", builder.make_args(
      builder.make_argument(PrimativeType::str_ty(), "s"))),
    make_builtin_func(ctx, "println", builder.make_args(
      builder.make_argument(PrimativeType::str_ty(), "s"))),
    make_builtin_func(ctx, "fail", builder.make_args(
      builder.make_argument(PrimativeType::str_ty(), "s"))),
    make_builtin_func(ctx, "input", {}, PrimativeType::str_ty()),
    make_builtin_func(ctx, "prompt", builder.make_args(
      builder.make_argument(PrimativeType::str_ty(), "s")), PrimativeType::str_ty()),
    make_builtin_func(ctx, "parse_int", builder.make_args(
      builder.make_argument(PrimativeType::str_ty(), "s")), ctx.new_type(parse_int_ret)),
    make_builtin_func(ctx, "int_to_string", builder.make_args(
      builder.make_argument(PrimativeType::int_ty(), "i")), PrimativeType::str_ty()),
    make_builtin_func(ctx, "input_int", {}, PrimativeType::int_ty()),
    make_builtin_func(ctx, "prompt_int", builder.make_args(
      builder.make_argument(PrimativeType::str_ty(), "s")), PrimativeType::int_ty()),
    make_builtin_func(ctx, "str_compare", builder.make_args(
      builder.make_argument(PrimativeType::str_ty(), "a"),
      builder.make_argument(PrimativeType::str_ty(), "b")), PrimativeType::int_ty()),
    make_builtin_func(ctx, "str_equal", builder.make_args(
        builder.make_argument(PrimativeType::str_ty(), "a"),
        builder.make_argument(PrimativeType::str_ty(), "b")), PrimativeType::bool_ty()),
    // vectors
    make_builtin_func(ctx, "new_vec", {}, vec_type, false, { vec_element_type }),
    make_builtin_func(ctx, "push_back", builder.make_args(vec_ref,
      builder.make_argument(vec_element_type, "e")), Type::void_ty(), false, { vec_element_type }),
    make_builtin_func(ctx, "pop_back", builder.make_args(vec_ref), Type::void_ty(), false, { vec_element_type }),
    make_builtin_func(ctx, "fill_vec", builder.make_args(vec_ref,
      builder.make_argument(vec_element_type, "e"),
      builder.make_argument(PrimativeType::int_ty(), "c")), Type::void_ty(), false, { vec_element_type })
  };

  for (auto builtin: builtin_funcs) {
    // Needed for some decl types
    std::get<Ast_Function_Declaration>(builtin.type->v).body.scope.set_parent(scope);
    scope.add(builtin);
  }
}

void add_test_runner_main(Ast_File& file) {
  std::vector<Function_Ptr> tests;

  // Ugh.... Just collecting tests & deleting the old main
  size_t idx = 0;
  std::optional<size_t> main_idx;
  for (auto func: file.functions) {
    if (func->test) {
      func->procedure = true;
      tests.push_back(func);
    }
    if (func->identifier.name == "main") {
      main_idx = idx;
    }
    idx += 1;
  }

  if (tests.empty()) {
    return;
  }

  if (main_idx) {
    file.functions.erase(file.functions.begin() + *main_idx);
  }

  AstBuilder builder(file);
  auto main_body = builder.make_body(false);

  size_t test_num = 1;
  for (auto test: tests) {
    main_body.statements.push_back(
      builder.make_expr_stmt(
        builder.make_block(false,
          builder.make_expr_stmt(
            builder.make_call(
              builder.make_macro_ident("println"),
              builder.make_string("[{n}/{total}] running test `{test}'"),
              builder.make_string(std::to_string(test_num)),
              builder.make_string(std::to_string(tests.size())),
              builder.make_string(test->identifier.name))
          ),
          builder.make_expr_stmt(
            builder.make_call(test->identifier.name)))));
    test_num += 1;
  }

  auto tests_main = builder.make_procedure("main", std::move(main_body));
  tests_main.return_type = Type::void_ty();
  tests_main.procedure = true;

  builder.add_functions(tests_main);
}

}
