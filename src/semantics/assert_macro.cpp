#include "sema/macros.h"

#include "parser/lexer.h"
#include "ast/ast_builder.h"
#include "errors/compiler_errors.h"

namespace Macros {

#define ASSERT_TEMPLATE "Assertion failed: {source_location}: `{check_source}'"

Ast_Expression_Type builtin_assert(
  Ast_Call& assert_call, AstBuilder& builder, Infer& infer, Lexer* source_file
) {
  assert(source_file != nullptr);
  auto args_count = assert_call.arguments.size();
  if (args_count < 1 || args_count > 2) {
    throw_error_at(assert_call, "assert expects one boolean expression");
  }

  auto check = assert_call.arguments.at(0);
  infer.match_or_constrain_types_at(check, check->meta.type, PrimativeType::bool_ty(),
    "check must be {1} expression (is {0})");

  Expr_Ptr check_note = nullptr;
  if (args_count > 1) {
    // has attached note
    check_note = assert_call.arguments.at(1);
    infer.match_or_constrain_types_at(check_note, check_note->meta.type,
      PrimativeType::str_ty(), "note must be a {1} (is {0})");
  }

  SourceLocation check_location = AstHelper::extract_location(check);
  auto check_source = std::string(source_file->extract_source(check_location));

  /*
    {
      check := <check>;
      if !check {
        eprintln!(...error...); # TODO: make special function for this in asserts
        abort();
      }
    }
  */

  // <filename>:<line>:
  auto check_location_str = formatxx::format_string("{}:{}",
    source_file->input_source_name(), check_location.start_line + 1);

  auto check_error = builder.make_call(
    builder.make_macro_ident("eprintln"),
    builder.make_string(
      !check_note ? ASSERT_TEMPLATE : ASSERT_TEMPLATE " ({note})"),
    builder.make_string(check_location_str),
    builder.make_string(check_source));
  if (check_note) {
    std::get<Ast_Call>(check_error->v).arguments.push_back(check_note);
  }

  auto assert_check = builder.make_body(false,
    builder.make_var_decl("!check", check),
    builder.make_if_stmt(
      builder.make_unary(Ast_Operator::LOGICAL_NOT, builder.make_ident("!check")),
      builder.make_block(false,
        builder.make_expr_stmt(check_error),
        builder.make_expr_stmt(builder.make_call(builder.make_ident("abort"))))));
  assert_check.location = assert_call.location;
  return assert_check;
}

}
