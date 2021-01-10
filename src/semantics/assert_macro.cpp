#include "sema/macros.h"
#include "sema/sema_errors.h"

#include "parser/lexer.h"
#include "ast/ast_builder.h"

namespace Macros {

Ast_Expression_Type builtin_assert(
  Ast_Call& assert_call, AstBuilder& builder, Infer& infer, Lexer* source_file
) {
  // Not the best assert implementation (but it does the trick).

  assert(source_file != nullptr);
  auto args_count = assert_call.arguments.size();
  if (args_count < 1 || args_count > 2) {
    throw_sema_error_at(assert_call, "assert expects one boolean expression");
  }

  auto check = assert_call.arguments.at(0);
  infer.match_or_constrain_types_at(check, check->meta.type, PrimativeType::bool_ty(),
    "check must be {1} expression (is {0})");

  Expr_Ptr check_note = nullptr;
  if (args_count > 1) {
    // has attached note
    check_note = assert_call.arguments.at(1);
    infer.match_or_constrain_types_at(check_note, check_note->meta.type,
      PrimativeType::get(PrimativeType::STRING),
      "note must be a {1} (is {0})");
  }

  SourceLocation check_location = AstHelper::extract_location(check);
  auto check_source = std::string(source_file->extract_source(check_location));

  /*
    {
      check := <check>;
      if !check {
        fail(check_source)
      }
    }
  */

  // <filename>:<line>:
  auto check_location_str = formatxx::format_string("{}:{}: ",
    source_file->input_source_name(), check_location.start_line + 1);

  // <expr> (<note>)
  auto check_error = !check_note
    ? builder.make_string(check_source)
    : builder.make_binary(Ast_Operator::PLUS,
        builder.make_string(check_source + " ("),
        builder.make_binary(Ast_Operator::PLUS, check_note,
        builder.make_string(")")));

  // fail: <location> <error>
  auto assert_check = builder.make_body(false,
    builder.make_var_decl("!check", check),
    builder.make_if_stmt(
      builder.make_unary(Ast_Operator::LOGICAL_NOT, builder.make_ident("!check")),
      builder.make_block(false,
        builder.make_expr_stmt(
          builder.make_call("fail", builder.make_binary(
            Ast_Operator::PLUS,
            builder.make_string(check_location_str),
            check_error
          ))))));
  return assert_check;
}

}
