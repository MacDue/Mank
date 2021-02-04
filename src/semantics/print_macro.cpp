#include <mpark/patterns.hpp>
#include <boost/algorithm/string/predicate.hpp>

#include "sema/macros.h"
#include "sema/sema_errors.h"

#include "ast/ast_builder.h"

namespace Macros {

#define peek(idx) ((idx) < raw_template.length() ? raw_template[idx] : 0)

#define GET_CALL(idx) ({ \
  auto& stmt = std::get<Ast_Expression_Statement>(print_block.statements.at(idx)->v); \
  &std::get<Ast_Call>(stmt.expression->v); })

Ast_Expression_Type builtin_print(
  Ast_Call& print_call, AstBuilder& builder, Infer&, Lexer*
) {
  using namespace mpark::patterns;
  auto& called_name = std::get<Ast_Macro_Identifier>(print_call.callee->v).name;
  bool error_print = called_name[0] == 'e';

  auto arg_count = print_call.arguments.size();
  if (arg_count < 1) {
    throw_sema_error_at(print_call, "print macro needs at least one argument");
  }

  // "Hello {}!\n Lol!"
  auto first_arg = print_call.arguments.at(0);
  auto format_template = std::get_if<Ast_Literal>(&first_arg->v);
  if (!format_template || format_template->literal_type != PrimativeType::STRING) {
    throw_sema_error_at(first_arg, "must be a constant string literal");
  }
  // remove outer "s
  auto raw_template = std::string_view(format_template->value)
    .substr(1, format_template->value.length() - 2);

  size_t holes_count = 0;
  bool in_hole = false;
  std::vector<size_t> insert_points;
  std::vector<std::string> partial_strings;

  Ast_Block print_block;
  print_block.has_final_expr = false;

  std::string current_string;
  for (size_t idx = 0; idx < raw_template.length() + 1; idx++) {
    auto add_str = [&](std::string str) {
      if (std::strlen(str.c_str()) > 0) {
        auto print_call = builder.make_call(
          !error_print ? "print" : "eprint", builder.make_string(str));
        print_block.statements.push_back(builder.make_expr_stmt(print_call));
        current_string = "";
      }
    };

    char c = peek(idx);
    match(c, peek(idx + 1))(
      pattern('{', '{') = [&]{ WHEN(!in_hole) { current_string += '{'; idx++; }; },
      pattern('}', '}') = [&]{ WHEN(!in_hole) { current_string += '}'; idx++; }; },
      pattern('{', _) = [&]{
        add_str(current_string);
        current_string = "";
        in_hole = true;
        holes_count += 1;
      },
      pattern('}', _) = [&]{
        in_hole = false;
        add_str("[hole]");
        insert_points.push_back(print_block.statements.size() - 1);
      },
      pattern(_, '\0') = [&]{ add_str(current_string + c); },
      pattern(_, _) = [&]{ WHEN(!in_hole) { current_string += c; }; },
      pattern(_, _) = []{}
    );
  }

  if (holes_count != arg_count - 1) {
    throw_sema_error_at(print_call, "number of holes does not match number of arguments");
  }

  for (size_t hole = 0; hole < holes_count; hole++) {
    size_t insert = insert_points.at(hole);
    auto arg = print_call.arguments.at(hole + 1);
    GET_CALL(insert)->arguments.at(0) = arg;
  }

  if (boost::algorithm::ends_with(called_name, "ln")) {
    std::get<Ast_Identifier>(GET_CALL(print_block.statements.size() - 1)->callee->v).name += "ln";
  }
  return print_block;
}

}
