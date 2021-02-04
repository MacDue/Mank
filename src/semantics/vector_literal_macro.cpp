#include <mpark/patterns.hpp>

#include "sema/macros.h"
#include "sema/sema_errors.h"

#include "ast/ast_builder.h"

namespace Macros {

  Ast_Expression_Type builtin_vec_literal(
    Ast_Call& vec_literal, AstBuilder& builder, Infer&, Lexer*
  ) {
    using namespace mpark::patterns;
    /*
      vec!([1,2,3])
      ->
      {
        ^vec = new_vec@(i32)();
        push_back(^vec, 1);
        push_back(^vec, 2);
        push_back(^vec, 3);
        ^vec
      }
    */

    auto list = [&]() -> Ast_Array_Literal* {
      if (vec_literal.arguments.size() != 1) return nullptr;
      return std::get_if<Ast_Array_Literal>(&vec_literal.arguments.at(0)->v);
    }();

    if (!list || list->elements.size() <= 0) {
      throw_sema_error_at(vec_literal, "vec macro expects a single array literal");
    }

    auto el_type = list->elements.at(0)->meta.type;
    // ^ prefix disables shadowing warnings
    auto vec_builder = builder.make_body(true,
      builder.make_var_decl("^vec",
        builder.make_call(builder.make_sp_ident("new_vec", el_type))));

    for (auto el: list->elements) {
      vec_builder.statements.push_back(
        builder.make_expr_stmt(
          builder.make_call("push_back", builder.make_ident("^vec"), el)));
    }

    vec_builder.statements.push_back(
      builder.make_expr_stmt(builder.make_ident("^vec"), true));

    return vec_builder;
  }

}
