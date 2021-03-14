#include <mpark/patterns.hpp>

#include "sema/macros.h"
#include "errors/compiler_errors.h"

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

      vec!([=42, 100]);
      ->
      {
        ^vec = new_vec@(i32)();
        fill_vec(^vec, 42, 100);
        ^vec
      }
    */
    // ^ prefix disables shadowing warnings

    if (vec_literal.arguments.size() != 1) {
      throw_error_at(vec_literal, "vec macro expects only a single array literal argument");
    }

    auto array_literal = vec_literal.arguments.at(0);

    auto vec_builder = builder.make_body(true);
    auto vec_ident = builder.make_ident("^vec");

    auto decl_vec = [&](Type_Ptr el_type){
      vec_builder.statements.push_back(
        builder.make_var_decl("^vec",
          builder.make_call(builder.make_sp_ident("new_vec", el_type))));
    };

    match(array_literal->v)(
      pattern(as<Ast_Array_Literal>(arg)) = [&](auto& array){
        if (array.elements.size() <= 0) {
          throw_error_at(vec_literal, "cannot infer vector type from empty array");
        }
        auto el_type = array.elements.at(0)->meta.type;
        decl_vec(el_type);
        for (auto el: array.elements) {
          vec_builder.statements.push_back(
            builder.make_expr_stmt(
              builder.make_call("push_back", vec_ident, el)));
        }
      },
      pattern(as<Ast_Array_Repeat>(arg)) = [&](auto& array_repeat) {
        auto el_type = array_repeat.initializer->meta.type;
        decl_vec(el_type);
        vec_builder.statements.push_back(
          builder.make_expr_stmt(
            builder.make_call("fill_vec", vec_ident,
              array_repeat.initializer, array_repeat.repetitions)));
      },
      pattern(_) = [&]{
        throw_error_at(array_literal, "invalid vector initializer");
      }
    );

    // ret vec
    vec_builder.statements.push_back(
      builder.make_expr_stmt(builder.make_ident("^vec"), true));

    return vec_builder;
  }

}
