#include "catch/catch.hpp"

#include "lexer.h"
#include "parser.h"

TEST_CASE("Hello world!", "[Parser]") {
  Lexer lexer;
  Parser parser(lexer);

  lexer.set_input_to_string(R"(
    proc main {
      print("Hello World");
    }
  )");

  auto parsed_result = parser.parse_file();
}

/*
  Want:
  auto expected_ast = make_file(
    make_procedure("main",
      make_block(
        make_expr_stmt(
          make_call("print",
            make_literal("Hello World")
          )
        )
      )
    )
  )
*/
