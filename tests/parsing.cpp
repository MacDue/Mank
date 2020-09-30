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
