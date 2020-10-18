#pragma once

#include <sstream>

#include "ast/ast_printer.h"
#include "../catch/catch.hpp"

#define MATCH_AST(expected, given) {                                \
  std::stringstream expected_ss, given_ss;                          \
                                                                    \
  AstPrinter(expected_ss, /*hide lex*/ true).print_file(expected);  \
  AstPrinter(given_ss,                 true).print_file(given);     \
                                                                    \
  auto expected_str = expected_ss.str();                            \
  auto given_str = given_ss.str();                                  \
                                                                    \
  REQUIRE(expected_str == given_str);                               \
}
