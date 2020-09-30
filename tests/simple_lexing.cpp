#include <array>
#include <utility>

#include "catch/catch.hpp"

#include "lexer.h"

#define next_token_type(lexer) (lexer).peek_next_token().type
#define LOG_SOURCE(source) INFO("Source is \"" << (source) << '"')

static void expect_eof(Lexer& lexer) {
  lexer.consume_token();
  REQUIRE(next_token_type(lexer) == TokenType::LEX_EOF);
}

static void expect_single_token(
  Lexer& lexer, char const * source_str, TokenType expected_token
) {
  LOG_SOURCE(source_str);
  lexer.set_input_to_string(source_str);
  REQUIRE(next_token_type(lexer) == expected_token);
  expect_eof(lexer);
}

template<typename T, size_t N>
inline void expect_all_single_tokens(Lexer& lexer, std::array<T,N> const & tests) {
  for (auto [source_str, expected_token] : tests) {
    expect_single_token(lexer, source_str, expected_token);
  }
}

TEST_CASE("Lex basic elements", "[Lexer]") {
  Lexer lexer;

  std::array basic_element_and_expected_token {
    std::make_pair(";", TokenType::SEMICOLON),
    std::make_pair(":", TokenType::COLON),
    std::make_pair(".", TokenType::DOT),
    std::make_pair("{", TokenType::LEFT_BRACE),
    std::make_pair("}", TokenType::RIGHT_BRACE),
    std::make_pair("(", TokenType::LEFT_PAREN),
    std::make_pair(")", TokenType::RIGHT_PAREN),
    std::make_pair("=", TokenType::ASSIGN),
    /* with padding */
    std::make_pair("   ;", TokenType::SEMICOLON),
    std::make_pair("}   ", TokenType::RIGHT_BRACE),
    std::make_pair("  =   ", TokenType::ASSIGN),
    std::make_pair("= # this is a '=' sign", TokenType::ASSIGN),
  };

  expect_all_single_tokens(lexer, basic_element_and_expected_token);
}


TEST_CASE("Lex operators", "[Lexer]") {
  Lexer lexer;

  std::array operators_and_expected_token {
    std::make_pair("+", TokenType::PLUS),
    std::make_pair("-", TokenType::MINUS),
    std::make_pair("/", TokenType::DIVIDE),
    std::make_pair("*", TokenType::TIMES),
    std::make_pair("%", TokenType::MODULO),
    std::make_pair("<", TokenType::LESS_THAN),
    std::make_pair(">", TokenType::GREATER_THAN),
    std::make_pair("~", TokenType::BITWISE_NOT),
    std::make_pair("&", TokenType::BITWISE_AND),
    std::make_pair("|", TokenType::BITWISE_OR),
    std::make_pair("¬", TokenType::LOGICAL_NOT),
    std::make_pair("|!", TokenType::BITWISE_XOR),
    std::make_pair("||", TokenType::LOGICAL_OR),
    std::make_pair("<<", TokenType::LEFT_SHIFT),
    std::make_pair(">>", TokenType::RIGHT_SHIFT),
    std::make_pair(">=", TokenType::GREATER_EQUAL),
    std::make_pair("<=", TokenType::LESS_EQUAL),
    std::make_pair("==", TokenType::EQUAL_TO),
    std::make_pair("!=", TokenType::NOT_EQUAL_TO),
    std::make_pair("&&", TokenType::LOGICAL_AND),
  };

  expect_all_single_tokens(lexer, operators_and_expected_token);
}

TEST_CASE("Lex assignment operators", "[Lexer]") {
  Lexer lexer;

  std::array assigment_operators {
    std::make_pair("+=", TokenType::PLUS_EQUAL),
    std::make_pair("-=", TokenType::MINUS_EQUAL),
    // std::make_pair("||=", TokenType::PLUS),
    std::make_pair("|=", TokenType::BITWISE_OR_EQUAL),
    std::make_pair("&=", TokenType::BITWISE_AND_EQUAL),
    std::make_pair("/=", TokenType::DIVIDE_EQUAL),
    std::make_pair("*=", TokenType::TIMES_EQUAL),
    std::make_pair("%=", TokenType::MODULO_EQUAL),
  };

  expect_all_single_tokens(lexer, assigment_operators);
}

TEST_CASE("Lex keywords", "[Lexer]") {
  Lexer lexer;

  std::array keywords {
    std::make_pair("domain", TokenType::DOMAIN),
    std::make_pair("of"    , TokenType::OF),
    std::make_pair("fun"   , TokenType::FUNCTION),
    std::make_pair("proc"  , TokenType::PROCEDURE),
    std::make_pair("spawn" , TokenType::SPAWN),
    std::make_pair("for"   , TokenType::FOR),
    std::make_pair("while" , TokenType::WHILE),
    std::make_pair("until" , TokenType::UNTIL),
    std::make_pair("if"    , TokenType::IF),
    std::make_pair("else"  , TokenType::ELSE),
    std::make_pair("pod"   , TokenType::POD),
    std::make_pair("return", TokenType::RETURN),
    std::make_pair("in"    , TokenType::IN),
  };

  expect_all_single_tokens(lexer, keywords);
}

TEST_CASE("Lex identifiers", "[Lexer]") {
  Lexer lexer;

  // TODO: Some invalid identifiers
  std::array valid_identifiers {
    std::make_pair("FooBar"    , TokenType::IDENT),
    std::make_pair("foo_bar"   , TokenType::IDENT),
    std::make_pair("_foo"      , TokenType::IDENT),
    std::make_pair("_0123"     , TokenType::IDENT),
    std::make_pair("dog1"      , TokenType::IDENT),
    std::make_pair("globalVars", TokenType::IDENT),
  };

  expect_all_single_tokens(lexer, valid_identifiers);
}

TEST_CASE("Lex string literals", "[Lexer]") {
  Lexer lexer;

  std::array valid_string_literals {
    R"("Hello, World!")",
    R"("You said \"Banana\"")",
    R"("void main() { return 1 + 2; } ")",
  };

  for (auto source_literal: valid_string_literals) {
    lexer.set_input_to_string(source_literal);
    auto token = lexer.peek_next_token();
    REQUIRE(token.type == TokenType::LITERAL);
    REQUIRE(token.literal_type == LiteralType::STRING);
    REQUIRE(token.raw_token == source_literal);
    expect_eof(lexer);
  }
}

TEST_CASE("Lex numerical literals", "[Lexer]") {
  Lexer lexer;

  // Note: prefexes +/- are treated as unary expressions
  std::array valid_numerical_literals_and_expected_type {
    std::make_pair("42", LiteralType::INTEGER),
    std::make_pair("10000.", LiteralType::FLOAT64),
    std::make_pair("3.14", LiteralType::FLOAT64),
    // Note the literals are not parsed yet (that is done later)
    std::make_pair("999999999999999999999999", LiteralType::INTEGER),
  };

  for (auto [numeric_literal, expected_literal_type]: valid_numerical_literals_and_expected_type) {
    lexer.set_input_to_string(numeric_literal);
    auto token = lexer.peek_next_token();
    REQUIRE(token.type == TokenType::LITERAL);
    REQUIRE(token.literal_type == expected_literal_type);
    REQUIRE(token.raw_token == numeric_literal);
    expect_eof(lexer);
  }
}
