#pragma once

#include <string>

#include "ast/primative_types.h"
#include "ast/source_location.h"

#include "parser/constants.h"

enum class TokenType {
  #include "parser/token_types.def"
};

struct Token {
  TokenType type = TokenType::EMPTY;
  SourceLocation location;
  PrimativeType::Tag literal_type;
  std::string_view raw_token;
};

struct Lexer {
  struct Position {
    uint char_idx = 0;
    uint line = 0;
    uint column = 0;
  } current;

  void reset();

  /* Loading sources */
  void load_file(std::string const & file_path);
  void set_input_to_string(std::string source, std::string name = "");

  /* Token actions */
  Token& peek_next_token();
  Token get_last_consumed() const;
  void consume_token();
  SourceLocation peek_next_token_location();
  SourceLocation get_last_consumed_location() const;

  /* Error messages */
  std::string_view extract_lines(SourceLocation loc) const;
  std::string_view extract_source(SourceLocation loc) const;

  std::string input_source_name() const;
private:
  std::string source;
  std::string source_name;
  Token last_token, last_consumed;

  /* Char actions */
  int peek_next_char();
  void consume_char();

  void skip_spaces();
  void skip_whitespace(); // includes comments

  void next_token();

  /* To allow easy match backtracking */
  void save_position();
  void restore_position();

  /* Matchers */
  bool match(std::string_view value);
  bool match(std::string_view value, TokenType type);
  bool match_digits(int& start, int& end);
  bool match_numeric_literal();
  bool match_string_literal();
  bool match_char_literal();
  bool one_of(std::string_view chars);

  /* Helpers */
  void set_token_start();
  void set_token_end();
  std::string_view extract_string(int from, int to) const;

  Position saved_position;
};
