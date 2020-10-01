#pragma once

#include <string>

#include "constants.h"
#include "literal_type.h"
#include "source_location.h"

enum class TokenType {
  #include "token_types.def"
};

struct Token {
  TokenType type = TokenType::EMPTY;
  SourceLocation location;
  LiteralType literal_type;
  std::string_view raw_token;
};

struct Lexer {
  struct Position {
    uint char_idx = 0;
    uint line = 0;
    uint column = 0;
  } current;

  void reset();

  /* To allow easy match backtracking */
  void save_state();
  void restore_state();

  /* Loading sources */
  void load_file(std::string const & file_path);
  void set_input_to_string(std::string source);

  /* Token actions */
  Token& peek_next_token();
  void consume_token();
  SourceLocation peek_next_token_location();
  SourceLocation get_last_token_location() const;

  /* Error messages */
  std::string_view extract_lines(SourceLocation loc) const;
  std::string input_source_name() const;
private:
  std::string source;
  std::string source_name;
  Token last_token;

  /* Char actions */
  int peek_next_char();
  void consume_char();

  void skip_spaces();
  void skip_whitespace(); // includes comments

  void next_token();

  /* Matchers */
  bool match(std::string_view value);
  bool match(std::string_view value, TokenType type);
  bool match_digits(int& start, int& end);
  bool match_numeric_literal();
  bool match_string_literal();
  bool one_of(std::string_view chars);

  /* Helpers */
  void set_token_start();
  void set_token_end();
  std::string_view extract_string(int from, int to) const;

  Token saved_token = last_token;
  Position saved_position = current;
};

