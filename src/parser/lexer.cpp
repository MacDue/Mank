#include <fstream>
#include <filesystem>

#include "parser/lexer.h"

/* Setup */

static std::string read_entire_file(std::string const & file) {
  std::ifstream in(file, std::ios::in);
  std::string file_text;
  if (in) {
    in.seekg(0, std::ios::end);
    file_text.resize(in.tellg());
    in.seekg(0, std::ios::beg);
    in.read(&file_text[0], file_text.size());
    in.close();
  } else {
    /* TODO: Handle errors */
  }
  return file_text;
}

void Lexer::reset() {
  this->last_token = Token{};
  this->last_consumed = this->last_token;
  this->current = Position{};
  this->save_position();
}

void Lexer::load_file(std::string const & file_path) {
  this->reset();
  this->source = read_entire_file(file_path);
  this->source_name = std::filesystem::path(file_path).filename();
}

void Lexer::set_input_to_string(std::string source) {
  this->reset();
  this->source = source;
  this->source_name = "<string>";
}

/* Token actions */

Token& Lexer::peek_next_token() {
  if (this->last_token.type == TokenType::EMPTY) {
    this->next_token();
  }
  return this->last_token;
}

void Lexer::consume_token() {
  this->last_consumed = this->last_token;
  this->last_token.type = TokenType::EMPTY;
}

Token Lexer::get_last_consumed() const {
  return this->last_consumed;
}

SourceLocation Lexer::peek_next_token_location() {
  (void) this->peek_next_token();
  return this->last_token.location;
}

SourceLocation Lexer::get_last_consumed_location() const {
  return this->last_consumed.location;
}

/* Error messages */

std::string_view Lexer::extract_lines(SourceLocation loc) const {
  auto pior_newline = source.rfind('\n', loc.start_char_idx);
  auto next_newline = source.find('\n', loc.end_char_idx);

  auto line_start = pior_newline == std::string::npos ? 0 : pior_newline + 1;
  auto line_end = next_newline == std::string::npos ? source.size() - 1 : next_newline;
  return extract_string(line_start, line_end);
}

std::string_view Lexer::extract_source(SourceLocation loc) const {
  return extract_string(loc.start_char_idx, loc.end_char_idx);
}


/* Char actions */

int Lexer::peek_next_char() {
  if (this->current.char_idx >= this->source.size()) {
    return EOF;
  }
  return this->source.at(this->current.char_idx);
}

void Lexer::consume_char() {
  // TODO: Windows line endings?
  if (this->peek_next_char() == '\n') {
    this->current.line += 1;
    this->current.column = -1; // immediately incremented
  }
  this->current.char_idx += 1;
  this->current.column += 1;
}

void Lexer::skip_spaces() {
  while (isascii(this->peek_next_char()) && isspace(this->peek_next_char())) {
    this->consume_char();
  }
}

void Lexer::skip_whitespace() {
  this->skip_spaces();
  while (this->match("#")) {
    while (this->peek_next_char() != EOF && this->peek_next_char() != '\n') {
      this->consume_char();
    }
    this->skip_spaces();
  }
}

/* Token parsing */

static TokenType ident_to_token(std::string_view ident) {
  if (ident == "fun") {
    return TokenType::FUNCTION;
  } else if (ident == "proc") {
    return TokenType::PROCEDURE;
  } else if (ident == "domain") {
    return TokenType::DOMAIN;
  } else if (ident == "of") {
    return TokenType::OF;
  } else if (ident == "spawn") {
    return TokenType::SPAWN;
  } else if (ident == "if") {
    return TokenType::IF;
  } else if (ident == "else") {
    return TokenType::ELSE;
  } else if (ident == "for") {
    return TokenType::FOR;
  } else if (ident == "while") {
    return TokenType::WHILE;
  } else if (ident == "until") {
    return TokenType::UNTIL;
  } else if (ident == "in") {
    return TokenType::IN;
  } else if (ident == "pod") {
    return TokenType::POD;
  } else if (ident == "return") {
    return TokenType::RETURN;
  } else if (ident == "true") {
    return TokenType::TRUE;
  } else if (ident == "false") {
    return TokenType::FALSE;
  } else if (ident == "ref") {
    return TokenType::REF;
  } else if (ident == "bind") {
    return TokenType::BIND;
  } else if (ident == "loop") {
    return TokenType::LOOP;
  } else if (ident == "break") {
    return TokenType::BREAK;
  } else if (ident == "continue") {
    return TokenType::CONTINUE;
  } else if (ident == "as") {
    return TokenType::AS;
  } else if (ident == "test") {
    return TokenType::TEST;
  } else {
    return TokenType::IDENT;
  }
}

void Lexer::next_token() {
  this->skip_whitespace();
  this->set_token_start();
  int token_start = this->current.char_idx;
  /*
    Mutli-char operator tokens must be parsed first!
    Otherwise '-=' will be parsed as MINUS, ASSIGN rather than MINUS_EQUAL */
  if (
    /* Multi-char operators */
       match("|!", TokenType::BITWISE_XOR)
    || match("||", TokenType::LOGICAL_OR)
    || match("<<", TokenType::LEFT_SHIFT)
    || match(">>", TokenType::RIGHT_SHIFT)
    || match(">=", TokenType::GREATER_EQUAL)
    || match("<=", TokenType::LESS_EQUAL)
    || match("==", TokenType::EQUAL_TO)
    || match("!=", TokenType::NOT_EQUAL_TO)
    || match("&&", TokenType::LOGICAL_AND)
    /* Multi-char assign */
    || match("+=", TokenType::PLUS_EQUAL)
    || match("-=", TokenType::MINUS_EQUAL)
    || match("|=", TokenType::BITWISE_OR_EQUAL)
    || match("&=", TokenType::BITWISE_AND_EQUAL)
    || match("/=", TokenType::DIVIDE_EQUAL)
    || match("*=", TokenType::TIMES_EQUAL)
    || match("%=", TokenType::MODULO_EQUAL)
    /* Muti-char (misc) tokens */
    || match("->", TokenType::ARROW)
    /* Single-char operators */
    || match("+", TokenType::PLUS)
    || match("-", TokenType::MINUS)
    || match("/", TokenType::DIVIDE)
    || match("*", TokenType::TIMES)
    || match("%", TokenType::MODULO)
    || match("<", TokenType::LESS_THAN)
    || match(">", TokenType::GREATER_THAN)
    || match("~", TokenType::BITWISE_NOT)
    || match("&", TokenType::BITWISE_AND)
    || match("|", TokenType::BITWISE_OR)
    || match("!", TokenType::EXCLAMATION_MARK)
    || match("¬", TokenType::LOGICAL_NOT)
    /* Basic elements */
    || match("..", TokenType::DOUBLE_DOT)
    || match(",", TokenType::COMMA)
    || match(";", TokenType::SEMICOLON)
    || match(":", TokenType::COLON)
    || match(".", TokenType::DOT)
    || match("{", TokenType::LEFT_BRACE)
    || match("}", TokenType::RIGHT_BRACE)
    || match("(", TokenType::LEFT_PAREN)
    || match(")", TokenType::RIGHT_PAREN)
    || match("[", TokenType::LEFT_SQUARE_BRACKET)
    || match("]", TokenType::RIGHT_SQUARE_BRACKET)
    || match("=", TokenType::ASSIGN)
    || match("@", TokenType::AT)
    || match("\\", TokenType::BACKSLASH)
  ) {
    /* Simple token is now matched (last_token updated in match) */
  } else if (isalpha(this->peek_next_char()) || this->peek_next_char() == '_') {
    /*
      IDENT: (_|[A-Za-z])(_|[A-Za-z]|[0-9])+
    */
    int ident_start = this->current.char_idx;
    while (isalnum(this->peek_next_char()) || this->peek_next_char() == '_') {
      this->consume_char();
    }
    auto ident = this->extract_string(ident_start, this->current.char_idx);
    this->last_token.type = ident_to_token(ident);
  } else if (match_numeric_literal()) {
    /* Numeric literal now matched & token set */
  } else if (match_string_literal()) {
    /* String literal now matched & token set */
  } else if (match_char_literal()) {
    /* Char literal now matched & token set */
  } else if (this->peek_next_char() == EOF) {
    this->last_token.type = TokenType::LEX_EOF;
  } else {
    this->last_token.type = TokenType::INVALID;
    this->consume_char();
  }
  this->last_token.raw_token = this->extract_string(token_start, this->current.char_idx);
  this->set_token_end();
}

/* Matchers */

bool Lexer::match(std::string_view value) {
  this->save_position();
  for (char c: value) {
    if (this->peek_next_char() == c) {
      this->consume_char();
    } else {
      this->restore_position();
      return false;
    }
  }
  return true;
}

bool Lexer::match(std::string_view value, TokenType type) {
  if (match(value)) {
    this->last_token.type = type;
    return true;
  }
  return false;
}

bool Lexer::match_digits(int& start, int& end) {
  start = this->current.char_idx;
  if (!isdigit(this->peek_next_char())) {
    return false;
  }
  while (isdigit(this->peek_next_char())) {
    this->consume_char();
  }
  end = this->current.char_idx;
  return true;
}

bool Lexer::match_numeric_literal() {
  /*
    Numeric literal: [0-9]+(\.[0-9]+)?
    TODO float literals like .3 (currently will be parsed as DOT 3)
      -- maybe handle in parser
  */
  int leading_digits_start, leading_digits_end;
  if (match_digits(leading_digits_start, leading_digits_end)) {
    this->last_token.type = TokenType::LITERAL;
    if (match(".")) {
      this->last_token.literal_type = PrimativeType::FLOAT64;
      int decimal_digits_start, decimal_digits_end;
      if (match_digits(decimal_digits_start, decimal_digits_end)) {
        /* Nothing needs to happen */
      }
      this->last_token.literal_type = PrimativeType::FLOAT64;
    } else {
      this->last_token.literal_type = PrimativeType::INTEGER;
    }
    return true;
  }
  return false;
}

bool Lexer::match_string_literal() {
  if (match("\"")) {
    this->last_token.type = TokenType::LITERAL;
    this->last_token.literal_type = PrimativeType::STRING;

    int next_char;
    while ((next_char = this->peek_next_char() != '"')) {
      if (next_char == EOF) {
        // TODO: Unclosed string literal
        this->last_token.type = TokenType::INVALID;
        break;
      }
      if (!match("\\\"") /* match and consume \" (an escaped quote)*/) {
        this->consume_char();
      }
    }

    this->consume_char();
    return true;
  } else {
    return false;
  }
}

bool Lexer::match_char_literal() {
  if (match("'")) {
    this->last_token.type = TokenType::LITERAL;
    this->last_token.literal_type = PrimativeType::CHAR;
    if (match("\\")) {
      this->consume_char();
    } else if (this->peek_next_char() != '\'') {
      this->consume_char();
    } else {
      this->last_token.type = TokenType::INVALID;
    }
    if (!this->match("'")) {
      this->last_token.type = TokenType::INVALID;
    }
    return true;
  } else {
    return false;
  }
}

/* Helpers */

void Lexer::save_position() {
  this->saved_position = this->current;
}

void Lexer::restore_position() {
  this->current = this->saved_position;
}

void Lexer::set_token_start() {
  this->last_token.location.start_line = this->current.line;
  this->last_token.location.start_column = this->current.column;
  this->last_token.location.start_char_idx = this->current.char_idx;
}

void Lexer::set_token_end() {
  this->last_token.location.end_line = this->current.line;
  this->last_token.location.end_column = this->current.column;
  this->last_token.location.end_char_idx = this->current.char_idx;
}

std::string_view Lexer::extract_string(int from, int to) const {
  std::string_view view = this->source;
  return view.substr(from, to - from);
}

std::string Lexer::input_source_name() const {
  return this->source_name;
}
