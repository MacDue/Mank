#include "token_helpers.h"

// TODO: generate with a script reading the token_types.def file
char const * token_type_to_string(TokenType token_type) {
  switch (token_type) {
    /* Basic elements */
    case TokenType::SEMICOLON:    return "\";\" (semicolon)";
    case TokenType::COLON:        return "\":\" (colon)";
    case TokenType::DOT:          return "\".\" (dot)";
    case TokenType::LEFT_BRACE:   return "\"{\" (left brace)";
    case TokenType::RIGHT_BRACE:  return "\"}\" (right brace)";
    case TokenType::LEFT_PAREN:   return "\"(\" (left paren)";
    case TokenType::RIGHT_PAREN:  return "\")\" (right paren)";
    case TokenType::ASSIGN:       return "\"=\" (assign)";
    case TokenType::COMMA:        return "\",\" (comma)";

    /* Operators */
    case TokenType::PLUS:           return "\"+\" (plus)";
    case TokenType::MINUS:          return "\"-\" (minus)";
    case TokenType::DIVIDE:         return "\"/\" (divide)";
    case TokenType::TIMES:          return "\"*\" (times)";
    case TokenType::MODULO:         return "\"%\" (modulo)";
    case TokenType::LESS_THAN:      return "\"<\" (less than)";
    case TokenType::GREATER_THAN:   return "\">\" (greater than)";
    case TokenType::BITWISE_NOT:    return "\"~\" (bitwise not)";
    case TokenType::BITWISE_AND:    return "\"&\" (bitwise and)";
    case TokenType::BITWISE_OR:     return "\"|\" (bitwise or)";
    case TokenType::LOGICAL_NOT:    return "\"¬\" (logical not)";
    case TokenType::BITWISE_XOR:    return "\"|!\" (bitwise xor)";
    case TokenType::LOGICAL_OR:     return "\"||\" (logical or)";
    case TokenType::LEFT_SHIFT:     return "\"<<\" (left shift)";
    case TokenType::RIGHT_SHIFT:    return "\">>\" (right shift)";
    case TokenType::GREATER_EQUAL:  return "\">=\" (greater equal)";
    case TokenType::LESS_EQUAL:     return "\"<=\" (less equal)";
    case TokenType::EQUAL_TO:       return "\"==\" (equal to)";
    case TokenType::NOT_EQUAL_TO:   return "\"!=\" (not equal to)";
    case TokenType::LOGICAL_AND:    return "\"&&\" (logical and)";

    /* Assigment operators */
    case TokenType::PLUS_EQUAL:         return "\"+=\" (plus equal)";
    case TokenType::MINUS_EQUAL:        return "\"-=\" (minus equal)";
    case TokenType::BITWISE_OR_EQUAL:   return "\"|=\" (bitwise or equal)";
    case TokenType::BITWISE_AND_EQUAL:  return "\"&=\" (bitwise and equal)";
    case TokenType::DIVIDE_EQUAL:       return "\"'/=\" (divide equal)";
    case TokenType::TIMES_EQUAL:        return "\"*=\" (times equal)";
    case TokenType::MODULO_EQUAL:       return "\"%=\" (modulo equal)";

    /* Keywords / identifiers */
    case TokenType::IDENT:      return "identifier";
    case TokenType::DOMAIN:     return "domain";
    case TokenType::OF:         return "of";
    case TokenType::FUNCTION:   return "fun";
    case TokenType::PROCEDURE:  return "proc";
    case TokenType::SPAWN:      return "spawn";
    case TokenType::FOR:        return "for";
    case TokenType::WHILE:      return "while";
    case TokenType::UNTIL:      return "until";
    case TokenType::IF:         return "if";
    case TokenType::ELSE:       return "else";
    case TokenType::POD:        return "pod";
    case TokenType::RETURN:     return "return";
    case TokenType::LITERAL:    return "literal";
    case TokenType::IN:         return "in";

    /* Lexer state */
    case TokenType::LEX_EOF:    return "[Lexer EOF]";
    case TokenType::EMPTY:      return "[Empty token]";
    case TokenType::INVALID:    return "[Invalid token]";

    default:
      return "??? (fix me!)";
  }
}
