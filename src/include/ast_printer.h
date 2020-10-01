#pragma once

#include <ostream>
#include <formatxx/std_string.h>

#include "ast.h"

class AstPrinter {
  std::ostream& os;

  struct DepthUpdate {
    AstPrinter* ast_printer;

    DepthUpdate(AstPrinter* ast_printer) : ast_printer{ast_printer} {
      this->ast_printer->current_depth += this->ast_printer->tab_width;
    }

    AstPrinter* operator -> () {
      return this->ast_printer;
    }

    ~DepthUpdate() {
      this->ast_printer->current_depth -= this->ast_printer->tab_width;
    }
  };

  void indent() {
    for (uint s = 0; s < tab_width; s++) os << ' ';
  }

  template<typename P, typename... A>
  void printf(P format_pattern, A const & ... args) {
    for (uint t = 0; t < current_depth; t++) indent();
    os << formatxx::format_string(format_pattern, args...);
  }

  template<typename P, typename... A>
  void putf(P format_pattern, A const & ... args) {
    printf(std::forward(format_pattern), std::forward(args...));
    os << '\n';
  }

  public:
    uint tab_width,
         current_depth;

    AstPrinter(std::ostream& os, uint tab_width = 2, uint current_depth = 0)
      : os{os}, tab_width{tab_width}, current_depth{current_depth} {}

    void print_file(Ast_File& file);
    void print_function(Ast_Function_Declaration& func);

    void print_stmt(Ast_Statement& stmt);
    void print_stmt(Ast_Block& block);
    void print_stmt(Ast_If_Statement& if_stmt);
    void print_stmt(Ast_Expression_Statement& expr_stmt);
    void print_stmt(Ast_Return_Statement& return_stmt);

    void print_expr(Ast_Expression& expr);
    void print_expr(Ast_Call& call);
    void print_expr(Ast_Literal& literal);
    void print_expr(Ast_Identifier& ident);
    void print_expr(Ast_Unary_Operation& unary);
    void print_expr(Ast_Binary_Operation& binop);

    DepthUpdate operator -> () {
      return DepthUpdate(this);
    }
};
