#pragma once

#include <ostream>
#include <formatxx/std_string.h>

#include "ast/ast.h"

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

  template<typename TPattern, typename... TArgs>
  void printf(TPattern format_pattern, TArgs const & ... args) {
    for (uint t = 0; t < current_depth; t++) indent();
    os << formatxx::format_string(format_pattern, args...);
  }

  template<typename TPattern, typename... TArgs>
  void putf(TPattern format_pattern, TArgs const & ... args) {
    printf(format_pattern, args...);
    os << '\n';
  }

  public:
    /*
      Hide stuff like locations and filenames.
      Allows the AST Printer to be used a simple way to test parsing.
    */
    bool hide_lex_details;

    uint tab_width,
         current_depth;

    AstPrinter(
      std::ostream& os,
      bool hide_lex_details = false,
      uint tab_width = 1,
      uint current_depth = 0
    ):
      os{os},
      hide_lex_details{hide_lex_details},
      tab_width{tab_width},
      current_depth{current_depth} {}

    void print_args(std::vector<Ast_Argument> args);

    void print_file(Ast_File& file);
    void print_pod(Ast_Pod_Declaration& pod);
    void print_function(Ast_Function_Declaration& func);

    void print_stmt(Ast_Statement& stmt);
    void print_stmt(Ast_Expression_Statement& expr_stmt);
    void print_stmt(Ast_Return_Statement& return_stmt);
    void print_stmt(Ast_Assign& assign);
    void print_stmt(Ast_Variable_Declaration& var_decl);
    void print_stmt(Ast_For_Loop& for_loop);

    void print_const(PrimativeValue const_value);

    void print_expr(Ast_Block& block);
    void print_expr(Ast_If_Expr& if_stmt);
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
