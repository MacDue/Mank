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

    inline std::string type_to_string(Type_Ptr type) {
      return ::type_to_string(type.get(), hide_lex_details);
    }

    void print_args(std::vector<Ast_Argument> const & args);
    void print_types(std::vector<Type_Ptr> const & types);

    void print_enum_members(
      std::vector<Ast_Enum_Declaration::Member> const & enum_members);

    void print_file(Ast_File& file);
    void print_pod(Ast_Pod_Declaration& pod);
    void print_enum(Ast_Enum_Declaration& enum_decl);
    void print_type_alias(Ast_Type_Alias& type_alias);
    void print_function(Ast_Function_Declaration& func);

    void print_stmt(Ast_Statement& stmt);
    void print_stmt(Ast_Expression_Statement& expr_stmt);
    void print_stmt(Ast_Return_Statement& return_stmt);
    void print_stmt(Ast_Assign& assign);
    void print_stmt(Ast_Variable_Declaration& var_decl);
    void print_stmt(Ast_For_Loop& for_loop);
    void print_stmt(Ast_Structural_Binding& binding);
    void print_stmt(Ast_Loop& loop);
    void print_stmt(Ast_While_Loop& while_loop);
    void print_stmt(Ast_Loop_Control& loop_control);
    void print_stmt(Ast_Constant_Declaration& const_decl);

    void print_binding(Ast_Binding const & binding);
    void print_binding(Ast_Tuple_Binds const & tuple_binds);
    void print_binding(Ast_Pod_Binds const & pod_binds);

    void print_const(PrimativeValue const_value);

    void print_expr(Ast_Block& block);
    void print_expr(Ast_If_Expr& if_stmt);
    void print_expr(Ast_Expression& expr);
    void print_expr(Ast_Call& call);
    void print_expr(Ast_Literal& literal);
    void print_expr(Ast_Identifier& ident);
    void print_expr(Ast_Unary_Operation& unary);
    void print_expr(Ast_Binary_Operation& binop);
    void print_expr(Ast_Field_Access& access);
    void print_expr(Ast_Array_Literal& array);
    void print_expr(Ast_Index_Access& index);
    void print_expr(Ast_Lambda& lambda);
    void print_expr(Ast_Macro_Identifier& macro_ident);
    void print_expr(Ast_Tuple_Literal& tuple);
    void print_expr(Ast_Pod_Literal& pod);
    void print_expr(Ast_As_Cast& as_cast);
    void print_expr(Ast_Array_Repeat& array_repeat);
    void print_expr(Ast_Spawn& spawn);
    void print_expr(Ast_Specialized_Identifier& special_ident);
    void print_expr(Ast_Path& path);
    void print_expr(Ast_Switch_Expr& switch_expr);

    void print_switch_cases(std::vector<SwitchCase>& cases);

    DepthUpdate operator -> () {
      return DepthUpdate(this);
    }
};
