#include <iostream>
#include "catch/catch.hpp"

/* core */
#include "parser.h"
#include "ast_printer.h"

/* testing */
#include "helpers/match_ast.h"
#include "helpers/ast_builder.h"

/*
  wrap procedure start/end

  Used for parsing tests where single statements are being tested, not the procedure.
  (the parser helpers only handle parsing from the top level)

  There's corresponding wrap_stmt/expr functions in the AST builder.
*/
#define WPS "proc test { "
#define WPE "}"

/*
  tests for expression statements/blocks are currently omitted as they
  are implicitly tested in all tests anyway
*/

TEST_CASE("Hello world!", "[Parser]") {
  auto parsed_result = Parser::parse_from_string(R"(
    proc main {
      print("Hello World");
    }
  )");

  auto expected_result = make_file(
    make_procedure("main", make_stmt_body(
      make_expr_stmt(
        make_call("print", make_string("Hello World"))))));

  MATCH_AST(expected_result, parsed_result);
}

TEST_CASE("If statements", "[Parser]") {

  SECTION("if with else") {
    auto parsed_if = Parser::parse_from_string(WPS R"(
      if true {

      } else {

      }
    )" WPE);

    auto expected_if = wrap_final_expr(
      make_if(make_boolean(true), make_stmt_block(), make_stmt_block()));

    MATCH_AST(parsed_if, expected_if);
  }

  SECTION("if without else") {
    auto parsed_if = Parser::parse_from_string(WPS R"(
      if true {

      }
    )" WPE);

    auto expected_if = wrap_final_expr(
      make_if(make_boolean(true), make_stmt_block()));

    MATCH_AST(parsed_if, expected_if);
  }

  SECTION("if with else if") {
    auto parsed_if = Parser::parse_from_string(WPS R"(
      if true {

      } else if false {

      }
    )" WPE);

    auto expected_if = wrap_final_expr(
      make_if(make_boolean(true),
        make_stmt_block(),
        make_if(make_boolean(false), make_stmt_block())));

    MATCH_AST(parsed_if, expected_if);
  }
}

TEST_CASE("Procedures", "[Parser]") {

  SECTION("procedure without any parameters") {
    auto parsed_proc = Parser::parse_from_string(R"(
      proc test {

      }
    )");

    auto expected_proc = make_file(
      make_procedure("test", make_stmt_body()));

    MATCH_AST(parsed_proc, expected_proc);
  }

  SECTION("procedure one argument") {
    auto parsed_proc = Parser::parse_from_string(R"(
      proc test (foo: i32) {

      }
    )");

    auto expected_proc = make_file(
      make_procedure("test", make_args(
      make_argument(make_unchecked_type("i32"), "foo")),
        make_stmt_body()));

    MATCH_AST(parsed_proc, expected_proc);
  }

  SECTION("procedure with multiple arguments") {
    auto parsed_proc = Parser::parse_from_string(R"(
      proc test (foo: i32, bar: bool, baz: float) {

      }
    )");

    auto expected_proc = make_file(
      make_procedure("test", make_args(
      make_argument(make_unchecked_type("i32"), "foo"),
      make_argument(make_unchecked_type("bool"), "bar"),
      make_argument(make_unchecked_type("float"), "baz")),
        make_stmt_body()));

    MATCH_AST(parsed_proc, expected_proc);
  }
}

TEST_CASE("Functions", "[Parser]") {

  SECTION("Function without any parameters") {
    auto parsed_fun_without_extra_parens = Parser::parse_from_string(R"(
      fun meaning_of_life: i32 {
        return 42;
      }
    )");

    auto parsed_fun_with_extra_parens = Parser::parse_from_string(R"(
      fun meaning_of_life: i32 () {
        return 42;
      }
    )");

    auto expected_fun = make_file(
      make_function(make_unchecked_type("i32"), "meaning_of_life",
      make_stmt_body(make_return(make_integer(42)))));

    MATCH_AST(parsed_fun_with_extra_parens, parsed_fun_without_extra_parens);
    MATCH_AST(parsed_fun_without_extra_parens, expected_fun);
  }

  SECTION("Function with parameters") {
    auto parsed_fun = Parser::parse_from_string(R"(
      # I'm sure floats will get us close enough :)
      fun launch_nukes: bool (lat: float, long: float) {
        return true;
      }
    )");

    auto expected_fun = make_file(
      make_function(make_unchecked_type("bool"), "launch_nukes", make_args(
      make_argument(make_unchecked_type("float"), "lat"),
      make_argument(make_unchecked_type("float"), "long")),
        make_stmt_body(make_return(make_boolean(true)))));

    MATCH_AST(expected_fun, parsed_fun);
  }
}

TEST_CASE("Expressions", "[Parser]") {

  SECTION("Calls") {
    auto parsed_call = Parser::parse_from_string(WPS R"(
      launch_nukes(53.480800, 2.242600);
    )" WPE);

    auto expected_call = wrap_expr(
      make_call("launch_nukes",
        make_float64(53.480800), make_float64(2.242600)));

    MATCH_AST(expected_call, parsed_call);
  }

  SECTION("Literals") {
    auto parsed_literals = Parser::parse_from_string(WPS R"(
      "What is a computer?";
      1337;
      1.323000;
      false;
      true;
    )" WPE);

    auto expected_literals = make_file(make_procedure("test", make_stmt_body(
      make_expr_stmt(make_string("What is a computer?")),
      make_expr_stmt(make_integer(1337)),
      make_expr_stmt(make_float64(1.323000)),
      make_expr_stmt(make_boolean(false)),
      make_expr_stmt(make_boolean(true)))));

    MATCH_AST(expected_literals, parsed_literals);
  }

  SECTION("Identifiers") {
    auto parsed_idents = Parser::parse_from_string(WPS R"(
      fooBar;
      foo_bar;
      i_1;
      _foo;
    )" WPE);

    auto expected_idents = make_file(make_procedure("test", make_stmt_body(
      make_expr_stmt(make_ident("fooBar")),
      make_expr_stmt(make_ident("foo_bar")),
      make_expr_stmt(make_ident("i_1")),
      make_expr_stmt(make_ident("_foo")))));

    MATCH_AST(expected_idents, parsed_idents);
  }

  SECTION("Unary operations") {
    auto parsed_unaries = Parser::parse_from_string(WPS R"(
      +1;
      -1;
      ¬true;
    )" WPE);

    auto expected_unaries = make_file(make_procedure("test", make_stmt_body(
      make_expr_stmt(make_unary(Ast_Operator::PLUS, make_integer(1))),
      make_expr_stmt(make_unary(Ast_Operator::MINUS, make_integer(1))),
      make_expr_stmt(make_unary(Ast_Operator::LOGICAL_NOT, make_boolean(true))))));

    MATCH_AST(expected_unaries, parsed_unaries);
  }

  SECTION("Binary operations") {

    SECTION("Simple operations") {
      auto parsed_binaries = Parser::parse_from_string(WPS R"(
        1 + 2;
        1 - 2;
        1 % 3;
      )" WPE);

      auto expected_binaries = make_file(make_procedure("test", make_stmt_body(
        make_expr_stmt(make_binary(Ast_Operator::PLUS,
          make_integer(1), make_integer(2))),
        make_expr_stmt(make_binary(Ast_Operator::MINUS,
          make_integer(1), make_integer(2))),
        make_expr_stmt(make_binary(Ast_Operator::MODULO,
          make_integer(1), make_integer(3))))));

      MATCH_AST(expected_binaries, parsed_binaries);
    }

    /* no right associative operations yet */
    SECTION("Left associativity") {
      auto parsed_binary = Parser::parse_from_string(
        WPS "1 + 2 + 3 + 4;" WPE);

      auto expected_binary = wrap_expr(
        make_binary(Ast_Operator::PLUS,
          make_binary(Ast_Operator::PLUS,
            make_binary(Ast_Operator::PLUS,
              make_integer(1), make_integer(2)),
            make_integer(3)),
          make_integer(4)));

      MATCH_AST(expected_binary, parsed_binary);
    }

    SECTION("Operator precedence") {
      auto parsed_binary = Parser::parse_from_string(
        WPS "1 * 2 + 3 - 4 + 5 * 6;" WPE);

      // I've implemented LISP....
      auto expected_binary = wrap_expr(
        make_binary(Ast_Operator::PLUS,
          make_binary(Ast_Operator::MINUS,
            make_binary(Ast_Operator::PLUS,
              make_binary(Ast_Operator::TIMES,
                make_integer(1), make_integer(2)),
              make_integer(3)),
            make_integer(4)),
          make_binary(Ast_Operator::TIMES,
            make_integer(5), make_integer(6))));

      MATCH_AST(expected_binary, parsed_binary);
    }

    SECTION("Expressions with parentheses") {
      auto parsed_binary = Parser::parse_from_string(
        WPS "1 * (2 + 3) - (4 + 5) * 6;" WPE);

      auto expected_binary = wrap_expr(
        make_binary(Ast_Operator::MINUS,
          make_binary(Ast_Operator::TIMES,
            make_integer(1),
            make_binary(Ast_Operator::PLUS,
              make_integer(2), make_integer(3))),
          make_binary(Ast_Operator::TIMES,
            make_binary(Ast_Operator::PLUS,
              make_integer(4), make_integer(5)),
            make_integer(6))));

      MATCH_AST(expected_binary, parsed_binary);
    }
  }
}

TEST_CASE("Block expressions", "[Parser]") {

  SECTION("Blocks with only a final expression") {
    auto parsed_block = Parser::parse_from_string(
      WPS "{ 1337 }" WPE);

    auto expected_block = wrap_final_expr(
      make_block(true,
        make_expr_stmt(make_integer(1337))));

    MATCH_AST(parsed_block, expected_block);
  }

  SECTION("Block with brace terminated expressions") {
    auto parsed_block = Parser::parse_from_string(WPS R"(
      {
        if true { 10 } else { 20 }  # Brace terminated expr
      }
    )" WPE);

    auto expected_block = wrap_final_expr(
      make_block(true,
        make_if_stmt(make_boolean(true),
          make_block(true, make_expr_stmt(make_integer(10))),
          make_block(true, make_expr_stmt(make_integer(20))))));

    MATCH_AST(parsed_block, expected_block);
  }

  SECTION("Block mixing brace terminated and semicolon terminated expressions") {
    auto parsed_block = Parser::parse_from_string(WPS R"(
      {
        if true { 1 } else { 0 }
        return 20;
      }
    )" WPE);

    auto expected_block = wrap_final_expr(
      make_stmt_block(
        make_if_stmt(make_boolean(true),
          make_block(true, make_expr_stmt(make_integer(1))),
          make_block(true, make_expr_stmt(make_integer(0)))),
        make_return(make_integer(20))));

    MATCH_AST(parsed_block, expected_block);
  }

  SECTION("Block with brace terminated expression before final expression") {
    auto parsed_block = Parser::parse_from_string(WPS R"(
      {
        { }
        42.000000
      }
    )" WPE);

    auto expected_block = wrap_final_expr(
      make_block(true,
        make_expr_stmt(make_stmt_block()),
        make_expr_stmt(make_float64(42.000000))));

    MATCH_AST(parsed_block, expected_block);
  }
}

TEST_CASE("Variable declarations", "[Parser]") {

  SECTION("Declaration with type and no initializer") {
    auto parsed_decl = Parser::parse_from_string(
      WPS "foo: i32;" WPE);

    auto expected_decl = wrap_stmt(
      make_var_decl("foo", make_type("i32")));

    MATCH_AST(parsed_decl, expected_decl);
  }

  SECTION("Declaration with type and initializer") {
    auto parsed_decl = Parser::parse_from_string(
      WPS "foo: i32 = 100;" WPE);

    auto expected_decl = wrap_stmt(
      make_var_decl("foo", make_type("i32"), make_integer(100)));

    MATCH_AST(parsed_decl, expected_decl);
  }

  SECTION("Declaration with initializer and no type") {
    auto parsed_decl = Parser::parse_from_string(
      WPS "foo := 100;" WPE);

    auto expected_decl = wrap_stmt(
      make_var_decl("foo", make_integer(100)));

    MATCH_AST(parsed_decl, expected_decl);
  }

  /* The following still parses though is invalid semantically (currently) */

  SECTION("Declaration without type or initializer") {
    auto parsed_decl = Parser::parse_from_string(
      WPS "foo:;" WPE);

    auto expected_decl = wrap_stmt(
      make_var_decl("foo"));

    MATCH_AST(parsed_decl, expected_decl);
  }
}

TEST_CASE("Assignment statements", "[Parser]") {

  SECTION("Simple assign") {
    // We don't have much more than simple (as assign expressions are a bad idea)
    auto parsed_assign = Parser::parse_from_string(
      WPS "foo = 100;" WPE);

    auto expected_assign = wrap_stmt(
      make_assignment(make_ident("foo"), make_integer(100)));

    MATCH_AST(parsed_assign, expected_assign);
  }

  /* Invalid but parsable assign (may be used in the future?) */
  SECTION("Assign to expression") {
    auto parsed_assign = Parser::parse_from_string(
      WPS "1 + 2 = 100;" WPE);

    auto expected_assign = wrap_stmt(
      make_assignment(
          make_binary(Ast_Operator::PLUS,
            make_integer(1), make_integer(2)),
          make_integer(100)));

    MATCH_AST(parsed_assign, expected_assign);
  }
}
