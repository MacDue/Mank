#include <iostream>
#include "catch/catch.hpp"

/* core */
#include "parser/parser.h"
#include "ast/ast_builder.h"
#include "ast/ast_printer.h"

/* testing */
#include "helpers/match_ast.h"

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

#define NEW_FILE(name)                  \
  auto name = AstBuilder::new_file();   \
  auto f = AstBuilder(name);

TEST_CASE("Hello world!", "[Parser]") {
  auto parsed_result = Parser::parse_from_string(R"(
    proc main {
      print("Hello World");
    }
  )");

  NEW_FILE(expected_result);
  f.add_functions(
    f.make_procedure("main", f.make_stmt_body(
      f.make_expr_stmt(
        f.make_call("print", f.make_string("Hello World"))))));

  MATCH_AST(expected_result, parsed_result);
}

TEST_CASE("If statements", "[Parser]") {

  SECTION("if with else") {
    auto parsed_if = Parser::parse_from_string(WPS R"(
      if true {

      } else {

      }
    )" WPE);

    NEW_FILE(file);
    auto& expected_if = f.wrap_final_expr(
      f.make_if(
        f.make_boolean(true),
          f.make_stmt_block(),
          f.make_stmt_block()));

    MATCH_AST(parsed_if, expected_if);
  }

  SECTION("if without else") {
    auto parsed_if = Parser::parse_from_string(WPS R"(
      if true {

      }
    )" WPE);

    NEW_FILE(file);
    auto& expected_if = f.wrap_final_expr(
      f.make_if(
        f.make_boolean(true),
        f.make_stmt_block()));

    MATCH_AST(parsed_if, expected_if);
  }

  SECTION("if with else if") {
    auto parsed_if = Parser::parse_from_string(WPS R"(
      if true {

      } else if false {

      }
    )" WPE);

    NEW_FILE(file);
    auto& expected_if = f.wrap_final_expr(
      f.make_if(f.make_boolean(true),
        f.make_stmt_block(),
        f.make_if(f.make_boolean(false), f.make_stmt_block())));

    MATCH_AST(parsed_if, expected_if);
  }
}

TEST_CASE("Procedures", "[Parser]") {

  SECTION("procedure without any parameters") {
    auto parsed_proc = Parser::parse_from_string(R"(
      proc test {

      }
    )");

    NEW_FILE(file);
    auto& expected_proc = f.add_functions(
      f.make_procedure("test", f.make_stmt_body()));

    MATCH_AST(parsed_proc, expected_proc);
  }

  SECTION("procedure one argument") {
    auto parsed_proc = Parser::parse_from_string(R"(
      proc test (foo: i32) {

      }
    )");

    NEW_FILE(file);
    auto& expected_proc = f.add_functions(
      f.make_procedure("test", f.make_args(
      f.make_argument(f.make_unchecked_type("i32"), "foo")),
        f.make_stmt_body()));

    MATCH_AST(parsed_proc, expected_proc);
  }

  SECTION("procedure with multiple arguments") {
    auto parsed_proc = Parser::parse_from_string(R"(
      proc test (foo: i32, bar: bool, baz: float) {

      }
    )");

    NEW_FILE(file);
    auto& expected_proc = f.add_functions(
      f.make_procedure("test", f.make_args(
      f.make_argument(f.make_unchecked_type("i32"), "foo"),
      f.make_argument(f.make_unchecked_type("bool"), "bar"),
      f.make_argument(f.make_unchecked_type("float"), "baz")),
        f.make_stmt_body()));

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

    NEW_FILE(file);
    auto& expected_fun = f.add_functions(
      f.make_function(f.make_unchecked_type("i32"), "meaning_of_life",
      f.make_stmt_body(f.make_return(f.make_integer(42)))));

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

    NEW_FILE(file);
    auto& expected_fun = f.add_functions(
      f.make_function(f.make_unchecked_type("bool"), "launch_nukes", f.make_args(
      f.make_argument(f.make_unchecked_type("float"), "lat"),
      f.make_argument(f.make_unchecked_type("float"), "long")),
        f.make_stmt_body(f.make_return(f.make_boolean(true)))));

    MATCH_AST(expected_fun, parsed_fun);
  }
}

TEST_CASE("Expressions", "[Parser]") {

  SECTION("Calls") {
    auto parsed_call = Parser::parse_from_string(WPS R"(
      launch_nukes(53.480800, 2.242600);
    )" WPE);

    NEW_FILE(file);
    auto& expected_call = f.wrap_expr(
      f.make_call("launch_nukes",
        f.make_float64(53.480800), f.make_float64(2.242600)));

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

    NEW_FILE(file);
    auto& expected_literals = f.add_functions(f.make_procedure("test", f.make_stmt_body(
      f.make_expr_stmt(f.make_string("What is a computer?")),
      f.make_expr_stmt(f.make_integer(1337)),
      f.make_expr_stmt(f.make_float64(1.323000)),
      f.make_expr_stmt(f.make_boolean(false)),
      f.make_expr_stmt(f.make_boolean(true)))));

    MATCH_AST(expected_literals, parsed_literals);
  }

  SECTION("Identifiers") {
    auto parsed_idents = Parser::parse_from_string(WPS R"(
      fooBar;
      foo_bar;
      i_1;
      _foo;
    )" WPE);

    NEW_FILE(file);
    auto& expected_idents = f.add_functions(f.make_procedure("test", f.make_stmt_body(
      f.make_expr_stmt(f.make_ident("fooBar")),
      f.make_expr_stmt(f.make_ident("foo_bar")),
      f.make_expr_stmt(f.make_ident("i_1")),
      f.make_expr_stmt(f.make_ident("_foo")))));

    MATCH_AST(expected_idents, parsed_idents);
  }

  SECTION("Unary operations") {
    auto parsed_unaries = Parser::parse_from_string(WPS R"(
      +1;
      -1;
      ¬true;
    )" WPE);

    NEW_FILE(file);
    auto& expected_unaries = f.add_functions(f.make_procedure("test", f.make_stmt_body(
      f.make_expr_stmt(f.make_unary(Ast_Operator::PLUS, f.make_integer(1))),
      f.make_expr_stmt(f.make_unary(Ast_Operator::MINUS, f.make_integer(1))),
      f.make_expr_stmt(f.make_unary(Ast_Operator::LOGICAL_NOT, f.make_boolean(true))))));

    MATCH_AST(expected_unaries, parsed_unaries);
  }

  SECTION("Binary operations") {

    SECTION("Simple operations") {
      auto parsed_binaries = Parser::parse_from_string(WPS R"(
        1 + 2;
        1 - 2;
        1 % 3;
      )" WPE);

      NEW_FILE(file);
      auto& expected_binaries = f.add_functions(f.make_procedure("test", f.make_stmt_body(
        f.make_expr_stmt(f.make_binary(Ast_Operator::PLUS,
          f.make_integer(1), f.make_integer(2))),
        f.make_expr_stmt(f.make_binary(Ast_Operator::MINUS,
          f.make_integer(1), f.make_integer(2))),
        f.make_expr_stmt(f.make_binary(Ast_Operator::MODULO,
          f.make_integer(1), f.make_integer(3))))));

      MATCH_AST(expected_binaries, parsed_binaries);
    }

    /* no right associative operations yet */
    SECTION("Left associativity") {
      auto parsed_binary = Parser::parse_from_string(
        WPS "1 + 2 + 3 + 4;" WPE);

      NEW_FILE(file);
      auto& expected_binary = f.wrap_expr(
        f.make_binary(Ast_Operator::PLUS,
          f.make_binary(Ast_Operator::PLUS,
            f.make_binary(Ast_Operator::PLUS,
              f.make_integer(1), f.make_integer(2)),
            f.make_integer(3)),
          f.make_integer(4)));

      MATCH_AST(expected_binary, parsed_binary);
    }

    SECTION("Operator precedence") {
      auto parsed_binary = Parser::parse_from_string(
        WPS "1 * 2 + 3 - 4 + 5 * 6;" WPE);

      // I've implemented LISP....
      NEW_FILE(file);
      auto& expected_binary = f.wrap_expr(
        f.make_binary(Ast_Operator::PLUS,
          f.make_binary(Ast_Operator::MINUS,
            f.make_binary(Ast_Operator::PLUS,
              f.make_binary(Ast_Operator::TIMES,
                f.make_integer(1), f.make_integer(2)),
              f.make_integer(3)),
            f.make_integer(4)),
          f.make_binary(Ast_Operator::TIMES,
            f.make_integer(5), f.make_integer(6))));

      MATCH_AST(expected_binary, parsed_binary);
    }

    SECTION("Expressions with parentheses") {
      auto parsed_binary = Parser::parse_from_string(
        WPS "1 * (2 + 3) - (4 + 5) * 6;" WPE);

      NEW_FILE(file);
      auto& expected_binary = f.wrap_expr(
        f.make_binary(Ast_Operator::MINUS,
          f.make_binary(Ast_Operator::TIMES,
            f.make_integer(1),
            f.make_binary(Ast_Operator::PLUS,
              f.make_integer(2), f.make_integer(3))),
          f.make_binary(Ast_Operator::TIMES,
            f.make_binary(Ast_Operator::PLUS,
              f.make_integer(4), f.make_integer(5)),
            f.make_integer(6))));

      MATCH_AST(expected_binary, parsed_binary);
    }
  }
}

TEST_CASE("Block expressions", "[Parser]") {

  SECTION("Blocks with only a final expression") {
    auto parsed_block = Parser::parse_from_string(
      WPS "{ 1337 }" WPE);

    NEW_FILE(file);
    auto& expected_block = f.wrap_final_expr(
      f.make_block(true,
        f.make_expr_stmt(f.make_integer(1337))));

    MATCH_AST(parsed_block, expected_block);
  }

  SECTION("Block with brace terminated expressions") {
    auto parsed_block = Parser::parse_from_string(WPS R"(
      {
        if true { 10 } else { 20 }  # Brace terminated expr
      }
    )" WPE);

    NEW_FILE(file);
    auto& expected_block = f.wrap_final_expr(
      f.make_block(true,
        f.make_if_stmt(f.make_boolean(true),
          f.make_block(true, f.make_expr_stmt(f.make_integer(10))),
          f.make_block(true, f.make_expr_stmt(f.make_integer(20))))));

    MATCH_AST(parsed_block, expected_block);
  }

  SECTION("Block mixing brace terminated and semicolon terminated expressions") {
    auto parsed_block = Parser::parse_from_string(WPS R"(
      {
        if true { 1 } else { 0 }
        return 20;
      }
    )" WPE);

    NEW_FILE(file);
    auto& expected_block = f.wrap_final_expr(
      f.make_stmt_block(
        f.make_if_stmt(f.make_boolean(true),
          f.make_block(true, f.make_expr_stmt(f.make_integer(1))),
          f.make_block(true, f.make_expr_stmt(f.make_integer(0)))),
        f.make_return(f.make_integer(20))));

    MATCH_AST(parsed_block, expected_block);
  }

  SECTION("Block with brace terminated expression before final expression") {
    auto parsed_block = Parser::parse_from_string(WPS R"(
      {
        { }
        42.000000
      }
    )" WPE);

    NEW_FILE(file);
    auto& expected_block = f.wrap_final_expr(
      f.make_block(true,
        f.make_expr_stmt(f.make_stmt_block()),
        f.make_expr_stmt(f.make_float64(42.000000))));

    MATCH_AST(parsed_block, expected_block);
  }
}

TEST_CASE("Variable declarations", "[Parser]") {

  SECTION("Declaration with type and no initializer") {
    auto parsed_decl = Parser::parse_from_string(
      WPS "foo: i32;" WPE);

    NEW_FILE(file);
    auto& expected_decl = f.wrap_stmt(
      f.make_var_decl("foo", f.make_type("i32")));

    MATCH_AST(parsed_decl, expected_decl);
  }

  SECTION("Declaration with type and initializer") {
    auto parsed_decl = Parser::parse_from_string(
      WPS "foo: i32 = 100;" WPE);

    NEW_FILE(file);
    auto& expected_decl = f.wrap_stmt(
      f.make_var_decl("foo", f.make_type("i32"), f.make_integer(100)));

    MATCH_AST(parsed_decl, expected_decl);
  }

  SECTION("Declaration with initializer and no type") {
    auto parsed_decl = Parser::parse_from_string(
      WPS "foo := 100;" WPE);

    NEW_FILE(file);
    auto& expected_decl = f.wrap_stmt(
      f.make_var_decl("foo", f.make_integer(100)));

    MATCH_AST(parsed_decl, expected_decl);
  }

  /* The following still parses though is invalid semantically (currently) */

  SECTION("Declaration without type or initializer") {
    auto parsed_decl = Parser::parse_from_string(
      WPS "foo:;" WPE);

    NEW_FILE(file);
    auto& expected_decl = f.wrap_stmt(
      f.make_var_decl("foo"));

    MATCH_AST(parsed_decl, expected_decl);
  }
}

TEST_CASE("Assignment statements", "[Parser]") {

  SECTION("Simple assign") {
    // We don't have much more than simple (as assign expressions are a bad idea)
    auto parsed_assign = Parser::parse_from_string(
      WPS "foo = 100;" WPE);

    NEW_FILE(file);
    auto& expected_assign = f.wrap_stmt(
      f.make_assignment(f.make_ident("foo"), f.make_integer(100)));

    MATCH_AST(parsed_assign, expected_assign);
  }

  /* Invalid but parsable assign (may be used in the future?) */
  SECTION("Assign to expression") {
    auto parsed_assign = Parser::parse_from_string(
      WPS "1 + 2 = 100;" WPE);

    NEW_FILE(file);
    auto& expected_assign = f.wrap_stmt(
      f.make_assignment(
          f.make_binary(Ast_Operator::PLUS,
            f.make_integer(1), f.make_integer(2)),
          f.make_integer(100)));

    MATCH_AST(parsed_assign, expected_assign);
  }
}

TEST_CASE("For loops", "[Parser]") {

  SECTION("For loop with loop value type") {
    auto parsed_for = Parser::parse_from_string(WPS R"(
      for x: i32 in 0 .. 10 {

      }
    )" WPE);

    NEW_FILE(file);
    auto& expected_for = f.wrap_stmt(
      f.make_for("x", f.make_type("i32"), f.make_integer(0), f.make_integer(10),
        f.make_stmt_body()));

    MATCH_AST(parsed_for, expected_for);
  }

  SECTION("For loop without loop value type") {
    auto parsed_for = Parser::parse_from_string(WPS R"(
      for x in 0 .. 10 {

      }
    )" WPE);

    NEW_FILE(file);
    auto& expected_for = f.wrap_stmt(
      f.make_for("x", f.make_integer(0), f.make_integer(10),
       f.make_stmt_body()));

    MATCH_AST(parsed_for, expected_for);
  }
}

TEST_CASE("Pods", "[Parser]") {
  auto parsed_pods = Parser::parse_from_string(R"(
    pod Apple {
      a: i32,
      b: f64,
      c: bool
    }

    pod Empty {}

    pod One { a: bool }
  )");

  NEW_FILE(expected_pods);
  expected_pods.pods.emplace_back(
    f.make_pod("Apple",
      f.make_argument(
        f.make_unchecked_type("i32"), "a"),
      f.make_argument(
        f.make_unchecked_type("f64"), "b"),
      f.make_argument(
        f.make_unchecked_type("bool"), "c")));
  expected_pods.pods.emplace_back(
    f.make_pod("Empty"));
  expected_pods.pods.emplace_back(
    f.make_pod("One",
      f.make_argument(f.make_unchecked_type("bool"), "a")));

  MATCH_AST(parsed_pods, expected_pods);
}

TEST_CASE("Field access", "[Parser]") {
  SECTION("Single level access") {
    auto parsed_access = Parser::parse_from_string(
      WPS "foo.bar;" WPE);

    NEW_FILE(file);
    auto& expected_access = f.wrap_expr(
      f.make_access(f.make_ident("foo"), "bar"));

    MATCH_AST(parsed_access, expected_access);
  }

  SECTION("Nested access") {
    auto parsed_access = Parser::parse_from_string(
      WPS "cs.manchester.ac.uk;" WPE);

    NEW_FILE(file);
    auto& expected_access = f.wrap_expr(
      f.make_access(
        f.make_access(
          f.make_access(
            f.make_ident("cs"), "manchester"), "ac"), "uk"));

    MATCH_AST(parsed_access, expected_access);
  }
}
