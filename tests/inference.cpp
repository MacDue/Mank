/* core */
#include "parser/parser.h"
#include "sema/semantics.h"

#undef WHEN
#include "catch/catch.hpp"

TEST_CASE("Inferring lambda types passed to functions", "[Infer]") {
  // This is an easy case where all the information for the lambda type
  // is directly matched against the unknown lambda type

  Semantics sema;

  auto code = Parser::parse_from_string(R"(
    proc apply (array: ref i32[5], func: \i32 -> i32) {
      for i in 0 .. array.length {
        array[i] = func(array[i]);
      }
    }

    proc test {
      a := [1, 2, 3, 4, 5];
      apply(a, \x -> { x * 2 });
    }
  )");

  REQUIRE_NOTHROW(sema.analyse_file(code));
  auto proc_test = *code.functions.at(1);

  // apply(a, \x -> { x * 2 })
  auto apply_call = std::get<Ast_Call>(
    std::get<Ast_Expression_Statement>(proc_test.body.statements.at(1)->v).expression->v);

  // \x -> { x * 2 }
  auto passed_lambda = apply_call.arguments.at(1);
  auto infered_lambda_type = std::get<LambdaType>(passed_lambda->meta.type->v);

  // Lambda should be \i32 -> i32
  REQUIRE(infered_lambda_type.argument_types.size() == 1);
  REQUIRE(match_types(infered_lambda_type.return_type, PrimativeType::get(PrimativeType::INTEGER)));
  REQUIRE(match_types(infered_lambda_type.argument_types.at(0), PrimativeType::get(PrimativeType::INTEGER)));
}

TEST_CASE("Inferring types of local lambdas from usage", "[Infer]") {
  Semantics sema;
  auto code = Parser::parse_from_string(R"(
    proc test {
      # takes a lambda 'y' can passes y another lambda that takes p and t and adds + returns them
      x := \y -> { y(\p,t -> { p + t }) }
      # call x with with a lambda that takes a lambda z and calls it with 1 and 3 and returns the result
      j := x(\z -> { z(1, 3) });
    }
  )");

  REQUIRE_NOTHROW(sema.analyse_file(code));

  /*
    The solution must be:
                    v- the lambda passed to y
    lambda (lambda (lambda i32, i32 -> i32) -> i32) -> i32
            ^- this lambda is y                 ^ result of y (solved from returning the value of the lambda passed to x)
  */

  // lambda param passed to y
  LambdaType y_param;
  y_param.return_type = PrimativeType::get(PrimativeType::INTEGER);
  y_param.argument_types = {
    PrimativeType::get(PrimativeType::INTEGER),
    PrimativeType::get(PrimativeType::INTEGER) };

  LambdaType y_type;
  y_type.return_type = PrimativeType::get(PrimativeType::INTEGER);
  y_type.argument_types = { code.ctx.new_type(y_param) };

  LambdaType x_type;
  x_type.return_type = PrimativeType::get(PrimativeType::INTEGER);
  x_type.argument_types = { code.ctx.new_type(y_type) };

  auto proc_test = *code.functions.at(0);
  auto x_decl = std::get<Ast_Variable_Declaration>(proc_test.body.statements.at(0)->v);

  REQUIRE(match_types(x_decl.type, code.ctx.new_type(x_type)));
}

TEST_CASE("Inference enforces special constraints on operations", "[Infer]") {
  using namespace Catch::Matchers;
  Semantics sema;
  SECTION("Two unknown types inferred to be bools can't be used with +") {
    auto code = Parser::parse_from_string(R"(
      proc test {
        # Do thing takes to unknown types and adds them
        # It should enforce a special constraint that x and y are numeric types
        # (e.g. an integer for floating point type)
        do_thing := \x,y -> { x + y }
        # The inference should now work -- then fail when validated against the
        # addtional constraints on the valid types
        res := do_thing(true, false);
      }
    )");

    // code.functions.at(0)

    sema.disable_type_inference_for_testing(); // allows running unify later
    REQUIRE_NOTHROW(sema.analyse_file(code)); // no type inference yet
    // Try to infer types
    auto& infer = sema.get_infer_for_testing();
    REQUIRE_THROWS_WITH(infer.unify_and_apply(),
      "was expecting a Numeric type but inferred Boolean");
  }
}

TEST_CASE("Inferred void lambda returns", "[Infer]") {
  using namespace Catch::Matchers;
  Semantics sema;
  SECTION("A void return is fine as long as that return is not used as value") {
    auto code = Parser::parse_from_string(R"(
      proc test {
        func := \ -> {
          # do something...
        }

        other_func := \ -> {
          return; # explict return
        }
      }
    )");

    REQUIRE_NOTHROW(sema.analyse_file(code));
    auto proc_test = *code.functions.at(0);

    auto func_decl = std::get<Ast_Variable_Declaration>(proc_test.body.statements.at(0)->v);
    REQUIRE(std::get<LambdaType>(func_decl.type->v).return_type->is_void());

    auto other_func_decl = std::get<Ast_Variable_Declaration>(proc_test.body.statements.at(1)->v);
    REQUIRE(std::get<LambdaType>(other_func_decl.type->v).return_type->is_void());
  }

  SECTION("An infered void return is still allowed if the lambda's return is used") {
    auto code = Parser::parse_from_string(R"(
      proc test {
        func := \ -> {};
        a := func();
      }
    )");
    REQUIRE_NOTHROW(sema.analyse_file(code));
  }

  SECTION("A return tvar cannot be switched to null if bound to another tvar") {
    auto code = Parser::parse_from_string(R"(
      proc test {
        func := \x -> {
          if x > 2 {
            return x;   # return x which is a tvar (unknown type)
                        # this binds the return type (which is also a tvar to x's tvar)
          } else {
            return;     # Void return (if this were it it would bind the return tvar to void)
          }
        }

        a := func(1);
      }
    )");

    // After the return is bound to x it the lambda then must return a value (as x can't be void)
    REQUIRE_THROWS_WITH(sema.analyse_file(code), Contains("cannot bind expression with type () to Integer"));
  }
}

TEST_CASE("Incomplete substitution", "[Infer]") {
  using namespace Catch::Matchers;
  Semantics sema;
  SECTION("Lambda that takes some unknown params and never uses/binds them") {
    auto code = Parser::parse_from_string(R"(
      proc test {
        a := \x -> {}
      }
    )");

    REQUIRE_THROWS_WITH(sema.analyse_file(code), Contains("type unknown!"));
  }

  SECTION("Same but within a nested lambda") {
    auto code = Parser::parse_from_string(R"(
      proc test {
        a := \y -> {
          \x -> {}
        }
        b := a(10);
      }
    )");

    REQUIRE_THROWS_WITH(sema.analyse_file(code), Contains("type unknown!"));
  }
}

TEST_CASE("Field constraints", "[Infer]") {
  Semantics sema;
  SECTION("Inferring with fields -- one level") {
    auto code = Parser::parse_from_string(R"(
      pod Foo {
        bar: i32
      }

      proc test {
        a := \ -> {
          foo:Foo;
          foo # Binds T0 (return) to Foo
        }
        my_foo := a(); # my_foo = T0

        bar := my_foo.bar; # i32 (T0[.bar])
      }
    )");

    REQUIRE_NOTHROW(sema.analyse_file(code));

    auto& proc_test = *code.functions.at(0);
    auto& my_foo_decl = std::get<Ast_Variable_Declaration>(proc_test.body.statements.at(1)->v);
    auto& bar_decl = std::get<Ast_Variable_Declaration>(proc_test.body.statements.at(2)->v);

    // my_foo is a Foo
    REQUIRE("Foo" ==
      std::get<Ast_Pod_Declaration>(my_foo_decl.type->v).identifier.name);

    // bar is an int
    REQUIRE(match_types(bar_decl.type, PrimativeType::get(PrimativeType::INTEGER)));
  }

  SECTION("Inferring with fields -- nested") {
    auto code = Parser::parse_from_string(R"(
      pod Foo {
        bar: Bar
      }

      pod Bar {
        baz: i32
      }

      proc test {
        a := \ -> {
          foo:Foo;
          foo.bar.baz = 666;
          foo
        }

        my_foo := a(); # my_foo = T0
        baz := my_foo.bar.baz; # T0[.bar] = T1, T1[.baz] = T2
      }
    )");

    REQUIRE_NOTHROW(sema.analyse_file(code));

    auto& proc_test = *code.functions.at(0);
    auto& my_foo_decl = std::get<Ast_Variable_Declaration>(proc_test.body.statements.at(1)->v);
    auto& baz_decl = std::get<Ast_Variable_Declaration>(proc_test.body.statements.at(2)->v);

    REQUIRE("Foo" ==
      std::get<Ast_Pod_Declaration>(my_foo_decl.type->v).identifier.name);

    REQUIRE(match_types(baz_decl.type, PrimativeType::get(PrimativeType::INTEGER)));
  }

  SECTION("It's invalid to infer a pod/field earlier than it's use") {
    auto code = Parser::parse_from_string(R"(
      pod Foo {
        bar: i32
      }

      proc test {
        a := \x -> { x.bar } # requires infering Foo type earlier in code
        my_foo:Foo;
        bar := a(my_foo);
      }
    )");

    REQUIRE_THROWS_WITH(sema.analyse_file(code), "not a pod type");
  }
}
