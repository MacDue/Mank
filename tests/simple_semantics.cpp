#include "catch/catch.hpp"

/* core */
#include "parser.h"
#include "semantics.h"

TEST_CASE("Calling functions", "[Sema]") {
  using namespace Catch::Matchers;

  SECTION("Calling an existing procedure, with correct args") {
    auto code = Parser::parse_from_string(R"(
      proc fire_nukes_at_target(lat: f64, long: f64) {
        # Complex stuff
      }

      proc main {
        fire_nukes_at_target(53.4808, 2.2426);
      }
    )");

    REQUIRE_NOTHROW(Sema::analyse_file(code));
  }

  SECTION("Calling an existing procedure before it's definded is okay") {
    auto code = Parser::parse_from_string(R"(
      proc main {
        fire_nukes_at_target(53.4808, 2.2426);
      }

      proc fire_nukes_at_target(lat: f64, long: f64) {
        # Complex stuff
      }
    )");

    REQUIRE_NOTHROW(Sema::analyse_file(code));
  }

  SECTION("Calling a function with correct args, and using return value") {
    auto code = Parser::parse_from_string(R"(
      fun is_earth_scheduled_for_demolition: bool {
        return true;
      }

      fun main: i32 {
        if is_earth_scheduled_for_demolition() {
          return 1;
        }
        return 0;
      }
    )");

    REQUIRE_NOTHROW(Sema::analyse_file(code));
  }

  SECTION("Calling function and implicitly discarding the return value should fail") {
    auto code = Parser::parse_from_string(R"(
      proc destroy_earth() {
        # TODO: Stub
      }

      fun is_earth_scheduled_for_demolition: bool {
        return false;
      }

      proc main {
        is_earth_scheduled_for_demolition();
        destroy_earth();
      }
    )");

    REQUIRE_THROWS_WITH(Sema::analyse_file(code), "return value discarded");
  }

  SECTION("Section calling something other than a function should fail") {
    auto code = Parser::parse_from_string(R"(
        proc main {
          # Special hacks!
          (1337 - 42 * 10)();
        }
      )");

    REQUIRE_THROWS_WITH(Sema::analyse_file(code), Contains("not callable"));
  }

  SECTION("Calling something with the wrong number of args should fail") {
    auto code = Parser::parse_from_string(R"(
      proc fire_nukes_at_target(lat: f64, long: f64, only_simulate: bool) {
        # TODO: stub
      }

      proc main {
        # Just test the nuke gun
        fire_nukes_at_target(53.4808, 2.2426);
      }
    )");

    REQUIRE_THROWS_WITH(Sema::analyse_file(code), Contains("expects 3 arguments"));
  }

  SECTION("Calling with the wrong argument types should fail") {
    auto code = Parser::parse_from_string(R"(
      fun endofunctor_in_the_category_of_floats: f64 (monad: f64) {
        return monad * monad;
      }

      fun monad_magic: f64 {
        return endofunctor_in_the_category_of_floats(1337);
      }
    )");

    REQUIRE_THROWS_WITH(Sema::analyse_file(code),
      "expected to be called with Float64 but found Integer");
  }

  SECTION("Calling an undefined function should fail") {
    auto code = Parser::parse_from_string(R"(
      proc main {
        meaning_of_life();
      }
    )");

    REQUIRE_THROWS_WITH(Sema::analyse_file(code), Contains("undefined function"));
  }
}

#define EXTRACT_FIRST_BINARY_EXPR(expected_name) ({                                            \
    auto& fun_function = std::get<Ast_Function_Declaration>(code.functions.at(0)->v);          \
    REQUIRE(fun_function.identifer.name == expected_name);                                     \
    auto& return_stmt = std::get<Ast_Return_Statement>(fun_function.body.statements.at(0)->v); \
    return_stmt.expression;                                                                    \
  })

template <typename T>
void require_constant_binary_expr_value(Ast_File& code, T expected_value) {
  auto binary_expr_ptr = EXTRACT_FIRST_BINARY_EXPR("const_expr");

  auto& binary_expr = std::get<Ast_Binary_Operation>(binary_expr_ptr->v);
    // We don't yet know
  REQUIRE(!binary_expr.is_const_expr());

  // Should be found to be a constant integer value
  REQUIRE_NOTHROW(Sema::analyse_file(code));
  REQUIRE(binary_expr.is_const_expr());
  REQUIRE(std::get<T>(binary_expr.const_expr_value) == expected_value);
}

TEST_CASE("Binary expressions", "[Sema]") {
  using namespace Catch::Matchers;

  SECTION("Simple expressions are resolved to the correct type") {
    auto code = Parser::parse_from_string(R"(
      fun caculate_scarey_maths: i32 (a: i32, b: i32, c: i32) {
        return a * b + c;
      }
    )");

    auto binary_expr = EXTRACT_FIRST_BINARY_EXPR("caculate_scarey_maths");

    // Start with no type
    REQUIRE(binary_expr->type.expired());

    REQUIRE_NOTHROW(Sema::analyse_file(code));

    auto binop_type = extract_type(binary_expr->type);
    REQUIRE(std::holds_alternative<PrimativeType>(binop_type->v));

    // Should be resolved to integer
    auto& primative_type = std::get<PrimativeType>(binop_type->v);
    REQUIRE(primative_type.tag == PrimativeTypeTag::INTEGER);

    // Should not non-constant expression
    REQUIRE(!std::get<Ast_Binary_Operation>(binary_expr->v).is_const_expr());
  }

  SECTION("Constant integer expressions resolve to a value") {
    auto code = Parser::parse_from_string(R"(
      fun const_expr: i32 {
        return 2 + 3 + (1 + -2) * 10;
      }
    )");

    require_constant_binary_expr_value(code, 2 + 3 + (1 + -2) * 10);
  }

  SECTION("Constant float expressions should resolve to a value") {
    auto code = Parser::parse_from_string(R"(
      fun const_expr: f64 {
        return (1.32 * 12.12) - 13.37 / 20. - -100.;
      }
    )");

    require_constant_binary_expr_value(code, (1.32 * 12.12) - 13.37 / 20. - -100.);
  }

  SECTION("Constant boolean expressions should resolve to a value") {
    auto code = Parser::parse_from_string(R"(
      fun const_expr: bool {
        return ((1 + 3) * 100 < (1 - 3) * 1203 + 1);
      }
    )");

    require_constant_binary_expr_value(code, ((1 + 3) * 100 < (1 - 3) * 1203 + 1));
  }

  SECTION("Division by a constant zero is invalid") {

    SECTION("Explicit zero") {
      auto code = Parser::parse_from_string(R"(
        fun black_hole_maths_or_something: i32 {
          return 1213 + 342 - 1 / 0;
        }
      )");

      REQUIRE_THROWS_WITH(Sema::analyse_file(code), "division by zero");
    }

    SECTION("Resolved zero") {
      auto code = Parser::parse_from_string(R"(
        fun i_dont_know_maths: i32 {
          return (1233 + 2323 * 2) / (3232 * 32 - (100 * 3) - 51562 * 2);
        }
      )");

      REQUIRE_THROWS_WITH(Sema::analyse_file(code), "division by zero");
    }
  }

  SECTION("Mixing floats and integers is not allowed implicitly") {
    auto code = Parser::parse_from_string(R"(
      fun but_c_lets_me: f64 (a: i32, b: f64) {
        return a * b - b * b * a;
      }
    )");

    REQUIRE_THROWS_WITH(Sema::analyse_file(code), Contains("incompatible types"));
  }

  SECTION("Mixing integers and booleans is not allowed implicitly") {
    auto code = Parser::parse_from_string(R"(
      fun booleans_are_just_ints_in_c_lol: i32 {
        return (100 < booleans_are_just_ints_in_c_lol()) + 1;
      }
    )");

    REQUIRE_THROWS_WITH(Sema::analyse_file(code), Contains("incompatible types"));
  }

  // TODO: A few more type mixings

  SECTION("Modulo operation are not allowed for floats") {
    auto code = Parser::parse_from_string(R"(
      fun fmod: f64 (a: f64, b: f64) {
        # Maybe I'll allow this later
        return a % b;
      }
    )");

    REQUIRE_THROWS_WITH(Sema::analyse_file(code), Contains("invalid operation"));
  }

  std::array bitwise_operations = { "<<", ">>", "&", "|", "|!" };

  SECTION("Bitwise operations are not allowed for floats") {
    for (auto bitwise_op: bitwise_operations) {
      auto code = Parser::parse_from_string(formatxx::format_string(R"(
        fun bitty_floats: f64 (f: f64) {{
          return f {} 2.0;
        }
      )", bitwise_op));

      REQUIRE_THROWS_WITH(Sema::analyse_file(code), Contains("invalid operation"));
    }
  }

  SECTION("Bitwise operations are not allowed for bools") {
    for (auto bitwise_op: bitwise_operations) {
      auto code = Parser::parse_from_string(formatxx::format_string(R"(
        fun cooly_booly: bool (a: bool, b: bool) {{
          return a {} b;
        }
      )", bitwise_op));

      REQUIRE_THROWS_WITH(Sema::analyse_file(code), Contains("invalid operation"));
    }
  }

  SECTION("Bitwise operations are allowed for integers") {
    for (auto bitwise_op: bitwise_operations) {
      auto code = Parser::parse_from_string(formatxx::format_string(R"(
        fun i_like_trains: i32 (a: i32, b: i32) {{
          return a {} b;
        }
      )", bitwise_op));

      REQUIRE_NOTHROW(Sema::analyse_file(code));
    }
  }
}

TEST_CASE("Unary expressions", "[Sema]") {
  using namespace Catch::Matchers;

  std::array arth_unary_ops {"+", "-"};
  std::array bitwise_unary_ops {"~"};
  std::array logical_unary_ops = {"¬"};

  SECTION("Arithmetic operations are valid for floats and ints") {
    for (auto arth_op: arth_unary_ops) {
      auto code = Parser::parse_from_string(formatxx::format_string(R"(
        fun floaty_mc_float_face: f32 (f: f32) {{
          return {0}f;
        }

        fun integers_are_a_social_construct: i32 (i: i32) {{
          return {0}i;
        }
      )", arth_op));

      REQUIRE_NOTHROW(Sema::analyse_file(code));
    }
  }

  SECTION("Bitwise operations are invalid for booleans and floats") {
    for (auto bitwise_op: bitwise_unary_ops) {
      auto float_code = Parser::parse_from_string(formatxx::format_string(R"(
        fun my_boats_float: f32 (f: f32) {{
          return {}f;
        }
      )", bitwise_op));

      auto bool_code = Parser::parse_from_string(formatxx::format_string(R"(
        fun bools_are_fools: bool(b: bool) {{
          return {}b;
        }
      )", bitwise_op));

      REQUIRE_THROWS_WITH(Sema::analyse_file(float_code),
        Contains("invalid unary operation"));

      REQUIRE_THROWS_WITH(Sema::analyse_file(bool_code),
        Contains("invalid unary operation"));
    }
  }

  SECTION("Logical operations are valid for booleans") {
    for (auto logical_op: logical_unary_ops) {
      auto code = Parser::parse_from_string(formatxx::format_string(R"(
        fun pure_logic_and_reason: bool (b: bool) {{
          return {}b;
        }
      )", logical_op));

      REQUIRE_NOTHROW(Sema::analyse_file(code));
    }
  }

  SECTION("Logical operations are invalid for floats and integers") {
    for (auto logical_op: logical_unary_ops) {
      auto float_code = Parser::parse_from_string(formatxx::format_string(R"(
        fun i_once_had_a_float_that_sat_on_a_moat: f32 (f: f32) {{
          return {}f;
        }
      )", logical_op));

      auto int_code = Parser::parse_from_string(formatxx::format_string(R"(
        fun ints_ints_ints: i32 (i: i32) {{
          return {}i;
        }
      )", logical_op));

      REQUIRE_THROWS_WITH(Sema::analyse_file(float_code),
        Contains("invalid unary operation"));

      REQUIRE_THROWS_WITH(Sema::analyse_file(int_code),
        Contains("invalid unary operation"));
    }
  }
}

TEST_CASE("Expression statements", "[Sema]") {
  // Should be warning in the future
  SECTION("Expressions without side effects are invalid") {
    auto code = Parser::parse_from_string(R"(
      proc haskals_reaction_when {
        # https://i.imgur.com/rXLMONc.jpg
        1 + 2 + 3 + 4 + 5 + 6; # Look ma! No side effects!
      }
    )");

    REQUIRE_THROWS_WITH(Sema::analyse_file(code), "statement has no effect");
  }
}


TEST_CASE("Type names") {
  using namespace Catch::Matchers;

  SECTION("Undeclared return types should fail") {
    auto code = Parser::parse_from_string(R"(
      fun what_is_type_theory: pure_magic {
        # TODO stub
      }
    )");

    REQUIRE_THROWS_WITH(Sema::analyse_file(code), Contains("undeclared return type"));
  }

  SECTION("Undeclared parameter types should fail") {
    auto code = Parser::parse_from_string(R"(
      proc monadic_bind (a: monad) {
        # Get some rope or something?
      }
    )");

    REQUIRE_THROWS_WITH(Sema::analyse_file(code), Contains("undeclared arg type"));
  }
}


TEST_CASE("Local variables") {

  SECTION("Locals should be visable within nested scopes") {
    auto code = Parser::parse_from_string(R"(
      fun clean_code: i32 (a: bool, b: bool, c: bool, d: bool, e: bool, f: bool, g: bool) {
        if a {
          if b {
            if c {
              if d {
                if e {
                  if f {
                    if g {
                      # Each if condtion is a new nested scope
                      return 1337;
                    }
                  }
                }
              }
            }
          }
        }
        return 42;
      }
    )");

    REQUIRE_NOTHROW(Sema::analyse_file(code));
  }

  SECTION("Using undeclared local variables should fail") {
    auto code = Parser::parse_from_string(R"(
      fun the_var_with_no_name: i32 {
        return 1 + n;
      }
    )");

    REQUIRE_THROWS_WITH(Sema::analyse_file(code), "n not declared");
  }
}

TEST_CASE("If statements semantics", "[Sema]") {

  SECTION("If statement condtion must be a boolean") {
    auto code = Parser::parse_from_string(R"(
      proc if_so {
        if "so" {
          # so
        }
      }
    )");

    REQUIRE_THROWS_WITH(Sema::analyse_file(code), "if condition must be a Boolean");
  }
}

TEST_CASE("Function and procedure semantics", "[Sema]") {
  using namespace Catch::Matchers;

  /* Other cases are implicitly tested in other tests */

  SECTION("Returning a type other than the return type is invalid") {
    auto code = Parser::parse_from_string(R"(
      fun a_number_is_a_number: i32 {
        return 10.0;
      }
    )");

    REQUIRE_THROWS_WITH(Sema::analyse_file(code), Contains("invalid return type"));
  }

  SECTION("Returning something in a procedure is invalid") {
    auto code = Parser::parse_from_string(R"(
      proc no {
        return 10;
      }
    )");

    REQUIRE_THROWS_WITH(Sema::analyse_file(code), Contains("expected Void"));
  }
}
