/* core */
#include "parser/parser.h"
#include "sema/semantics.h"

#undef WHEN
#include "catch/catch.hpp"

#include "helpers/warning_matchers.h"

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

    REQUIRE_NOTHROW(Semantics().analyse_file(code));
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

    REQUIRE_NOTHROW(Semantics().analyse_file(code));
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

    REQUIRE_NOTHROW(Semantics().analyse_file(code));
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

    REQUIRE_THROWS_WITH(Semantics().analyse_file(code), "return value discarded");
  }

  SECTION("Section calling something other than a function should fail") {
    auto code = Parser::parse_from_string(R"(
        proc main {
          # Special hacks!
          (1337 - 42 * 10)();
        }
      )");

    REQUIRE_THROWS_WITH(Semantics().analyse_file(code), Contains("not callable"));
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

    REQUIRE_THROWS_WITH(Semantics().analyse_file(code), Contains("expects 3 arguments"));
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

    REQUIRE_THROWS_WITH(Semantics().analyse_file(code),
      "cannot bind expression with type Integer32 to Float64");
  }

  SECTION("Calling an undefined function should fail") {
    auto code = Parser::parse_from_string(R"(
      proc main {
        meaning_of_life();
      }
    )");

    REQUIRE_THROWS_WITH(Semantics().analyse_file(code), Contains("undefined function"));
  }
}

#define EXTRACT_FIRST_BINARY_EXPR(expected_name) ({                                            \
    auto& fun_function = *code.functions.at(0);                                                \
    REQUIRE(fun_function.identifier.name == expected_name);                                    \
    auto& return_stmt = std::get<Ast_Return_Statement>(fun_function.body.statements.at(0)->v); \
    return_stmt.expression;                                                                    \
  })

template <typename T>
void require_constant_binary_expr_value(Ast_File& code, T expected_value) {
  auto binary_expr_ptr = EXTRACT_FIRST_BINARY_EXPR("const_expr");

  auto& binary_expr = std::get<Ast_Binary_Operation>(binary_expr_ptr->v);
    // We don't yet know
  REQUIRE(!binary_expr.get_meta().is_const());

  // Should be found to be a constant integer value
  REQUIRE_NOTHROW(Semantics().analyse_file(code));
  REQUIRE(binary_expr.get_meta().is_const());
  REQUIRE(std::get<T>(binary_expr.const_value()) == expected_value);
}

TEST_CASE("Binary expressions", "[Sema]") {
  using namespace Catch::Matchers;

  Semantics sema;

  SECTION("Simple expressions are resolved to the correct type") {
    auto code = Parser::parse_from_string(R"(
      fun caculate_scarey_maths: i32 (a: i32, b: i32, c: i32) {
        return a * b + c;
      }
    )");

    auto binary_expr = EXTRACT_FIRST_BINARY_EXPR("caculate_scarey_maths");

    // Start with no type
    REQUIRE(!binary_expr->meta.type);

    REQUIRE_NOTHROW(sema.analyse_file(code));

    auto binop_type = binary_expr->meta.type;
    REQUIRE(std::holds_alternative<PrimativeType>(binop_type->v));

    // Should be resolved to integer
    auto& primative_type = std::get<PrimativeType>(binop_type->v);
    REQUIRE(primative_type.tag == PrimativeType::INTEGER);

    // Should not non-constant expression
    REQUIRE(!std::get<Ast_Binary_Operation>(binary_expr->v).get_meta().is_const());
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

      REQUIRE_THROWS_WITH(sema.analyse_file(code), "division by zero");
    }

    SECTION("Resolved zero") {
      auto code = Parser::parse_from_string(R"(
        fun i_dont_know_maths: i32 {
          return (1233 + 2323 * 2) / (3232 * 32 - (100 * 3) - 51562 * 2);
        }
      )");

      REQUIRE_THROWS_WITH(sema.analyse_file(code), "division by zero");
    }
  }

  SECTION("Mixing floats and integers is not allowed implicitly") {
    auto code = Parser::parse_from_string(R"(
      fun but_c_lets_me: f64 (a: i32, b: f64) {
        return a * b - b * b * a;
      }
    )");

    REQUIRE_THROWS_WITH(sema.analyse_file(code), Contains("incompatible types"));
  }

  SECTION("Mixing integers and booleans is not allowed implicitly") {
    auto code = Parser::parse_from_string(R"(
      fun booleans_are_just_ints_in_c_lol: i32 {
        return (100 < booleans_are_just_ints_in_c_lol()) + 1;
      }
    )");

    REQUIRE_THROWS_WITH(sema.analyse_file(code), Contains("incompatible types"));
  }

  // TODO: A few more type mixings

  SECTION("Modulo operation are not allowed for floats") {
    auto code = Parser::parse_from_string(R"(
      fun fmod: f64 (a: f64, b: f64) {
        # Maybe I'll allow this later
        return a % b;
      }
    )");

    REQUIRE_THROWS_WITH(sema.analyse_file(code), Contains("invalid operation"));
  }

  std::array bitwise_operations = { "<<", ">>", "&", "|", "|!" };

  SECTION("Bitwise operations are not allowed for floats") {
    for (auto bitwise_op: bitwise_operations) {
      auto code = Parser::parse_from_string(formatxx::format_string(R"(
        fun bitty_floats: f64 (f: f64) {{
          return f {} 2.0;
        }
      )", bitwise_op));

      REQUIRE_THROWS_WITH(sema.analyse_file(code), Contains("invalid operation"));
    }
  }

  SECTION("Bitwise operations are not allowed for bools") {
    for (auto bitwise_op: bitwise_operations) {
      auto code = Parser::parse_from_string(formatxx::format_string(R"(
        fun cooly_booly: bool (a: bool, b: bool) {{
          return a {} b;
        }
      )", bitwise_op));

      REQUIRE_THROWS_WITH(sema.analyse_file(code), Contains("invalid operation"));
    }
  }

  SECTION("Bitwise operations are allowed for integers") {
    for (auto bitwise_op: bitwise_operations) {
      auto code = Parser::parse_from_string(formatxx::format_string(R"(
        fun i_like_trains: i32 (a: i32, b: i32) {{
          return a {} b;
        }
      )", bitwise_op));

      REQUIRE_NOTHROW(sema.analyse_file(code));
    }
  }
}

TEST_CASE("Unary expressions", "[Sema]") {
  using namespace Catch::Matchers;

  std::array arth_unary_ops {"+", "-"};
  std::array bitwise_unary_ops {"~"};
  std::array logical_unary_ops = {"¬"};

  Semantics sema;

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

      REQUIRE_NOTHROW(sema.analyse_file(code));
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

      REQUIRE_THROWS_WITH(sema.analyse_file(float_code),
        Contains("invalid unary operation"));

      REQUIRE_THROWS_WITH(sema.analyse_file(bool_code),
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

      REQUIRE_NOTHROW(sema.analyse_file(code));
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

      REQUIRE_THROWS_WITH(sema.analyse_file(float_code),
        Contains("invalid unary operation") || Contains("cannot perform"));

      REQUIRE_THROWS_WITH(sema.analyse_file(int_code),
        Contains("invalid unary operation") || Contains("cannot perform"));
    }
  }
}

TEST_CASE("Expression statements", "[Sema]") {
  // Should be warning in the future
  Semantics sema;

  SECTION("Expressions without side effects are invalid") {
    auto code = Parser::parse_from_string(R"(
      proc haskals_reaction_when {
        # https://i.imgur.com/rXLMONc.jpg
        1 + 2 + 3 + 4 + 5 + 6; # Look ma! No side effects!
      }
    )");

    sema.analyse_file(code);
    REQUIRE_THAT(sema.get_warnings(), HasWarning("statement has no effect"));
  }
}


TEST_CASE("Type names", "[Sema]") {
  using namespace Catch::Matchers;

  Semantics sema;

  SECTION("Undeclared return types should fail") {
    auto code = Parser::parse_from_string(R"(
      fun what_is_type_theory: pure_magic {
        # TODO stub
      }
    )");

    REQUIRE_THROWS_WITH(sema.analyse_file(code), Contains("undeclared return type"));
  }

  SECTION("Undeclared parameter types should fail") {
    auto code = Parser::parse_from_string(R"(
      proc monadic_bind (a: monad) {
        # Get some rope or something?
      }
    )");

    REQUIRE_THROWS_WITH(sema.analyse_file(code), Contains("undeclared arg type"));
  }
}


TEST_CASE("Local variables", "[Sema]") {
  Semantics sema;

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

    REQUIRE_NOTHROW(sema.analyse_file(code));
  }

  SECTION("Using undeclared local variables should fail") {
    auto code = Parser::parse_from_string(R"(
      fun the_var_with_no_name: i32 {
        return 1 + n;
      }
    )");

    REQUIRE_THROWS_WITH(sema.analyse_file(code), "n not declared");
  }
}

TEST_CASE("If statements semantics", "[Sema]") {
  Semantics sema;
  using namespace Catch::Matchers;

  SECTION("If statement condtion must be a boolean") {
    auto code = Parser::parse_from_string(R"(
      proc if_so {
        if "so" {
          # so
        }
      }
    )");

    REQUIRE_THROWS_WITH(sema.analyse_file(code), Contains("if condition must be a Boolean"));
  }
}

TEST_CASE("Function and procedure semantics", "[Sema]") {
  using namespace Catch::Matchers;

  Semantics sema;

  /* Other cases are implicitly tested in other tests */

  SECTION("Returning a type other than the return type is invalid") {
    auto code = Parser::parse_from_string(R"(
      fun a_number_is_a_number: i32 {
        return 10.0;
      }
    )");

    REQUIRE_THROWS_WITH(sema.analyse_file(code), Contains("cannot bind expression"));
  }

  SECTION("Returning something in a procedure is invalid") {
    auto code = Parser::parse_from_string(R"(
      proc no {
        return 10;
      }
    )");

    REQUIRE_THROWS_WITH(sema.analyse_file(code),
      Contains("cannot bind expression") && Contains("Integer32 to ()"));
  }
}

TEST_CASE("If expression semantics", "[Sema]") {
  using namespace Catch::Matchers;

  Semantics sema;

  SECTION("If expressions with matching else are valid") {
    auto code = Parser::parse_from_string(R"(
      fun bool_to_int: i32 (b: bool) {
        # implict return
        if b { 1 } else { 0 }
      }
    )");

    REQUIRE_NOTHROW(sema.analyse_file(code));
  }

  SECTION("If expression must have matching else block") {
    auto code = Parser::parse_from_string(R"(
      fun bool_to_int: i32 (b: bool) {
        if b { 1 } # todo else
      }
    )");

    REQUIRE_THROWS_WITH(sema.analyse_file(code), Contains("without a matching else"));
  }

  SECTION("If expressions with mismatched types are invalid") {
    auto code = Parser::parse_from_string(R"(
      fun bool_to_int: i32 (b: bool) {
        if b { 1 } else { 0.0 }
      }
    )");

    REQUIRE_THROWS_WITH(sema.analyse_file(code), Contains("does not match else block"));
  }
}

TEST_CASE("Block expressions semantics", "[Sema]") {
  using namespace Catch::Matchers;

  Semantics sema;

  SECTION("Blocks can be used in expressions if they have a final expression") {
    auto code = Parser::parse_from_string(R"(
      fun quick_maths: i32 {
        { 2 } + { 2 } - { 1 }
      }
    )");

    REQUIRE_NOTHROW(sema.analyse_file(code));
  }

  SECTION("Blocks with statements can be used in expressions if the block contains a final expression") {
    auto code = Parser::parse_from_string(R"(
      fun is_this_even_legal: i32 (n: i32) {
        # Evaluates to the sum of 1 to n times 2
        {
          # dumb code
          sum := 0;
          for i in 1 .. n + 1 {
            sum = sum + 1;
          }
          sum
        } * 2
      }
    )");

    REQUIRE_NOTHROW(sema.analyse_file(code));
  }

  SECTION("Blocks used in expressions must have the correct type") {
    auto code = Parser::parse_from_string(R"(
      fun when_fun_stops_stop: i32 {
        { 1.0 } + { 1 }
      }
    )");

    REQUIRE_THROWS_WITH(sema.analyse_file(code), Contains("incompatible types"));
  }
}

TEST_CASE("Variable declaration semantics", "[Sema]") {
  using namespace Catch::Matchers;

  Semantics sema;

  SECTION("Declarations with types are valid") {
    auto code = Parser::parse_from_string(R"(
      proc make_int {
        my_int:i32 = 10;
        my_other_int:f64;
      }
    )");

    REQUIRE_NOTHROW(sema.analyse_file(code));
  }

  SECTION("Declarations without types and initializers are valid") {
    auto code = Parser::parse_from_string(R"(
      proc main_int_cool {
        my_int := 10;
      }
    )");

    REQUIRE_NOTHROW(sema.analyse_file(code));

    // The type should be """infered""" (too simple to really be inferance)
    auto& func = *code.functions.at(0);
    auto& decl = std::get<Ast_Variable_Declaration>(func.body.statements.at(0)->v);
    REQUIRE(std::get<PrimativeType>(decl.type->v).tag == PrimativeType::INTEGER);
  }

  SECTION("Declarations without types or initializers are invalid") {
    auto code = Parser::parse_from_string(R"(
      proc ugly {
        what:; # real nasty look :(
      }
    )");

    REQUIRE_THROWS_WITH(sema.analyse_file(code), Contains("cannot infer type of what"));
  }

  SECTION("Declarations with mistmatched types are invalid") {
    auto code = Parser::parse_from_string(R"(
      proc misfit {
        no:i32 = 32.0;
      }
    )");

    REQUIRE_THROWS_WITH(sema.analyse_file(code), Contains("cannot bind expression"));
  }

  SECTION("Declarations with void initializers are valid") {
    auto code = Parser::parse_from_string(R"(
      proc so_this_is_a_thing {
        void := {
          welcome := "to the void";
        }
      }
    )");

    REQUIRE_NOTHROW(sema.analyse_file(code));
  }
}

TEST_CASE("Assign semantics", "[Sema]") {
  using namespace Catch::Matchers;

  Semantics sema;

  SECTION("Assigning to an expression is invalid") {
    auto code = Parser::parse_from_string(R"(
      proc cool_beans {
        1 + 1 = 2;
      }
    )");

    REQUIRE_THROWS_WITH(sema.analyse_file(code),
      "target is not an lvalue");
  }

  SECTION("Assigning to an variable is valid") {
    auto code = Parser::parse_from_string(R"(
      proc cooler_beans {
        cool_beans := 0;
        cool_beans = 1 + 1;
      }
    )");

    REQUIRE_NOTHROW(sema.analyse_file(code));
  }
}

TEST_CASE("For loop semantics", "[Sema]") {
  using namespace Catch::Matchers;

  Semantics sema;

  SECTION("Using a non-existent loop value type is invalid") {
    auto code = Parser::parse_from_string(R"(
      proc _ {
        for x: banana in 0 .. 10 {
          # Who knows!
        }
      }
    )");

    REQUIRE_THROWS_WITH(sema.analyse_file(code), Contains("undeclared loop type"));
  }

  SECTION("Using non matching types for the start and end ranges is invalid") {
    auto code = Parser::parse_from_string(R"(
      proc _ {
        for x in 0.0 .. 10 {
          # :(
        }
      }
    )");

    REQUIRE_THROWS_WITH(sema.analyse_file(code),
      Contains("end range type") && Contains("does not match start range type"));
  }

  SECTION("Loop bodies that evaluate to a value are invalid") {
    auto code = Parser::parse_from_string(R"(
      proc _ {
        for x in 0 .. 10 {
          1000
        }
      }
    )");

    REQUIRE_THROWS_WITH(sema.analyse_file(code),
      Contains("should not evaluate to a value"));
  }
}

TEST_CASE("Pod types and field access semantics", "[Sema]") {
  using namespace Catch::Matchers;

  Semantics sema;

  SECTION("Correctly accessing and assigning fields in a non-nested pod") {
    auto code = Parser::parse_from_string(R"(
      pod AbstractBean {
        coolness: i32,
        abstractness: f64
      }

      fun bean_stuff_you_would_not_understand: f64 {
        bean: AbstractBean;
        bean.coolness = 10000;
        bean.abstractness = 32839.9999;
        bean.abstractness
      }
    )");

    REQUIRE_NOTHROW(sema.analyse_file(code));
    auto& function = *code.functions.at(0);

    auto& assign_1 = std::get<Ast_Assign>(function.body.statements.at(1)->v);
    auto& access_1 = std::get<Ast_Field_Access>(assign_1.target->v);
    // coolness -> index 0 (first element in pod)
    REQUIRE(access_1.field_index == 0);

    auto& assign_2 = std::get<Ast_Assign>(function.body.statements.at(2)->v);
    auto& access_2 = std::get<Ast_Field_Access>(assign_2.target->v);
    // abstractness -> index 1 (second element in pod)
    REQUIRE(access_2.field_index == 1);
  }

  SECTION("Correctly accessing nested pod fields") {
    auto code = Parser::parse_from_string(R"(
      pod A {
        test: i32
      }

      pod B {
        ayy: A
      }

      pod C {
        bee: B
      }

      proc main {
        cee: C;
        cee.bee.ayy.test = 10;
      }
    )");

    REQUIRE_NOTHROW(sema.analyse_file(code));
  }

  SECTION("Accessing fields on non-pods is invalid") {
    auto code = Parser::parse_from_string(R"(
      proc main {
        cake := 1000;
        cake.bake = 10;
      }
    )");

    REQUIRE_THROWS_WITH(sema.analyse_file(code), "not a pod type");
  }

  SECTION("Accessing a field that does not exist on a pod is invalid") {
    auto code = Parser::parse_from_string(R"(
      pod Ben {}

      fun sad: i32 {
        ben: Ben;
        ben.qi
      }
    )");

    REQUIRE_THROWS_WITH(sema.analyse_file(code), Contains("no field named \"qi\""));
  }

  SECTION("Assigning to a non-lvalue field is invalid") {
    auto code = Parser::parse_from_string(R"(
      pod Bar {
        a: i32,
        b: i32
      }

      fun make_bar: Bar {
        bar: Bar;
        bar.a = 1;
        bar.b = 2;
        bar
      }

      proc main {
        make_bar().a = 10;
      }
    )");

    REQUIRE_THROWS_WITH(sema.analyse_file(code), Contains("not an lvalue"));
  }
}

#define REQUIRE_REF_TYPE_TO(function, stmt_idx, to, extra) {        \
  auto& decl = std::get<Ast_Variable_Declaration>(function.body.statements.at(stmt_idx)->v); \
  auto ref_type = std::get_if<ReferenceType>(&decl.type->v);        \
  REQUIRE(ref_type);                                                \
  auto references = std::get_if<to>(&ref_type->references->v);      \
  REQUIRE(references);                                              \
  extra;                                                            \
}

TEST_CASE("Reference binding", "[Sema]") {
  /*
    Note that the same binding logic is used in other bind points,
    e.g. calling a function with a reference type parameter
  */
  using namespace Catch::Matchers;

  Semantics sema;

  SECTION("References bound to lvalues is valid") {
    auto code = Parser::parse_from_string(R"(
      pod Example {
        my_float: f64,
        my_int: i32
      }

      proc ref_test {
        my_pod: Example;
        my_array := [1,2,3,4];
        my_var := 1;

        a := ref my_pod;            # 3
        b := ref my_pod.my_float;   # 4
        c := ref my_pod.my_int;     # 5
        d := ref my_array;          # 6
        e := ref my_array[2];       # 7
        f := ref my_var;            # 8
      }
    )");

    REQUIRE_NOTHROW(sema.analyse_file(code));
    auto& function = *code.functions.at(0);

    // a
    REQUIRE_REF_TYPE_TO(function, 3, Ast_Pod_Declaration, ({}));

    // b
    REQUIRE_REF_TYPE_TO(function, 4, PrimativeType, ({
      REQUIRE(references->tag == PrimativeType::FLOAT64);
    }));

    // c
    REQUIRE_REF_TYPE_TO(function, 5, PrimativeType, ({
      REQUIRE(references->tag == PrimativeType::INTEGER);
    }));

    // d
    REQUIRE_REF_TYPE_TO(function, 6, FixedSizeArrayType, ({
      REQUIRE(references->size == 4);
    }));

    // e
    REQUIRE_REF_TYPE_TO(function, 7, PrimativeType, ({
      REQUIRE(references->tag == PrimativeType::INTEGER);
    }));

    // f
    REQUIRE_REF_TYPE_TO(function, 8, PrimativeType, ({
      REQUIRE(references->tag == PrimativeType::INTEGER);
    }));
  }

  SECTION("References are not implicitly copied") {
    auto code = Parser::parse_from_string(R"(
      proc ref_test {
        my_var := 1;
        a := ref my_var; # 1
        b := my_var;     # 2
      }
    )");

    REQUIRE_NOTHROW(sema.analyse_file(code));

    auto& function = *code.functions.at(0);

    REQUIRE_REF_TYPE_TO(function, 1, PrimativeType, ({
      REQUIRE(references->tag == PrimativeType::INTEGER);
    }));

    auto& b = std::get<Ast_Variable_Declaration>(function.body.statements.at(2)->v);
    // not a reference type
    REQUIRE(std::get_if<PrimativeType>(&b.type->v));
  }

  SECTION("References cannot be bound to rvalues") {
    auto code = Parser::parse_from_string(R"(
      proc ref_test {
        a: ref i32 = 1;
      }
    )");

    REQUIRE_THROWS_WITH(sema.analyse_file(code), Contains("cannot bind rvalue"));
  }

  SECTION("References must be initialized") {
    auto code = Parser::parse_from_string(R"(
      proc ref_test {
        a: ref i32;
      }
    )");

    REQUIRE_THROWS_WITH(sema.analyse_file(code), Contains("must be initialized"));
  }
}

TEST_CASE("References act like their base type", "[Sema]") {
  Semantics sema;
  auto code = Parser::parse_from_string(R"(
    pod Example {
      a: i32
    }

    proc ref_test {
      # Base types
      a: i32 = 1;
      b: f64 = 1.0;
      c := [1, 2, 3];
      d: Example;

      # References
      a_ref := ref a;
      b_ref := ref b;
      c_ref := ref c;
      d_ref := ref d;

      # If the following statements check, then it shows that the reference
      # type is matched with the base type
      a_ref += 1;
      b_ref += 1.0;
      c_ref[1] += 1;
      d_ref.a += 1;
      d_ref = d;
    }
  )");

  REQUIRE_NOTHROW(sema.analyse_file(code));
}

TEST_CASE("Expressions that evaluate to references can be used as lvalues", "[Sema]") {
  using namespace Catch::Matchers;

  Semantics sema;

  SECTION("Non-reference type if expr cannot be assigned to") {
    auto code = Parser::parse_from_string(R"(
      proc ref_test {
        a := 1;
        b := 2;
        if true { a } else { b } = 1;
      }
    )");

    REQUIRE_THROWS_WITH(sema.analyse_file(code), Contains("not an lvalue"));
  }

  SECTION("Reference type if expr can be assigned to") {
    auto code = Parser::parse_from_string(R"(
      proc ref_test {
        a := 1;
        b := 2;
        if true { ref a } else { ref b } = 1;
      }
    )");

    REQUIRE_NOTHROW(sema.analyse_file(code));
  }

  SECTION("Non-reference type call expr cannot be assigned to") {
    auto code = Parser::parse_from_string(R"(
      fun callable: i32 { 1 }

      proc ref_test {
        callable() = 1;
      }
    )");

    REQUIRE_THROWS_WITH(sema.analyse_file(code), Contains("not an lvalue"));
  }

  SECTION("Reference type call can be assigned to") {
    auto code = Parser::parse_from_string(R"(
      fun ref_cast: ref i32 (a: ref i32) { a }

      proc ref_test {
        a: i32;
        ref_cast(a) = 1;
      }
    )");

    REQUIRE_NOTHROW(sema.analyse_file(code));
  }
}
