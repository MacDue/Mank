/* core */
#include "parser/parser.h"
#include "sema/semantics.h"

#undef WHEN
#include "catch/catch.hpp"

#include "helpers/warning_matchers.h"

WarningsMatcher HasWarning(std::string needle) {
  return Catch::Matchers::Predicate<std::vector<CompilerMessage>>(
    [&](auto const & warnings) -> bool {
      for (auto const& warning: warnings) {
        if (warning.message.find(needle) != std::string::npos) {
          return true;
        }
      }
      return false;
    });
}


TEST_CASE("Return reachability", "[Sema]") {
  using namespace Catch::Matchers;

  Semantics sema;

  SECTION("Simple functions that return are valid") {
    auto code = Parser::parse_from_string(R"(
      fun foo_bar: i32 {
        return 1337;
      }
    )");

    REQUIRE_NOTHROW(sema.analyse_file(code));
  }

  SECTION("Functions without returns in a if statement are valid, if it can fallthrough to a return") {
    auto code = Parser::parse_from_string(R"(
      fun fallthrough_1: i32 {
        if true {

        }
        return 10;
      }

      fun fallthrough_2: i32 (a: i32) {
        if a < 10 {

        } else {

        }
        return 20;
      }

      fun fallthrough_3: i32 (a: i32) {
        if a >= 20 {
          return 20;
        } else {

        }
        return 10;
      }

      fun fallthrough_4: i32 (a: i32) {
        if a - 1 > 2 {

        } else {
          return 20;
        }
        return 10;
      }
    )");

    REQUIRE_NOTHROW(sema.analyse_file(code));
  }

  SECTION("Not returning on all paths is invalid, even if where there's not returns can be found to be unreachable") {
    auto code = Parser::parse_from_string(R"(
      fun thats_a_paddlin: bool {
        if false {
          # This is so what's valid does not change as constrant propagation
          # improves
        } else {
          return true;
        }
      }
    )");

    REQUIRE_THROWS_WITH(sema.analyse_file(code), Contains("fails to return a value"));
  }

  SECTION("Having returns only within an if statement is valid, if all paths of the if return") {
    auto code = Parser::parse_from_string(R"(
      fun i_like_ifs: i32 (a: i32) {
        if a < 50 {
          return 1;
        } else {
          return 0;
        }
      }

      fun nested_ifs: i32 (a: i32, b: i32) {
        if a > 10 {
          if b < 0 {
            return 1;
          } else {
            return 2;
          }
        } else {
          if b > 1 {
            return 3;
          }
          return 4;
        }
      }
    )");

    REQUIRE_NOTHROW(sema.analyse_file(code));
  }

  SECTION("Not returning in a simple function is invalid") {
    auto code = Parser::parse_from_string(R"(
      fun well_im_not_doing_it: i32 {
        # no >:(
      }
    )");

    REQUIRE_THROWS_WITH(sema.analyse_file(code), Contains("fails to return a value"));
  }

  SECTION("Not returning a value in some path of an if is invalid (if there's no return after the if)") {
    auto code = Parser::parse_from_string(R"(
      fun oops_i_forgot: i32 (a: i32) {
        if a < 2 {
          if a < -1 {
            return 10;
          }
        } else {
          return 20;
        }
      }
    )");

    REQUIRE_THROWS_WITH(sema.analyse_file(code), Contains("fails to return a value"));
  }
}

TEST_CASE("Unreachable code", "[Sema]") {
  Semantics sema;

  SECTION("Unreachable then blocks are invalid") {
    auto code = Parser::parse_from_string(R"(
      fun bad_ifs: i32 {
        # Maybe allow this with a const if?
        if false {
          return 10; # <--- unreachable
        }
        return 20;
      }
    )");

    sema.analyse_file(code);
    REQUIRE_THAT(sema.get_warnings(), HasWarning("unreachable code"));
  }

  SECTION("Unreachable else blocks are invalid") {
    auto code = Parser::parse_from_string(R"(
      fun the_else_is_a_lie: i32 {
        if true {
          return 10;
        } else {
          return 20; # <--- unreachable
        }
      }
    )");

    sema.analyse_file(code);
    REQUIRE_THAT(sema.get_warnings(), HasWarning("unreachable code"));
  }

  SECTION("Unreachable statements are invalid") {
    auto code = Parser::parse_from_string(R"(
      fun bad_code: i32 {
        return 42;
        return 1337; # <-- unreachable
      }
    )");

    sema.analyse_file(code);
    REQUIRE_THAT(sema.get_warnings(), HasWarning("unreachable code"));
  }
}
