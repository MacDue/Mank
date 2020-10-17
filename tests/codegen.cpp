#include "catch/catch.hpp"

#include "parser.h"
#include "codegen.h"
#include "semantics.h"

CodeGen parse_and_compile(std::string source) {
  Ast_File code = Parser::parse_from_string(source);
  Semantics().analyse_file(code);
  CodeGen codegen(code);
  // codegen.jit_compile_code();
  return codegen;
}

TEST_CASE("Simple addition", "[Codegen]") {
  auto codegen = parse_and_compile(R"(
    fun add: i32 (a: i32, b: i32) {
      a + b
    }
  )");

  auto add_fn = codegen.extract_function_from_jit<int(int, int)>("add");

  REQUIRE(add_fn(1, 1) == 2);
}

int reference_fibonacci(int n) {
  // This
  int a = 0;
  int b = 1;
  for (int i = 0; i < n; i++) {
    int temp = b;
    b = a + b;
    a = temp;
  }
  return a;
}

#define MATCH_INTEGER_TO_INTEGER_FUNCTION(fn, referece, terms) \
  for (int i = 0; i < terms; i++) {   \
    INFO("i = " << i);                \
    REQUIRE(fn(i) == referece(i));    \
  }

TEST_CASE("Fibonacci showdown", "[Codegen]") {
  SECTION("C style recursive fibonacci") {
    auto codegen = parse_and_compile(R"(
      fun fib: i32 (n: i32) {
        if (n == 0) {
          return 0;
        } else if (n == 1) {
          return 1;
        } else {
          return fib(n - 1) + fib(n - 2);
        }
      }
    )");

    auto fib_fn = codegen.extract_function_from_jit<int(int)>("fib");
    MATCH_INTEGER_TO_INTEGER_FUNCTION(fib_fn, reference_fibonacci, 20);
  }

  SECTION("Expression based fibonacci") {
    auto codegen = parse_and_compile(R"(
      fun fib: i32 (n: i32) {
        if n == 0 {
          0
        } else if n == 1 {
          1
        } else {
          fib(n - 1) + fib(n - 2)
        }
      }
    )");

    auto fib_fn = codegen.extract_function_from_jit<int(int)>("fib");
    MATCH_INTEGER_TO_INTEGER_FUNCTION(fib_fn, reference_fibonacci, 20);
  }

  SECTION("Iterative fibonacci") {
    auto codegen = parse_and_compile(R"(
      fun fib: i32 (n: i32) {
        a := 0;
        b := 1;
        for _ in 0 .. n {
          temp := b;
          b = a + b;
          a = temp;
        }
        a
      })");

    auto fib_fn = codegen.extract_function_from_jit<int(int)>("fib");
    MATCH_INTEGER_TO_INTEGER_FUNCTION(fib_fn, reference_fibonacci, 1000);
  }
}
