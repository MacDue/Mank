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

    auto fib = codegen.extract_function_from_jit<int(int)>("fib");
    MATCH_INTEGER_TO_INTEGER_FUNCTION(fib, reference_fibonacci, 20);
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

    auto fib = codegen.extract_function_from_jit<int(int)>("fib");
    MATCH_INTEGER_TO_INTEGER_FUNCTION(fib, reference_fibonacci, 20);
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

    auto fib = codegen.extract_function_from_jit<int(int)>("fib");
    MATCH_INTEGER_TO_INTEGER_FUNCTION(fib, reference_fibonacci, 1000);
  }
}

TEST_CASE("Peculiar nesting", "[Codegen]") {

  auto codegen = parse_and_compile(R"(
    fun pointless_computation: i32 (cool_number: i32) {
      total := 0;
      for x in
      {
        sum := 0;
        for n in 0 .. cool_number {
          sum = sum + n;
        }
        sum
      }
      ..
      {
        factorial := 1;
        for n in 1 .. cool_number + 1 {
          factorial = factorial * n;
        }
        factorial
      }
      {
        total = total + x;
      }
      total
    }
  )");

  auto pointless_computation = codegen.extract_function_from_jit<int(int)>("pointless_computation");
  REQUIRE(pointless_computation(8) == 812830662);
}

TEST_CASE("Declaration shadowing within nested scopes", "[Codegen]") {
  // This code previously failed to compile
  // It should run and just return 0
  auto codegen = parse_and_compile(R"(
    fun broken_sum: i32 (n: i32) {
      total := 0;
      for x in 1 .. n + 1 {
        # Mistake declaring a new total variable
        total := total + x;
      }
      total
    }
  )");

  auto broken_codegen = codegen.extract_function_from_jit<int(int)>("broken_sum");
  REQUIRE(broken_codegen(100) == 0); // Since there is a mistake in this code
}

TEST_CASE("Calling other functions", "[Codegen]") {

  SECTION("Simple clamp") {
    auto codegen = parse_and_compile(R"(
      fun clamp: i32 (n: i32, lo: i32, hi: i32) {
        return max(lo, min(n, hi));
      }

      fun max: i32 (a: i32, b: i32) {
        if a > b { a } else { b }
      }

      fun min: i32 (a: i32, b: i32) {
        if a < b { a } else { b }
      }
    )");

    auto max = codegen.extract_function_from_jit<int(int, int)>("max");
    REQUIRE(max(0, -20) == 0);

    auto clamp = codegen.extract_function_from_jit<int(int, int, int)>("clamp");

    REQUIRE(clamp(10, 0, 5) == 5);
    REQUIRE(clamp(-20, 0, 5) == 0);
    REQUIRE(clamp(3, 0, 5) == 3);
  }
}
