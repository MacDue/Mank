#include <gc/gc.h>

#include "parser/parser.h"
#include "sema/semantics.h"
#include "codegen/codegen.h"

#undef WHEN
#include "catch/catch.hpp"

CodeGen compile(std::string source) {
  // Setup libgc for unit tests (I don't think this is even needed)
  if (!GC_is_init_called()) GC_INIT();
  Ast_File code = Parser::parse_from_string(source);
  Semantics sema;
  sema.analyse_file(code);
  // Note: some ast related pointers in codegen will dangle after sema dtors.
  CodeGen codegen(code);
  return codegen;
}

TEST_CASE("Simple addition", "[Codegen]") {
  auto codegen = compile(R"(
    fun add: i32 (a: i32, b: i32) {
      a + b
    }
  )");

  auto add_fn = codegen.extract_function_from_jit<int(int, int)>("add");
  REQUIRE(add_fn(1, 1) == 2);
}

int reference_fibonacci(int n) {
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
  for (int i = 0; i < terms; i++) {  \
    INFO("i = " << i);               \
    REQUIRE(fn(i) == referece(i));   \
  }

TEST_CASE("Fibonacci showdown", "[Codegen]") {
  SECTION("C style recursive fibonacci") {
    auto codegen = compile(R"(
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
    auto codegen = compile(R"(
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
    auto codegen = compile(R"(
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

  auto codegen = compile(R"(
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
  auto codegen = compile(R"(
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
    auto codegen = compile(R"(
      fun clamp: i32 (n: i32, lo: i32, hi: i32) {
        max(lo, min(n, hi))
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

TEST_CASE("Type identity functions", "[Codegen]") {
  /* Just testing that each type minimally works */

  SECTION("i32") {
    auto codegen = compile(
      "fun id: i32 (x: i32) { x }");

    auto id = codegen.extract_function_from_jit<int(int)>("id");
    REQUIRE(id(-23) == -23);
    REQUIRE(id(100'000) == 100'000);
  }

  SECTION("f64") {
    auto codegen = compile(
      "fun id: f64 (x: f64) { x }");

    auto id = codegen.extract_function_from_jit<double(double)>("id");
    REQUIRE(id(666.123) == 666.123);
  }

  SECTION("f32") {
    auto codegen = compile(
      "fun id: f32 (x: f32) { x }");

    auto id = codegen.extract_function_from_jit<float(float)>("id");
    REQUIRE(id(32.21f) == 32.21f);
    REQUIRE(id(-42.12f) == -42.12f);
  }

  SECTION("bool") {
    auto codegen = compile(
      "fun id: bool (x: bool) { x }");

    auto id = codegen.extract_function_from_jit<bool(bool)>("id");
    REQUIRE(id(true) == true);
    REQUIRE(id(false) == false);
  }
}

TEST_CASE("Abstract bean factory", "[Codegen]") {
  auto codegen = compile(R"(
    pod AbstractBean {
      coolness: i32,
      abstractness: f64
    }

    fun abstract_bean_factory: AbstractBean (bean_kind: i32) {
      bean := AbstractBean {
        .coolness = 0,
        .abstractness = 1.0
      };
      if bean_kind == 0 {
        bean.coolness = 5;
        bean.abstractness = 0.4;
      } else if bean_kind == 1 {
        bean.coolness = 1000;
        bean.abstractness = 1000000.1;
      } else {
        bean.coolness = -100000;
        bean.abstractness = 0.0;
      }
      return bean;
    }

    fun get_bean_type_coolness: i32 (bean_kind: i32) {
      abstract_bean_factory(bean_kind).coolness
    }

    fun get_bean_type_abstractness: f64 (bean_kind: i32) {
      abstract_bean_factory(bean_kind).abstractness
    }
  )");

  auto get_bean_type_coolness = codegen.extract_function_from_jit<int(int)>("get_bean_type_coolness");
  REQUIRE(get_bean_type_coolness(0) == 5);
  REQUIRE(get_bean_type_coolness(1) == 1000);
  REQUIRE(get_bean_type_coolness(-1) == -100000);

  auto get_bean_type_abstractness = codegen.extract_function_from_jit<double(int)>("get_bean_type_abstractness");
  REQUIRE(get_bean_type_abstractness(0) == 0.4);
  REQUIRE(get_bean_type_abstractness(1) == 1000000.1);
  REQUIRE(get_bean_type_abstractness(-1) == 0.0);
}

TEST_CASE("Getting and setting in nested pods", "[Codegen]") {
  auto codegen = compile(R"(
    pod One {
      two: Two
    }

    pod Two {
      three: Three
    }

    pod Three {
      test: i32
    }

    fun make_nested_pod: One {
      my_pod := One {.two = Two{.three = Three{.test = 0}}};
      my_pod.two.three.test = 42;
      my_pod
    }

    # Extracting field from local
    fun test_1: i32 {
      my_pod := make_nested_pod();
      my_pod.two.three.test
    }

    # Extracting field from rvalue
    fun test_2: i32 {
      make_nested_pod().two.three.test
    }

    # Setting a field of a local
    fun test_3: i32 {
      my_pod := make_nested_pod();
      my_pod.two.three.test = 1337;
      my_pod.two.three.test
    }
  )");

  auto test_1 = codegen.extract_function_from_jit<int()>("test_1");
  REQUIRE(test_1() == 42);

  auto test_2 = codegen.extract_function_from_jit<int()>("test_2");
  REQUIRE(test_2() == 42);

  auto test_3 = codegen.extract_function_from_jit<int()>("test_3");
  REQUIRE(test_3() == 1337);
}

TEST_CASE("Basic array functionality", "[Codegen]") {
  SECTION("Array sum") {
    auto codegen = compile(R"(
      fun sum: i32 {
        array := [1, 2, 3, 4, 5];
        sum := 0;
        for i in 0 .. array.length {
          sum = sum + array[i];
        }
        sum
      }
    )");

    auto sum = codegen.extract_function_from_jit<int()>("sum");
    REQUIRE(sum() == 15);
  }

  SECTION("Index setting") {
    auto codegen = compile(R"(
      fun index_get_set: i32 {
        not_just_array_myrray := [0,1,2,3];

        not_just_array_myrray[0] =
            not_just_array_myrray[1]
          + not_just_array_myrray[2]
          + not_just_array_myrray[3];

        not_just_array_myrray[0]
      }
    )");

    auto index_get_set = codegen.extract_function_from_jit<int()>("index_get_set");
    REQUIRE(index_get_set() == 1 + 2 + 3);
  }

  SECTION("Non-const array init") {
    auto codegen = compile(R"(
      fun num_sum: i32 (a: i32, b: i32, c: i32, d: i32) {
        nums := [a, b, c, d];
        sum := 0;
        for i in 0 .. nums.length {
          sum = sum + nums[i];
        }
        sum
      }
    )");

    auto num_sum = codegen.extract_function_from_jit<int(int, int, int, int)>("num_sum");
    REQUIRE(num_sum(1,2,3,4) == 1 + 2 + 3 + 4);
  }
}

TEST_CASE("Messing with references", "[Codegen]") {
  SECTION("Passing primatives by reference") {
    auto codegen = compile(R"(
      proc add_by_ref(a: i32, b: i32, result: ref i32) {
        result = a + b;
      }
    )");

    auto add_by_ref = codegen.extract_function_from_jit<void(int, int, int*)>("add_by_ref");

    int result;
    add_by_ref(1, 1, &result);
    REQUIRE(result == 2);
  }

  SECTION("Pass array by reference (no modifications)") {
    auto codegen = compile(R"(
      fun array_sum: i32 (array: ref i32[10]) {
        sum := 0;
        for i in 0 .. array.length {
          sum += array[i];
        }
        sum
      }

      fun sum_to_10: i32 {
        my_array := [1,2,3,4,5,6,7,8,9,10];
        array_sum(my_array)
      }
    )");

    auto sum_to_10 = codegen.extract_function_from_jit<int()>("sum_to_10");
    REQUIRE(sum_to_10() == 55);
  }


  SECTION("Mutate array reference") {
    auto codegen = compile(R"(
      proc swap_pair (pair: ref i32[2]) {
        temp := pair[0];
        pair[0] = pair[1];
        pair[1] = temp;
      }

      fun second: i32 (a: i32, b: i32) {
        pair := [a, b];
        swap_pair(pair);
        pair[0]
      }
    )");

    auto second = codegen.extract_function_from_jit<int(int, int)>("second");
    REQUIRE(second(5,6) == 6);
    REQUIRE(second(2,-1) == -1);
    REQUIRE(second(32,3223) == 3223);
  }

  SECTION("Local references") {
    auto codegen = compile(R"(
      fun idk_what_this_is: i32 (a: i32, b: i32) {
        a_ref: = ref a;
        b_ref: = ref b;
        if a > b { a_ref } else { b_ref } = 100;
        a
      }
    )");

    auto idk_what_this_is = codegen.extract_function_from_jit<int(int, int)>("idk_what_this_is");
    REQUIRE(idk_what_this_is(3, 2) == 100);
    REQUIRE(idk_what_this_is(2, 3) == 2);
  }
}

TEST_CASE("Simple lambdas", "[Codegen]") {
  SECTION("Lambda with no env") {
    auto codegen = compile(R"(
      fun apply_lambda: i32 (lambda: \i32,i32 -> i32, a: i32, b: i32) {
        lambda(a, b)
      }

      fun make_adder: \i32,i32 -> i32 {
        \a: i32, b: i32 -> i32 { a + b }
      }

      fun make_mult: \i32,i32 -> i32 {
        \a: i32, b: i32 -> i32 { a * b }
      }

      fun add: i32 (a: i32, b: i32) {
        multer := make_adder();
        apply_lambda(multer, a, b)
      }

      fun mult: i32 (a: i32, b: i32) {
        adder := make_mult();
        apply_lambda(adder, a, b)
      }
    )");

    auto add = codegen.extract_function_from_jit<int(int, int)>("add");
    REQUIRE(add(1,1) == 2);
    REQUIRE(add(10, -5) == 5);

    auto mult = codegen.extract_function_from_jit<int(int, int)>("mult");
    REQUIRE(mult(1, 1) == 1);
    REQUIRE(mult(10, -5) == 10 * -5);
  }
}

TEST_CASE("Closures", "[Codegen]") {
  SECTION("makeAdder") {
    auto codegen = compile(R"(
      fun make_adder: \i32 -> i32 (x: i32) {
        \y: i32 -> i32 {
          x + y
        }
      }

      fun add: i32 (a: i32, b: i32) {
        adder := make_adder(a);
        adder(b)
      }
    )");

    auto add = codegen.extract_function_from_jit<int(int, int)>("add");
    REQUIRE(add(1, 1) == 2);
    REQUIRE(add(20, -5) == 15);
  }

  SECTION("Curried addThree") {
    auto codegen = compile(R"(
      fun make_add_three: \i32 -> \i32 -> \i32 -> i32 {
        \a:i32 -> \i32 -> \i32 -> i32 {
          \b:i32 -> \i32 -> i32 {
            \c:i32 -> i32 {
              a + b + c
            }
          }
        }
      }

      fun add_three: i32 (a: i32, b: i32, c: i32) {
        adder := make_add_three();
        adder(a)(b)(c)
      }
    )");

    auto add_three = codegen.extract_function_from_jit<int(int, int, int)>("add_three");
    REQUIRE(add_three(1, 2, 3) == 1 + 2 + 3);
  }

  SECTION("Counter") {
    auto codegen = compile(R"(
      fun make_counter: \ -> i32 (start: i32) {
        count := start;
        \ -> i32 {
          count += 1;
          count
        }
      }

      fun count_to: i32 (n: i32) {
        counter := make_counter(0);
        for _  in 0 .. n - 1 {
          __ := counter();
        }
        counter()
      }
    )");

    auto count_to = codegen.extract_function_from_jit<int(int)>("count_to");
    REQUIRE(count_to(10) == 10);
  }

  SECTION("Recursive lambda") {
    auto codegen = compile(R"(
      fun make_fib: \i32 -> i32 {
        fib_proto := \x:i32 -> {0}; # dummy
        \ -> \i32 -> i32 {
          fib_ref := ref fib_proto;
          fib_proto = \n: i32 -> i32 {
            if n == 0 {
              0
            } else if (n == 1) {
              1
            } else {
              fib_ref(n - 1) + fib_ref(n - 2)
            }
          }
          fib_proto
        }()
      }

      fun fib: i32 (n: i32) {
        fib_func := make_fib();
        fib_func(n)
      }
    )");

    auto fib = codegen.extract_function_from_jit<int(int)>("fib");
    MATCH_INTEGER_TO_INTEGER_FUNCTION(fib, reference_fibonacci, 20);
  }

  SECTION("Using top level function as lambda") {
    auto codegen = compile(R"(
      fun normal_add: i32 (a: i32, b: i32) { a + b }

      fun pbind: \i32 -> i32 (func: \i32,i32 -> i32, a: i32) {
        \b: i32 -> i32 {
          func(a, b)
        }
      }

      fun make_adder: \i32 -> i32 (x: i32) {
        pbind(normal_add, x)
      }

      fun add: i32 (a: i32, b: i32) {
        adder := make_adder(a);
        adder(b)
      }
    )");

    auto add = codegen.extract_function_from_jit<int(int, int)>("add");
    REQUIRE(add(1,2) == 3);
    REQUIRE(add(-19, 9) == -10);
  }

  // This should map to one top level wrapper & not crash
  SECTION("Repeatedly using a top level function as a lambda") {
    auto codegen = compile(R"(
      fun double: i32 (x: i32) {
        x * 2
      }

      proc apply (pair: ref i32[2], op: \i32 -> i32) {
        pair[0] = op(pair[0]);
        pair[1] = op(pair[1]);
      }

      fun do_apply: i32 (x: i32, op: \i32 -> i32) {
        op(x)
      }

      fun test: i32 {
        a := [1, 2];
        b := 3;

        apply(a, double);
        b = do_apply(b, double);

        return a[0] + a[1] + b;
      }
    )");

    auto test = codegen.extract_function_from_jit<int()>("test");
    REQUIRE(test() + (1 + 2 + 3) * 2);
  }
}

TEST_CASE("Bind macro", "[Codegen]") {
  SECTION("Binding top level functions") {
    auto codegen = compile(R"(
      fun add4: i32 (a: i32, b: i32, c: i32, d: i32) {
        a + b + c + d
      }

      fun fun_add: i32 (a: i32, b: i32) {
        adder := pbind!(add4, a, 10);
        adder(b, 20)
      }
    )");

    auto fun_add = codegen.extract_function_from_jit<int(int, int)>("fun_add");
    REQUIRE(fun_add(9, 1) == 40);
  }
}

TEST_CASE("Curry marco", "[Codegen]") {
  SECTION("Currying top level functions") {
    auto codegen = compile(R"(
      fun add4: i32 (a: i32, b: i32, c: i32, d: i32) {
        a + b + c + d
      }

      fun just_like_haskell: i32 {
        func := curry!(add4);
        func(1)(2)(3)(4)
      }
    )");

    auto just_like_haskell = codegen.extract_function_from_jit<int()>("just_like_haskell");
    REQUIRE(just_like_haskell() == 1 + 2 + 3 + 4);
  }

}

TEST_CASE("Really like haskell", "[Codegen]") {
  auto codegen = compile(R"(
    fun sum: i32 {
      # Curried without a macro
      add4 := \a -> {
        \b -> {
          \c -> {
            \d -> {
              a + b + c + d }}}};
      add4(1)(2)(3)(4)
    }
  )");
  auto sum = codegen.extract_function_from_jit<int()>("sum");
  REQUIRE(sum() == 1 + 2 + 3 + 4);
}

// TODO: More tuple tests
TEST_CASE("Tuples", "[Codegen]") {
  SECTION("Tuple pattern assignment") {
    auto codegen = compile(R"(
      proc swap(a: ref i32, b: ref i32) {
        (a, b) = (b, a);
      }
    )");
    auto swap = codegen.extract_function_from_jit<int(int*, int*)>("swap");
    int x = 10, y = 20;
    REQUIRE((x == 10 && y == 20));
    swap(&x, &y);
    REQUIRE((x == 20 && y == 10));
  }
}

TEST_CASE("Pod literals", "[Codegen]") {
  SECTION("Non-nested pod") {
    auto codegen = compile(R"(
      pod Test {
        hello: i32,
        world: f64,
        lol: bool
      }

      fun test_pass: bool (given: ref Test) {
        # This assumes earlier (simpler) features work :)
        if given.hello == 1000 {
          if given.world == 1.0 {
            if given.lol {
              return true;
            }
          }
        }
        return false;
      }

      fun sanity_check: bool {
        my_test := Test {
          .hello = 999,
          .world = 1.0,
          .lol = false
        };
        test_pass(my_test)
      }

      fun all_fields_read_back_should_match_init: bool {
        my_test := Test {
          .hello = 1000,
          .world = 1.0,
          .lol = true
        };
        test_pass(my_test)
      }

      fun order_of_init_fields_does_not_change_result: bool {
        my_test := Test {
          .lol = true,
          .hello = 1000,
          .world = 1.0
        };
        test_pass(my_test)
      }
    )");

    auto sanity_check = codegen.extract_function_from_jit<bool()>("sanity_check");
    REQUIRE(!sanity_check());

    auto all_fields_read_back_should_match_init = codegen.extract_function_from_jit<bool()>(
      "all_fields_read_back_should_match_init");

    REQUIRE(all_fields_read_back_should_match_init());

    auto order_of_init_fields_does_not_change_result = codegen.extract_function_from_jit<bool()>(
      "order_of_init_fields_does_not_change_result");

    REQUIRE(order_of_init_fields_does_not_change_result());
  }

  SECTION("Pod nested in pod") {
    auto codegen = compile(R"(
      pod Foo {
        an_int: i32,
        bar: Bar
      }

      pod Bar {
        is_cool: bool,
        a_float: f64
      }

      fun test_pass: bool (given: ref Foo) {
        if given.an_int == 1066 {
          if ¬given.bar.is_cool {
            if given.bar.a_float == 3.14 {
              return true;
            }
          }
        }
        return false;
      }

      fun test_creating_nested_pod: bool {
        my_foo := Foo {
          .an_int = 1066,
          .bar = Bar { .is_cool = false, .a_float = 3.14 }
        };
        test_pass(my_foo)
      }
    )");
    auto test_creating_tested_pod = codegen.extract_function_from_jit<bool()>(
      "test_creating_nested_pod");

    REQUIRE(test_creating_tested_pod());
  }

  SECTION("Nesting tuples/arrays in pod init") {
    auto codegen = compile(R"(
      pod Agent {
        weights: f64[4],
        chosen_move: (i32, bool)
      }

      fun kalah: bool {
        agent := Agent {
          .weights = [0.4, 0.2, 1.0, -0.1],
          .chosen_move = (4, true)
        };

        if agent.weights[0] == 0.4 {
          if agent.weights[1] == 0.2 {
            if agent.weights[2] == 1.0 {
              if agent.weights[3] == -0.1 {
                bind (pit, best_move) = agent.chosen_move;
                if pit == 4 {
                  if best_move {
                    return true;
                  }
                }
              }
            }
          }
        }
        return false;
      }
    )");
    auto kalah = codegen.extract_function_from_jit<bool()>("kalah");
    REQUIRE(kalah());
  }
}
