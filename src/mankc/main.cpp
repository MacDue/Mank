#include <vector>
#include <string>
#include <optional>
#include <iostream>
// #include <boost/program_options.hpp>

#include "parser/parser.h"
#include "sema/semantics.h"
#include "codegen/codegen.h"
#include "ast/ast_printer.h"

#include "compiler_message_printer.h"

struct CompilerOptions {
  std::vector<std::string> input_files;
  bool
    success     :1 = true,
    show_help   :1 = false,
    print_ast   :1 = false,
    check_sema  :1 = false,
    code_gen    :1 = false,
    build_tests :1 = false,
    suppress_warnings:1 = false;
};

#define ARG_GIVEN(name) vm.count(name)

// See: https://valelab4.ucsf.edu/svn/3rdpartypublic/boost/doc/html/program_options/tutorial.html
static CompilerOptions parse_command_line_args(int argc, char* argv[]) {
  // namespace po = boost::program_options;
  // po::options_description desc("Mank compiler options");
  CompilerOptions selected_options;
  // desc.add_options()
  //   ("help", "show help")
  //   ("print-ast", "print AST of the input source")
  //   ("check-sema", "check the semantics of the input")
  //   ("codegen", "generate LLVM IR")
  //   ("suppress-warnings", "don't output warnings")
  //   ("tests", "build tests and test runner")
  //   ("input-file", po::value<std::vector<std::string>>(), "input file");

  // po::positional_options_description p;
  // p.add("input-file", -1);

  // po::variables_map vm;
  // po::store(po::command_line_parser(argc, argv)
  //           .options(desc).positional(p).run(), vm);
  // po::notify(vm);

  // if (vm.count("help")) {
  //   std::cout << desc << '\n';
  //   selected_options.show_help = true;
  //   return selected_options;
  // }

  // selected_options.print_ast = ARG_GIVEN("print-ast");
  // selected_options.code_gen = ARG_GIVEN("codegen");
  // selected_options.check_sema = selected_options.code_gen || ARG_GIVEN("check-sema");
  // selected_options.suppress_warnings = ARG_GIVEN("suppress-warnings");
  // selected_options.build_tests = ARG_GIVEN("tests");

  // if (vm.count("input-file")) {
  //   selected_options.input_files = vm["input-file"].as<std::vector<std::string>>();
  // }

  selected_options.print_ast = true;
  selected_options.code_gen = true;

  return selected_options;
}

static void print_ast(Ast_File& ast) {
  AstPrinter ast_printer(std::cout);
  ast_printer.print_file(ast);
}

static auto constexpr PRELUDE = R"(
  # Some useful "stdlib" functions.

  # Output

  proc _print(s: str, out: \char -> i32) {
    for i in 0 .. s.length {
      _ := out(s[i]);
    }
  }

  proc _println(s: str, out: \char -> i32) {
    _print(s, out);
    _ := out('\n');
  }

  proc print(s: str) {
    _print(s, putchar)
  }

  proc eprint(s: str) {
    _print(s, stderr_putchar)
  }

  proc println(s: str) {
    _println(s, putchar)
  }

  proc eprintln(s: str) {
    _println(s, stderr_putchar)
  }

  proc fail(msg: str) {
    eprintln!("fail: {msg}", msg);
    abort();
  }

  # Input

  fun input: str {
    # Bad unbuffered impl
    read: str = "";
    loop {
      next := getchar() as char;
      if next == '\n' {
        break;
      }
      read += next as str;
    }
    read
  }

  fun prompt: str (msg: str) {
    if msg.length > 0 {
      print!("{msg} ", msg);
    }
    input()
  }

  fun prompt_int: i32 (msg: str) {
    bind (value, parsed) = (-1, false);
    while (!parsed) {
      value_str := prompt(msg);
      (value, parsed) = parse_int(value_str);
      if (!parsed) {
        println("Please enter a valid number!");
      }
    }
    value
  }

  fun input_int: i32 { prompt_int("") }

  # Parsing

  fun parse_int: (i32, bool) (s: str) {
    value := 0;
    is_neg := false;
    for i in 0 .. s.length {
      c := s[i];
      if i == 0 {
        if (c == '+') {
          # do nothing
          continue;
        } else if (c == '-') {
          is_neg = true;
          continue;
        }
      }
      if c >= '0' && c <= '9' {
        value *= 10;
        value += (c - '0') as i32;
      } else {
        return (-1, false);
      }
    }
    return (if is_neg { -value } else { value }, true);
  }

  # String helpers

  fun reverse_str: str (s: str) {
    out := "";
    for idx in 0 .. s.length {
      out += s[s.length - idx - 1] as str;
    }
    out
  }

  fun int_to_string: str (value: i32) {
    value_str := "";
    bind (value, is_neg) = if value < 0 {
      (-value, true)
    } else {
      (value, false)
    };
    loop {
      value_str += ((value % 10) as char + '0') as str;
      value = value / 10;
      if value == 0 { break; }
    }
    # Temp hack till buffers -> str added
    value_str = reverse_str(value_str);
    if is_neg {
      "-" + value_str
    } else {
      value_str
    }
  }

  fun str_compare: i32 (a: str, b: str) {
    idx := 0;
    while idx < a.length && idx < b.length {
      if a[idx] != b[idx] {
        return (a[idx] as i32) - (b[idx] as i32);
      }
      idx += 1;
    }
    if a.length != b.length {
      a.length - b.length
    } else {
      0
    }
  }

  fun str_equal: bool (a: str, b: str) {
    str_compare(a, b) == 0
  }
)";

static bool compile(std::string program, CompilerOptions options, bool path = true) {
  Lexer lexer;
  Parser parser(lexer);
  Semantics sema;

  if (options.build_tests) {
    sema.build_test_runner();
  }

  std::optional<Ast_File> parsed_file;
  std::optional<CompilerError> sema_error;
  try {
    if (path) {
      lexer.load_file(program);
    } else {
      lexer.set_input_to_string(program);
    }

    parsed_file.emplace(parser.parse_file());
    if (options.check_sema || options.code_gen) {
      sema.set_source(lexer);
      sema.analyse_file(*parsed_file);
    }
  } catch (CompilerError& e) {
    sema_error = e;
    sema_error->set_lexing_context(lexer);
  }

  if (parsed_file && options.print_ast) {
    print_ast(*parsed_file);
  }

  // TODO: the error/warnings should already have a reference to the lexer
  if (!options.suppress_warnings && path /* !path -> stdlib */) {
    for (auto& warning: sema.get_warnings()) {
      warning.source_lexer = &lexer;
      std::cerr << warning;
    }
  }

  if (sema_error) {
    std::cerr << *sema_error;
    return false;
  }

  if (options.code_gen) {
    try {
      CodeGen codegen(*parsed_file);
    } catch (CompilerError& e) {
      std::cerr << e;
    }
  }

  return true;
}

int main(int argc, char* argv[]) {
  auto selected_options = parse_command_line_args(argc, argv);
  if (selected_options.show_help) {
    return 0;
  }

  // Hacky current solution to builtin functions
  if (selected_options.code_gen) {
    bool prelude_compiled = compile(PRELUDE, selected_options, false);
    assert(prelude_compiled);
    if (!prelude_compiled) {
      // Should not happen
      return 1;
    }
  }

  if (selected_options.input_files.empty()) {
    std::cerr << CompilerMessage{std::nullopt, "no input files"};
    return 1;
  }

  for (auto const & input_file: selected_options.input_files) {
    if (!compile(input_file, selected_options)) {
      return 1;
    }
  }
  return 0;
}
