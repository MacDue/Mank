#include <vector>
#include <string>
#include <optional>
#include <iostream>
#include <boost/program_options.hpp>

#include "parser/parser.h"
#include "sema/semantics.h"
#include "codegen/codegen.h"
#include "ast/ast_printer.h"

#include "compiler_message_printer.h"

struct CompilerOptions {
  std::vector<std::string> input_files;
  bool
    success = true,
    show_help = false,
    print_ast = false,
    check_sema = false,
    code_gen = false,
    suppress_warnings = false;
};

#define ARG_GIVEN(name) vm.count(name)

// See: https://valelab4.ucsf.edu/svn/3rdpartypublic/boost/doc/html/program_options/tutorial.html
static CompilerOptions parse_command_line_args(int argc, char* argv[]) {
  namespace po = boost::program_options;
  po::options_description desc("Mank compiler options");
  CompilerOptions selected_options;
  desc.add_options()
    ("help", "show help")
    ("print-ast", "print AST of the input source")
    ("check-sema", "check the semantics of the input")
    ("codegen", "generate LLVM IR")
    ("suppress-warnings", "don't output warnings")
    ("input-file", po::value<std::vector<std::string>>(), "input file");

  po::positional_options_description p;
  p.add("input-file", -1);

  po::variables_map vm;
  po::store(po::command_line_parser(argc, argv)
            .options(desc).positional(p).run(), vm);
  po::notify(vm);

  if (vm.count("help")) {
    std::cout << desc << '\n';
    selected_options.show_help = true;
    return selected_options;
  }

  selected_options.print_ast = ARG_GIVEN("print-ast");
  selected_options.code_gen = ARG_GIVEN("codegen");
  selected_options.check_sema = selected_options.code_gen || ARG_GIVEN("check-sema");
  selected_options.suppress_warnings = ARG_GIVEN("suppress-warnings");

  if (vm.count("input-file")) {
    selected_options.input_files = vm["input-file"].as<std::vector<std::string>>();
  }

  return selected_options;
}

static void print_ast(Ast_File& ast) {
  AstPrinter ast_printer(std::cout);
  ast_printer.print_file(ast);
}

static auto constexpr PRELUDE = R"(
  # Some useful "stdlib" functions.

  proc print(s: str) {
    for i in 0 .. s.length {
      putchar(s[i]);
    }
  }

  proc println(s: str) {
    print(s);
    putchar('\n');
  }

  proc fail(msg: str) {
    println!("fail: {msg}", msg);
    abort();
  }
)";

static bool compile(std::string program, CompilerOptions options, bool path = true) {
  Lexer lexer;
  Parser parser(lexer);

  if (path) {
    lexer.load_file(program);
  } else {
    lexer.set_input_to_string(program);
  }

  Semantics sema;
  std::optional<Ast_File> parsed_file;
  std::optional<CompilerError> sema_error;
  try {
    parsed_file.emplace(parser.parse_file());
    if (options.check_sema || options.code_gen) {
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
  if (!options.suppress_warnings) {
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
    CodeGen codegen(*parsed_file);
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
  }

  for (auto const & input_file: selected_options.input_files) {
    if (!compile(input_file, selected_options)) {
      return 1;
    }
  }
  return 0;
}
