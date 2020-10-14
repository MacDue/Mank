#include <vector>
#include <string>
#include <optional>
#include <iostream>
#include <boost/program_options.hpp>

#include "parser.h"
#include "semantics.h"
#include "ast_printer.h"

#include "codegen.h"

#include "compiler_message_printer.h"

struct CompilerOptions {
  std::vector<std::string> input_files;
  bool
    success = true,
    show_help = false,
    print_ast = false,
    check_sema = false,
    code_gen = false;
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

  if (vm.count("input-file")) {
    selected_options.input_files = vm["input-file"].as<std::vector<std::string>>();
  }

  return selected_options;
}

static void print_ast(Ast_File& ast) {
  AstPrinter ast_printer(std::cout);
  ast_printer.print_file(ast);
}

int main(int argc, char* argv[]) {
  auto selected_options = parse_command_line_args(argc, argv);
  if (selected_options.show_help) {
    return 0;
  }

  Lexer lexer;
  Parser parser(lexer);
  for (auto const & input_file: selected_options.input_files) {
    lexer.load_file(input_file);

    Semantics sema;
    std::optional<Ast_File> parsed_file;
    std::optional<CompilerError> sema_error;
    try {
      parsed_file = parser.parse_file();
      if (selected_options.check_sema || selected_options.code_gen) {
        sema.analyse_file(*parsed_file);
      }
    } catch (CompilerError& e) {
      sema_error = e;
      sema_error->set_lexing_context(lexer);
    }

    if (parsed_file && selected_options.print_ast) {
      print_ast(*parsed_file);
    }

    // TODO: the error/warnings should alreadg have a reference to the lexer
    for (auto& warning: sema.get_warnings()) {
      warning.source_lexer = &lexer;
      std::cerr << warning;
    }

    if (sema_error) {
      std::cerr << *sema_error;
      return 1;
    }

    if (selected_options.code_gen) {
      CodeGen codegen(*parsed_file);
    }
  }
  return 0;
}
