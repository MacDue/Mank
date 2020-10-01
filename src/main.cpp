#include <vector>
#include <string>
#include <optional>
#include <iostream>
#include <boost/program_options.hpp>

#include "lexer.h"
#include "parser.h"
#include "semantics.h"
#include "ast_printer.h"

struct CompilerOptions {
  std::vector<std::string> input_files;
  bool
    success = true,
    show_help = false,
    print_ast = false,
    check_sema = false;
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
  selected_options.check_sema = ARG_GIVEN("check-sema");

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
    try {
      // TODO: the error should have a reference to the parser
      lexer.load_file(input_file);
      auto parsed_file = parser.parse_file();


      /* yuck! */
      std::optional<CompilerError> sema_error;
      try {
        if (selected_options.check_sema) {
          Sema::analyse_file(parsed_file);
        }
      } catch (CompilerError& e) {
        sema_error = e;
      }
      print_ast(parsed_file);
      if (sema_error) {
        throw *sema_error;
      }

    } catch (CompilerError & error) {
      error.set_lexing_context(lexer);
      std::cerr << error;
    }
  }
}
