#pragma once

#include <memory>

#include "ast.h"
#include "code_generator.h"

class CodeGen {
  std::unique_ptr<CodeGenerator> impl;
public:
  CodeGen(Ast_File& file_ast);

  void emit_object(std::string const & path);
};
