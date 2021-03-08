#pragma once

#include <memory>
#include <string>
#include <type_traits>

#include "ast/ast.h"
#include "codegen/code_generator.h"

class CodeGen {
  std::unique_ptr<CodeGenerator> impl;

  void* find_jit_symbol(std::string name);
public:
  CodeGen(Ast_File& file_ast);

  void emit_object(std::string const & path);
  std::string get_generated_code() const;

  template<typename TFunc>
  TFunc* extract_function_from_jit(std::string name, bool mank = true) {
    static_assert(std::is_function_v<TFunc> && "can only extract functions from jit");
    if (mank) {
      name = "__mank__" + name; // FIXME: Mangle hack!
    }
    return reinterpret_cast<TFunc*>(find_jit_symbol(name));
  }
};
