#pragma once

#include <string>

#include <llvm/IR/Value.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/Target/TargetMachine.h>

#include "ast.h"
#include "scope.h"
#include "code_generator.h"

class LLVMCodeGen: public CodeGenerator {

  struct SymbolMetaLocal: SymbolMeta {
    llvm::Value* value;

    SymbolMetaLocal(llvm::Value* value)
      : value{value} {}
  };

  /*
    Owns core llvm datastructures (such as type & constants tables)
  */
  llvm::LLVMContext llvm_context;
  /*
    Helper to generate llvm IR.
    Keeps track of the current place to insert instructions.
  */
  llvm::IRBuilder<> ir_builder{llvm_context};
  /*
    Contains functions and globals (it owns the memory for generated IR)
    I think this is like an transactional unit
  */
  std::unique_ptr<llvm::Module> module;

  Ast_File& file_ast;

public:
  LLVMCodeGen(Ast_File& file_ast);
  void create_module();

  /* Types */
  llvm::Type* map_primative_to_llvm(PrimativeTypeTag primative);
  llvm::Type* map_type_to_llvm(Type const * type);

  /* Functions */
  llvm::Function* get_function(Ast_Function_Declaration& func);
  llvm::Function* codegen_function_header(Ast_Function_Declaration& func);
  llvm::AllocaInst* create_entry_alloca(llvm::Function* func, Symbol* symbol);
  void codegen_function_body(Ast_Function_Declaration& func);

  /* Statements */
  void codegen_statement(Ast_Statement& stmt, Scope& scope);
  void codegen_statement(Ast_Block& stmt, Scope& scope);
  void codegen_statement(Ast_If_Statement& stmt, Scope& scope);
  void codegen_statement(Ast_Expression_Statement& stmt, Scope& scope);
  void codegen_statement(Ast_Return_Statement& stmt, Scope& scope);

  /* Expressions */
  llvm::Value* codegen_expression(Ast_Expression& expr, Scope& scope);
  llvm::Value* codegen_expression(Ast_Call& expr, Scope& scope);
  llvm::Value* codegen_expression(Ast_Literal& expr, Scope& scope);
  llvm::Value* codegen_expression(Ast_Identifier& expr, Scope& scope);
  llvm::Value* codegen_expression(Ast_Unary_Operation& expr, Scope& scope);
  llvm::Value* codegen_expression(Ast_Binary_Operation& expr, Scope& scope);
};
