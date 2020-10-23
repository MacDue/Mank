#pragma once

#include <string>
#include <optional>

/* Shut up GCC warning about LLVM code */
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wreturn-type"
#pragma GCC diagnostic ignored "-Wredundant-move"
#pragma GCC diagnostic ignored "-Wunused-parameter"

#include <llvm/IR/Value.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Support/TargetSelect.h>

#include <llvm/IR/Verifier.h>
#include <llvm/Support/raw_ostream.h>

// TODO: Replace with my own JIT
// TODO: Don't jit here make a friend class
#include "kaleidoscope_jit.h"
#pragma GCC diagnostic pop

#include "ast/ast.h"
#include "ast/scope.h"
#include "codegen/code_generator.h"

class LLVMCodeGen: public CodeGenerator {

  struct SymbolMetaLocal: SymbolMeta {
    llvm::AllocaInst* alloca;

    SymbolMetaLocal(llvm::AllocaInst* alloca)
      : alloca{alloca} {}
  };

  struct SymbolMetaReturn: SymbolMetaLocal {
    llvm::BasicBlock* return_block;

    SymbolMetaReturn(llvm::AllocaInst* return_value, llvm::BasicBlock* return_block)
      : SymbolMetaLocal(return_value), return_block{return_block} {}
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
  std::unique_ptr<llvm::Module> llvm_module;

  std::optional<llvm::orc::VModuleKey> jit_module_handle;
  std::unique_ptr<llvm::orc::KaleidoscopeJIT> jit_engine;

  Ast_File& file_ast;

  bool block_terminates_here();

  void create_exit_br(llvm::BasicBlock* target);

  llvm::orc::VModuleKey jit_current_module();
public:
  LLVMCodeGen(Ast_File& file_ast);
  void create_module();

  /* Types */
  llvm::Type* map_primative_to_llvm(PrimativeType::Tag primative);
  llvm::Type* map_type_to_llvm(Type const * type);

  /* Functions */
  llvm::Function* get_current_function();
  llvm::Function* get_function(Ast_Function_Declaration& func);
  llvm::Function* codegen_function_header(Ast_Function_Declaration& func);
  llvm::AllocaInst* create_entry_alloca(llvm::Function* func, Symbol* symbol);
  void codegen_function_body(Ast_Function_Declaration& func);

  /* Statements */
  void codegen_statement(Ast_Statement& stmt, Scope& scope);
  void codegen_statement(Ast_Expression_Statement& expr_stmt, Scope& scope);
  void codegen_statement(Ast_Return_Statement& return_stmt, Scope& scope);
  void codegen_statement(Ast_Assign& assign, Scope& scope);
  void codegen_statement(Ast_Variable_Declaration& var_decl, Scope& scope);
  void codegen_statement(Ast_For_Loop& for_loop, Scope& scope);

  /* Expressions */
  llvm::Value* codegen_expression(Ast_Expression& expr, Scope& scope);
  llvm::Value* codegen_expression(Ast_Block& block, Scope& scope);
  llvm::Value* codegen_expression(Ast_If_Expr& if_stmt, Scope& scope);
  llvm::Value* codegen_expression(Ast_Call& call, Scope& scope);
  llvm::Value* codegen_expression(Ast_Literal& literal, Scope& scope);
  llvm::Value* codegen_expression(Ast_Identifier& ident, Scope& scope);
  llvm::Value* codegen_expression(Ast_Unary_Operation& unary, Scope& scope);
  llvm::Value* codegen_expression(Ast_Binary_Operation& binop, Scope& scope);
  llvm::Value* codegen_expression(Ast_Field_Access& access, Scope& scope);

  /* JIT tools */
  void* jit_find_symbol(std::string name);

  ~LLVMCodeGen();
};
