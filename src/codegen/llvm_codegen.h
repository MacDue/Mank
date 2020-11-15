#pragma once

#include <stack>
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

  struct SymbolMetaCompoundType : SymbolMeta {
    llvm::Type* type;

    SymbolMetaCompoundType(llvm::Type* type)
      : type{type} {}
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

  /* Codegen state */

  std::optional<llvm::orc::VModuleKey> jit_module_handle;
  std::unique_ptr<llvm::orc::KaleidoscopeJIT> jit_engine;

  Ast_File& file_ast;

  struct ClosureInfo {
    Scope* parent;
    Closure* closure;
    llvm::Type* closure_type;
    llvm::Value* closure_ptr = nullptr;
  };

  std::stack<ClosureInfo> current_closure_info;

  /* LLVM Helpers */

  bool block_terminates_here();

  void create_exit_br(llvm::BasicBlock* target);

  llvm::orc::VModuleKey jit_current_module();

  void create_module();

  llvm::Function* get_gc_malloc();

  llvm::Value* create_llvm_idx(uint value);

  Ast_Expression& flatten_nested_pod_accesses(
    Ast_Field_Access& access, std::vector<uint>& idx_list);
  std::vector<llvm::Value*> make_idx_list_for_gep(
    std::vector<uint> const & idx_list);

  llvm::Value* get_local(Ast_Identifier& ident, Scope& scope);

  // Expression value extractor to help dealing with lvalues and rvalues
  class ExpressionExtract {
    LLVMCodeGen* codegen;
    bool is_lvalue;
    llvm::Value* value_or_address;
public:
    ExpressionExtract(LLVMCodeGen* codegen, Ast_Expression& expr, Scope& scope);
    llvm::Value* get_value(
      std::vector<unsigned> const & idx_list, llvm::Twine const & name);
  };

  inline ExpressionExtract get_value_extractor(Ast_Expression& expr, Scope& scope) {
    return ExpressionExtract(this, expr, scope);
  }

  /* Types */
  std::vector<llvm::Type*> map_arg_types_to_llvm(
    std::vector<Ast_Argument> const & args, Scope& scope);

  llvm::Type* map_lambda_type_to_llvm(LambdaType const & lambda_type, Scope& scope);
  llvm::Type* map_pod_to_llvm(Ast_Pod_Declaration const & pod_type, Scope& scope);
  llvm::Type* map_primative_to_llvm(PrimativeType::Tag primative);
  llvm::Type* map_type_to_llvm(Type const * type, Scope& scope);

  /* Functions */
  llvm::Function* get_current_function();
  llvm::Function* get_function(Ast_Function_Declaration& func);
  llvm::Function* codegen_function_header(Ast_Function_Declaration& func);
  llvm::AllocaInst* create_entry_alloca(
    llvm::Function* func, Scope& scope, Type* type, std::string name);
  llvm::AllocaInst* create_entry_alloca(llvm::Function* func, Symbol* symbol);
  void codegen_function_body(Ast_Function_Declaration& func, llvm::Function* llvm_func = nullptr);

  /* Statements */
  void codegen_statement(Ast_Statement& stmt, Scope& scope);
  void codegen_statement(Ast_Expression_Statement& expr_stmt, Scope& scope);
  void codegen_statement(Ast_Return_Statement& return_stmt, Scope& scope);
  void codegen_statement(Ast_Assign& assign, Scope& scope);
  void codegen_statement(Ast_Variable_Declaration& var_decl, Scope& scope);
  void codegen_statement(Ast_For_Loop& for_loop, Scope& scope);

  Ast_Expression& flatten_nested_array_indexes(
    Ast_Index_Access& index, Scope& scope, std::vector<llvm::Value*>& idx_list);

  void initialize_array(llvm::Value* array_ptr, Ast_Array_Literal& values, Scope& scope);

  llvm::Value* address_of(Ast_Expression& expr, Scope& scope);
  llvm::Value* codegen_bind(Ast_Expression& expr, Type* bound_to, Scope& scope);

  /* Expressions */
  llvm::Value* codegen_expression(Ast_Expression& expr, Scope& scope, bool as_lvalue = false);
  llvm::Value* codegen_expression(Ast_Block& block, Scope& scope, bool as_lvalue = false);
  llvm::Value* codegen_expression(Ast_If_Expr& if_stmt, Scope& scope, bool as_lvalue = false);
  llvm::Value* codegen_expression(Ast_Call& call, Scope& scope);
  llvm::Value* codegen_expression(Ast_Literal& literal, Scope& scope);
  llvm::Value* codegen_expression(Ast_Identifier& ident, Scope& scope);
  llvm::Value* codegen_expression(Ast_Unary_Operation& unary, Scope& scope);
  llvm::Value* codegen_expression(Ast_Binary_Operation& binop, Scope& scope);
  llvm::Value* codegen_expression(Ast_Field_Access& access, Scope& scope);
  llvm::Value* codegen_expression(Ast_Array_Literal& array, Scope& scope);
  llvm::Value* codegen_expression(Ast_Index_Access& index, Scope& scope);
  llvm::Value* codegen_expression(Ast_Lambda& lambda, Scope& scope);

  inline llvm::Value* codegen_expression(Ast_Macro_Identifier& lambda, Scope& scope) {
    assert(false && "??? there should not be any macros left at this stage!");
  }

public:
  LLVMCodeGen(Ast_File& file_ast);
  /* JIT tools */
  void* jit_find_symbol(std::string name);

  ~LLVMCodeGen();
};
