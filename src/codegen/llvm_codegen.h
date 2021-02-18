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
#include "ast/ast_builder.h"

#include "codegen/code_generator.h"

class LLVMCodeGen: public CodeGenerator {

  struct SymbolMetaLocal: SymbolMeta {
    llvm::Value* alloca;

    SymbolMetaLocal(llvm::Value* alloca): alloca{alloca} {}
  };

  struct SymbolMetaReturn: SymbolMetaLocal {
    llvm::BasicBlock* return_block;

    SymbolMetaReturn(llvm::AllocaInst* return_value, llvm::BasicBlock* return_block)
      : SymbolMetaLocal(return_value), return_block{return_block} {}
  };

  struct SymbolMetaCompoundType: SymbolMeta {
    llvm::Type* type;

    SymbolMetaCompoundType(llvm::Type* type)
      : type{type} {}
  };

  struct SymbolMetaBoundsCheck: SymbolMeta {
    // Hack to insert bounds checking code into the AST
    SourceLocation location;
    llvm::Value *length, *index;

    SymbolMetaBoundsCheck(SourceLocation location,
      llvm::Value* length, llvm::Value* index
    ): location{location}, length{length}, index{index} {}
  };

  Ast_File& file_ast;
  llvm::Value* source_filename = nullptr;

  llvm::Value* get_source_filename();

  /*
    Owns mank datastructures.
  */
  AstContext& mank_ctx;

  /*
    Helps generate new mank _things_.
  */
  AstBuilder ast_builder;

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

  struct ClosureInfo {
    Scope* parent;
    Closure* closure;
    llvm::Type* closure_type;
    llvm::Value* closure_ptr = nullptr;
  };
  std::stack<ClosureInfo> current_closure_info;

  struct LoopInfo {
    llvm::BasicBlock *loop_end = nullptr, *loop_head = nullptr;
  };
  std::stack<LoopInfo> current_loop_info;

  inline void push_loop_info(llvm::BasicBlock* loop_end, llvm::BasicBlock* loop_head) {
    current_loop_info.push(LoopInfo{
      .loop_end = loop_end,
      .loop_head = loop_head
    });
  }

  inline void pop_loop_info() { current_loop_info.pop(); }

  LoopInfo& get_loop_info() {
    assert(!current_loop_info.empty());
    return current_loop_info.top();
  }

  /* LLVM Helpers */

  bool block_terminates_here();

  void create_exit_br(llvm::BasicBlock* target);

  llvm::orc::VModuleKey jit_current_module();

  void create_module();

  llvm::GlobalVariable* create_global(std::string const & name, llvm::Type* type);

  llvm::Function* get_external(
    llvm::StringRef name,
    llvm::Type* return_type,
    llvm::ArrayRef<llvm::Type*> arg_types);

  llvm::Function* get_gc_malloc();
  llvm::Function* get_str_concat_internal();

  llvm::Function* get_init_vec(Scope& scope);
  llvm::Function* get_vec_push_back(Scope& scope);
  llvm::Function* get_vec_pop_back(Scope& scope);
  llvm::Function* get_vec_fill(Scope& scope);

  llvm::Function* get_bounds_error();

  llvm::Type* get_string_ty(Scope& scope);

  llvm::Constant* create_const_string_initializer(std::string value);
  llvm::Constant* create_const_string(std::string value, Scope& scope);
  llvm::Value* create_const_string_global(
    std::string value, std::string const & name, Scope& scope);

  llvm::Value* create_string_concat(
    Ast_Expression& s1, Ast_Expression& s2, Scope& scope);
  llvm::Value* create_char_string_cast(llvm::Value* char_value, Scope& scope);

  /* length/char ptr */
  std::pair<llvm::Value*, llvm::Value*> extract_string_info(
    Ast_Expression& expr, Scope& scope);

  llvm::Value* fix_string_length(llvm::Value* length);

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
    ExpressionExtract(LLVMCodeGen* codegen, llvm::Value* value, bool is_lvalue);

    llvm::Value* get_value(
      std::vector<unsigned> const & idx_list, llvm::Twine const & name);

    llvm::Value* get_bind(
      std::vector<unsigned> const & idx_list, llvm::Twine const & name);

    llvm::Value* get_bind(
      std::vector<unsigned> const & idx_list, Type_Ptr bound_to, llvm::Twine const & name);
  };

  inline ExpressionExtract get_value_extractor(Ast_Expression& expr, Scope& scope) {
    return ExpressionExtract(this, expr, scope);
  }

  /* Types */
  std::vector<llvm::Type*> map_arg_types_to_llvm(
    std::vector<Ast_Argument> const & args, Scope& scope);

  llvm::Type* get_vector_ty(Scope& scope);

  llvm::Type* map_lambda_type_to_llvm(LambdaType const & lambda_type, Scope& scope);
  llvm::Type* map_pod_to_llvm(Ast_Pod_Declaration const & pod_type, Scope& scope);
  llvm::Type* map_primative_to_llvm(PrimativeType::Tag primative);
  llvm::Type* map_type_to_llvm(Type const * type, Scope& scope);

  /* Functions */
  llvm::Function* get_current_function();
  llvm::Function* get_function(Ast_Function_Declaration& func);
  llvm::Function* codegen_function_header(Ast_Function_Declaration& func);
  llvm::AllocaInst* create_entry_alloca(
    llvm::Function* func, llvm::Type* type, std::string name);
  llvm::AllocaInst* create_entry_alloca(
    llvm::Function* func, Scope& scope, Type* type, std::string name);
  llvm::AllocaInst* create_entry_alloca(llvm::Function* func, Symbol* symbol);
  void codegen_function_body(Ast_Function_Declaration& func, llvm::Function* llvm_func = nullptr);

  llvm::Value* create_heap_alloc(
    llvm::Type* type, llvm::Twine const & name, llvm::Value** raw_ptr = nullptr);

  ExpressionExtract get_tuple_extractor(Ast_Expression& tuple, Scope& scope);

  void codegen_tuple_assign(
    Ast_Tuple_Literal& tuple_pattern, ExpressionExtract& tuple, std::vector<unsigned> idxs, Scope& scope);

  /* Statements */
  void codegen_statement(Ast_Statement& stmt, Scope& scope);
  void codegen_statement(Ast_Expression_Statement& expr_stmt, Scope& scope);
  void codegen_statement(Ast_Return_Statement& return_stmt, Scope& scope);
  void codegen_statement(Ast_Assign& assign, Scope& scope);
  void codegen_statement(Ast_Variable_Declaration& var_decl, Scope& scope);
  void codegen_statement(Ast_For_Loop& for_loop, Scope& scope);
  void codegen_statement(Ast_Loop& loop, Scope& scope);
  void codegen_statement(Ast_While_Loop& while_loop, Scope& scope);
  void codegen_statement(Ast_Loop_Control& loop_control, Scope& scope);
  void codegen_statement(Ast_Constant_Declaration& const_decl, Scope& scope);

  void codegen_value_bind(
    Ast_Bind& bind,
    Type_Ptr value_type,
    Ast_Identifier& bound_name,
    ExpressionExtract& aggregate,
    std::vector<unsigned> const & idxs,
    Scope& scope,
    llvm::Twine const & codename);

  void codegen_value_bind(Ast_Bind& bind, Ast_Identifier& bound_name, llvm::Value* value, Scope& scope);

  void codegen_tuple_bindings(
    Ast_Tuple_Binds& tuple_binds, ExpressionExtract& tuple, std::vector<unsigned> idxs, Scope& scope);

  void codegen_pod_bindings(
    Ast_Pod_Binds& pod_binds, ExpressionExtract& pod, std::vector<unsigned> idxs, Scope& scope);

  void codegen_statement(Ast_Structural_Binding& binding, Scope& scope);

  Ast_Expression& flatten_nested_array_indexes(
    Ast_Index_Access& index, Scope& scope, std::vector<llvm::Value*>& idx_list);

  void initialize_aggregate(llvm::Value* ptr, Ast_Expression_List& values, Scope& scope);
  void initialize_pod(llvm::Value* ptr, Ast_Pod_Literal& initializer, Scope& scope);

  llvm::Value* get_vector_length(llvm::Value* data_ptr);

  llvm::Value* get_special_field_value(Type_Ptr agg_type, Ast_Expression& agg, Scope& scope);

  llvm::Value* dereference(llvm::Value* value, Type_Ptr type);
  llvm::Value* address_of(Ast_Expression& expr, Scope& scope);
  llvm::Value* codegen_bind(Ast_Expression& expr, Type_Ptr bound_to, Scope& scope);

  llvm::Value* create_lambda(llvm::Type* lambda_type, llvm::Function* body, llvm::Value* env_ptr);
  llvm::Value* create_string(llvm::Value* raw_str_ptr, llvm::Value* length, Scope& scope);

  llvm::Value* index_vector(Ast_Index_Access vector_index, Scope& scope);

  llvm::Value* do_cast(llvm::Value* value, Type_Ptr source_type, Type_Ptr target_type, Scope& scope);

  Expr_Ptr simplify_short_circuit(Ast_Binary_Operation& short_circuit);

  Expr_Ptr mank_builtin_array_set(Type_Ptr array_type, Expr_Ptr initializer, Scope& scope);

  void raise_mank_builtin_bounds_error(
    llvm::Value* length, llvm::Value* index,
    SourceLocation const & location);

  void insert_bounds_check(
    llvm::Value* length, llvm::Value* index,
    Ast_Index_Access const & access, Scope& scope);

  llvm::Value* codegen_builtin_vector_calls(
    Ast_Call& call, Ast_Function_Declaration& func_type, Scope& scope);

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
  llvm::Value* codegen_expression(Ast_Expression_List& array_like, Scope& scope);
  llvm::Value* codegen_expression(Ast_Index_Access& index, Scope& scope);
  llvm::Value* codegen_expression(Ast_Lambda& lambda, Scope& scope);
  llvm::Value* codegen_expression(Ast_Pod_Literal& pod, Scope& scope);
  llvm::Value* codegen_expression(Ast_As_Cast& as_cast, Scope& scope);
  llvm::Value* codegen_expression(Ast_Array_Repeat& array_repeat, Scope& scope);
  llvm::Value* codegen_expression(Ast_Spawn& spawn, Scope& scope);

  inline llvm::Value* codegen_expression(Ast_Macro_Identifier& macro, Scope& scope) {
    (void) macro; (void) scope;
    assert(false && "??? there should not be any macros left at this stage!");
  }

  inline llvm::Value* codegen_expression(Ast_Specialized_Identifier& special_ident, Scope& scope) {
    (void) special_ident; (void) scope;
    assert(false && "??? don't think special idents need codegen!");
  }

  inline llvm::Value* codegen_expression(Ast_Path& path, Scope& scope) {
    (void) path; (void) scope;
    assert(false && "??? path codegen");
  }

public:
  LLVMCodeGen(Ast_File& file_ast);
  /* JIT tools */
  void* jit_find_symbol(std::string name);

  ~LLVMCodeGen();
};
