#include <cassert>
#include <iterator>
#include <algorithm>

#include <mpark/patterns.hpp>

#include <llvm/Support/raw_ostream.h>

#include "codegen.h"
#include "llvm_codegen.h"

CodeGen::CodeGen(Ast_File& file_ast)
  : impl{std::make_unique<LLVMCodeGen>(file_ast)} {}

LLVMCodeGen::LLVMCodeGen(Ast_File& file_ast)
  : file_ast{file_ast}
{
  this->create_module();

  for (auto& func_type: file_ast.functions) {
    auto& func = std::get<Ast_Function_Declaration>(func_type->v);
    this->codegen_function_body(func);
  }
}

void LLVMCodeGen::create_module() {
  this->module = std::make_unique<llvm::Module>(
    file_ast.filename, llvm_context);
  /* TODO: set machine target */
  /* TODO: set up optimizations */
}

/* Types */

llvm::Type* LLVMCodeGen::map_primative_to_llvm(PrimativeTypeTag primative) {
  switch (primative) {
    case PrimativeTypeTag::INTEGER:
      return llvm::Type::getInt32Ty(llvm_context);
    case PrimativeTypeTag::FLOAT32:
      return llvm::Type::getFloatTy(llvm_context);
    case PrimativeTypeTag::FLOAT64:
      return llvm::Type::getDoubleTy(llvm_context);
    case PrimativeTypeTag::STRING:
      return llvm::Type::getInt8PtrTy(llvm_context);
    case PrimativeTypeTag::BOOL:
      return llvm::Type::getInt1Ty(llvm_context);
  }
}

llvm::Type* LLVMCodeGen::map_type_to_llvm(Type const * type) {
  using namespace mpark::patterns;
  if (!type) {
    return llvm::Type::getVoidTy(llvm_context);
  }
  return match(type->v)(
    pattern(as<PrimativeType>(arg)) = [&](auto const & primative){
      return map_primative_to_llvm(primative.tag);
    },
    pattern(_) = []{
      assert(false && "not implemented");
      return static_cast<llvm::Type*>(nullptr);
    }
  );
}

/* Functions */

llvm::Function* LLVMCodeGen::get_function(Ast_Function_Declaration& func) {
  if (auto llvm_func = module->getFunction(func.identifer.name)) {
    return llvm_func;
  } else {
    return codegen_function_header(func);
  }
}

llvm::Function* LLVMCodeGen::codegen_function_header(Ast_Function_Declaration& func) {
  std::vector<llvm::Type*> arg_types;
  std::transform(func.arguments.begin(), func.arguments.end(), std::back_inserter(arg_types),
    [&](auto const & arg) {
      return map_type_to_llvm(arg.type.get());
    });

  llvm::Type* return_type = map_type_to_llvm(func.return_type.get());

  llvm::FunctionType* func_type = llvm::FunctionType::get(
    return_type, arg_types, /*vararg:*/ false);

  llvm::Function* llvm_func = llvm::Function::Create(
    func_type,
    llvm::Function::ExternalLinkage,
    func.identifer.name,
    module.get());

  uint arg_idx = 0;
  for (auto& arg: llvm_func->args()) {
    arg.setName(func.arguments.at(arg_idx).name.name);
    ++arg_idx;
  }

  return llvm_func;
}

llvm::AllocaInst* LLVMCodeGen::create_entry_alloca(llvm::Function* func, Symbol* symbol) {
  /*
    Create an alloca for a (local) symbol at a functions entry.
    When using allocas for locals you must put them in the entry block
    for the mem2reg optimization to promote them to registers
  */
  llvm::IRBuilder<> entry_ir_builder(
    &func->getEntryBlock(),
    func->getEntryBlock().begin());

  assert("must be a local symbol" && symbol->kind == Symbol::LOCAL);

  llvm::Type* llvm_type = map_type_to_llvm(symbol->type.get());
  llvm::AllocaInst* alloca =  entry_ir_builder.CreateAlloca(
      llvm_type, /*array size:*/ 0, symbol->name.name);

  symbol->meta = std::make_shared<SymbolMetaLocal>(alloca);

  return alloca;
}

void LLVMCodeGen::codegen_function_body(Ast_Function_Declaration& func) {
  llvm::Function* llvm_func = this->get_function(func);

  assert("function must not already be generated" && llvm_func->empty());

  // Create & insert body block (entry block)
  llvm::BasicBlock* function_body = llvm::BasicBlock::Create(
    llvm_context, "body", llvm_func);
  ir_builder.SetInsertPoint(function_body);

  // Create allocas for the locals (the arguments) & store them
  for (auto& arg: llvm_func->args()) {
    Symbol* local_arg = func.body.scope.lookup_first(std::string(arg.getName()));
    llvm::AllocaInst* arg_alloca = create_entry_alloca(llvm_func, local_arg);
    // Store passed argument
    ir_builder.CreateStore(&arg, arg_alloca);
  }

  llvm_func->print(llvm::errs());
}

/* Statements */

void LLVMCodeGen::codegen_statement(Ast_Statement& stmt, Scope& scope) {
  std::visit([&](auto& stmt) {
    codegen_statement(stmt, scope);
  }, stmt.v);
}

void LLVMCodeGen::codegen_statement(Ast_Block& stmt, Scope& scope) {

}

void LLVMCodeGen::codegen_statement(Ast_If_Statement& stmt, Scope& scope) {

}

void LLVMCodeGen::codegen_statement(Ast_Expression_Statement& stmt, Scope& scope) {

}

void LLVMCodeGen::codegen_statement(Ast_Return_Statement& stmt, Scope& scope) {

}

/* Expressions */

llvm::Value* codegen_expression(Ast_Expression& expr, Scope& scope) {

}

llvm::Value* codegen_expression(Ast_Call& expr, Scope& scope) {

}

llvm::Value* codegen_expression(Ast_Literal& expr, Scope& scope) {

}

llvm::Value* codegen_expression(Ast_Identifier& expr, Scope& scope) {

}

llvm::Value* codegen_expression(Ast_Unary_Operation& expr, Scope& scope) {

}

llvm::Value* codegen_expression(Ast_Binary_Operation& expr, Scope& scope) {

}
