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
  arg_types.reserve(func.arguments.size());

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

  codegen_statement(func.body, func.body.scope);

  llvm_func->print(llvm::errs());
}

/* Statements */

void LLVMCodeGen::codegen_statement(Ast_Statement& stmt, Scope& scope) {
  std::visit([&](auto& stmt) {
    codegen_statement(stmt, scope);
  }, stmt.v);
}

void LLVMCodeGen::codegen_statement(Ast_Block& block, Scope& scope) {
  for (auto& stmt: block.statements) {
    codegen_statement(*stmt, block.scope);
  }
}

void LLVMCodeGen::codegen_statement(Ast_If_Statement& if_stmt, Scope& scope) {

}

void LLVMCodeGen::codegen_statement(Ast_Expression_Statement& expr_stmt, Scope& scope) {
  (void) codegen_expression(*expr_stmt.expression, scope);
}

void LLVMCodeGen::codegen_statement(Ast_Return_Statement& return_stmt, Scope& scope) {
  ir_builder.CreateRet(
    codegen_expression(*return_stmt.expression, scope));
}

/* Expressions */

llvm::Value* LLVMCodeGen::codegen_expression(Ast_Expression& expr, Scope& scope) {
  std::visit([&](auto& expr) {
    codegen_expression(expr, scope);
  }, expr.v);
}

llvm::Value* LLVMCodeGen::codegen_expression(Ast_Call& call, Scope& scope) {
  auto& called_function_ident = std::get<Ast_Identifier>(call.callee->v);

  Symbol* called_function = scope.lookup_first_name(called_function_ident);
  auto& function_type = std::get<Ast_Function_Declaration>(called_function->type->v);

  llvm::Function* callee = this->get_function(function_type);

  std::vector<llvm::Value*> call_args;
  call_args.reserve(call.arguments.size());

  for (auto& arg: call.arguments) {
    call_args.push_back(codegen_expression(*arg, scope));
  }

  return ir_builder.CreateCall(callee, call_args, "call_ret");
}

llvm::Value* LLVMCodeGen::codegen_expression(Ast_Literal& literal, Scope& scope) {
  /* TODO: better parsing */
  switch (literal.literal_type) {
    case PrimativeTypeTag::INTEGER:
      return llvm::ConstantInt::get(llvm_context,
        llvm::APInt(/*bits:*/ 32, /*value:*/ std::stoi(literal.value), /*signed:*/ true));
    case PrimativeTypeTag::FLOAT32:
      return llvm::ConstantFP::get(llvm_context,
        llvm::APFloat(/*value:*/ std::stof(literal.value)));
    case PrimativeTypeTag::FLOAT64:
        llvm::ConstantFP::get(llvm_context,
          llvm::APFloat(/*value:*/ std::stod(literal.value)));
    case PrimativeTypeTag::STRING:
      assert(false && "string literal codegen not implemented");
      return nullptr;
    case PrimativeTypeTag::BOOL:
      return llvm::ConstantInt::get(llvm_context,
        llvm::APInt(/*bits*/ 1,
                    /*value: */ literal.value == "true" ? 1 : 0,
                    /*signed: */ false));
  }
}

llvm::Value* LLVMCodeGen::codegen_expression(Ast_Identifier& ident, Scope& scope) {
  Symbol* symbol = scope.lookup_first_name(ident);
  assert(symbol->kind == Symbol::LOCAL && "only locals implemented");

  using namespace mpark::patterns;
  return match(symbol->meta)(
    pattern(some(as<SymbolMetaLocal>(arg))) = [&](auto& meta) {
      return meta.value;
    },
    pattern(_) = []{
      assert(false && "fix me! unknown symbol meta");
      return static_cast<llvm::Value*>(nullptr);
    }
  );
}

llvm::Value* LLVMCodeGen::codegen_expression(Ast_Unary_Operation& unary, Scope& scope) {
  llvm::Value* operand = codegen_expression(*unary.operand, scope);

  // switch (unary.operation) {
  //   case /* constant-expression */:
  //     /* code */
  //     break;

  //   default:
  //     break;
  // }

}

llvm::Value* LLVMCodeGen::codegen_expression(Ast_Binary_Operation& binop, Scope& scope) {
  using namespace mpark::patterns;
  auto& binop_type = std::get<PrimativeType>(binop.left->type->v);
  /* this could better */
  return match(binop_type.tag, binop.operation)(
    /* Basic integer operations */
    pattern(PrimativeTypeTag::INTEGER, Ast_Operator::PLUS) = [&]{
      return ir_builder.CreateAdd(
        codegen_expression(*binop.left, scope),
        codegen_expression(*binop.right, scope),
        "int_add");
    },
    pattern(PrimativeTypeTag::INTEGER, Ast_Operator::MINUS) = [&]{
      return ir_builder.CreateSub(
        codegen_expression(*binop.left, scope),
        codegen_expression(*binop.right, scope),
        "int_sub");
    },
    pattern(PrimativeTypeTag::INTEGER, Ast_Operator::DIVIDE) = [&]{
      return ir_builder.CreateSDiv(
        codegen_expression(*binop.left, scope),
        codegen_expression(*binop.right, scope),
        "int_div");
    },
    pattern(PrimativeTypeTag::INTEGER, Ast_Operator::DIVIDE) = [&]{
      return ir_builder.CreateMul(
        codegen_expression(*binop.left, scope),
        codegen_expression(*binop.right, scope),
        "int_mult");
    },
    pattern(PrimativeTypeTag::INTEGER, Ast_Operator::DIVIDE) = [&]{
      return ir_builder.CreateSRem(
        codegen_expression(*binop.left, scope),
        codegen_expression(*binop.right, scope),
        "int_mod");
    },
    pattern(PrimativeTypeTag::INTEGER, Ast_Operator::LESS_THAN) = [&]{
      return ir_builder.CreateICmpSLT(
        codegen_expression(*binop.left, scope),
        codegen_expression(*binop.right, scope),
        "int_lt");
    },
    pattern(PrimativeTypeTag::INTEGER, Ast_Operator::GREATER_THAN) = [&]{
      return ir_builder.CreateICmpUGT(
        codegen_expression(*binop.left, scope),
        codegen_expression(*binop.right, scope),
        "int_gt");
    },
    pattern(PrimativeTypeTag::INTEGER, Ast_Operator::BITWISE_AND) = [&]{
      return ir_builder.CreateAnd(
        codegen_expression(*binop.left, scope),
        codegen_expression(*binop.right, scope),
        "int_bitwise_and");
    },
    pattern(PrimativeTypeTag::INTEGER, Ast_Operator::BITWISE_OR) = [&]{
      return ir_builder.CreateOr(
        codegen_expression(*binop.left, scope),
        codegen_expression(*binop.right, scope),
        "int_bitwise_or");
    },
    pattern(PrimativeTypeTag::INTEGER, Ast_Operator::GREATER_EQUAL) = [&]{
      return ir_builder.CreateICmpSGE(
        codegen_expression(*binop.left, scope),
        codegen_expression(*binop.right, scope),
        "int_ge");
    },
    pattern(PrimativeTypeTag::INTEGER, Ast_Operator::LESS_EQUAL) = [&]{
      return ir_builder.CreateICmpSLE(
        codegen_expression(*binop.left, scope),
        codegen_expression(*binop.right, scope),
        "int_le");
    },
    pattern(PrimativeTypeTag::INTEGER, Ast_Operator::EQUAL_TO) = [&]{
      return ir_builder.CreateICmpEQ(
        codegen_expression(*binop.left, scope),
        codegen_expression(*binop.right, scope),
        "int_eq");
    },
    pattern(PrimativeTypeTag::INTEGER, Ast_Operator::NOT_EQUAL_TO) = [&]{
      return ir_builder.CreateICmpNE(
        codegen_expression(*binop.left, scope),
        codegen_expression(*binop.right, scope),
        "int_ne");
    },
    /*
      Basic float operations
      TODO: Understand floating point ordering!
      Read: https://stackoverflow.com/a/38516544/3818491
      Something to do with NaN (just using unordered operations for now)
    */
    pattern(PrimativeTypeTag::FLOAT64, Ast_Operator::PLUS) = [&]{
      return ir_builder.CreateFAdd(
        codegen_expression(*binop.left, scope),
        codegen_expression(*binop.right, scope),
        "float_add");
    },
    pattern(PrimativeTypeTag::FLOAT64, Ast_Operator::MINUS) = [&]{
      return ir_builder.CreateFSub(
        codegen_expression(*binop.left, scope),
        codegen_expression(*binop.right, scope),
        "float_sub");
    },
    pattern(PrimativeTypeTag::FLOAT64, Ast_Operator::DIVIDE) = [&]{
      return ir_builder.CreateFDiv(
        codegen_expression(*binop.left, scope),
        codegen_expression(*binop.right, scope),
        "float_div");
    },
    pattern(PrimativeTypeTag::FLOAT64, Ast_Operator::DIVIDE) = [&]{
      return ir_builder.CreateFMul(
        codegen_expression(*binop.left, scope),
        codegen_expression(*binop.right, scope),
        "float_mult");
    },
    pattern(PrimativeTypeTag::FLOAT64, Ast_Operator::LESS_THAN) = [&]{
      return ir_builder.CreateFCmpULT(
        codegen_expression(*binop.left, scope),
        codegen_expression(*binop.right, scope),
        "float_lt");
    },
    pattern(PrimativeTypeTag::FLOAT64, Ast_Operator::GREATER_THAN) = [&]{
      return ir_builder.CreateFCmpUGT(
        codegen_expression(*binop.left, scope),
        codegen_expression(*binop.right, scope),
        "float_gt");
    },
    pattern(PrimativeTypeTag::FLOAT64, Ast_Operator::GREATER_EQUAL) = [&]{
      return ir_builder.CreateFCmpUGE(
        codegen_expression(*binop.left, scope),
        codegen_expression(*binop.right, scope),
        "float_ge");
    },
    pattern(PrimativeTypeTag::FLOAT64, Ast_Operator::LESS_EQUAL) = [&]{
      return ir_builder.CreateFCmpULE(
        codegen_expression(*binop.left, scope),
        codegen_expression(*binop.right, scope),
        "float_le");
    },
    pattern(PrimativeTypeTag::FLOAT64, Ast_Operator::EQUAL_TO) = [&]{
      return ir_builder.CreateFCmpUEQ(
        codegen_expression(*binop.left, scope),
        codegen_expression(*binop.right, scope),
        "float_eq");
    },
    pattern(PrimativeTypeTag::FLOAT64, Ast_Operator::NOT_EQUAL_TO) = [&]{
      return ir_builder.CreateFCmpUNE(
        codegen_expression(*binop.left, scope),
        codegen_expression(*binop.right, scope),
        "float_ne");
    }
  );
}
