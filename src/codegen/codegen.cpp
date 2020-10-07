#include <cassert>
#include <iterator>
#include <algorithm>

#include <mpark/patterns.hpp>
#include <formatxx/std_string.h>

#include "codegen.h"
#include "llvm_codegen.h"
#include "token_helpers.h"

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
    default:
      assert(false && "fix me! mapping unknown primative to LLVM");
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

#define FUNCTION_RETURN_LOCAL "!return_value"

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

  llvm::AllocaInst* return_alloca = nullptr;
  if (/* functions */ !func.procedure) {
    // Create function return value
    func.body.scope.symbols.emplace_back(Symbol(
      SymbolName(FUNCTION_RETURN_LOCAL),
      func.return_type,
      Symbol::LOCAL));
    return_alloca = create_entry_alloca(
      llvm_func, &func.body.scope.symbols.back());
  }

  codegen_statement(func.body, func.body.scope);

  llvm::Value* return_value = nullptr;
  if (return_alloca) {
    return_value = ir_builder.CreateLoad(return_alloca, "load_return_value");
  }

  ir_builder.CreateRet(return_value);

  llvm_func->print(llvm::errs());
  llvm::errs() << '\n';

  assert("function should be valid IR" && !llvm::verifyFunction(*llvm_func, &llvm::errs()));
}

/* Statements */

void LLVMCodeGen::codegen_statement(Ast_Statement& stmt, Scope& scope) {
  std::visit([&](auto& stmt) {
    codegen_statement(stmt, scope);
  }, stmt.v);
}

void LLVMCodeGen::codegen_statement(Ast_Block& block, Scope& scope) {
  (void) scope; // Not needed
  for (auto& stmt: block.statements) {
    codegen_statement(*stmt, block.scope);
  }
}

void LLVMCodeGen::codegen_statement(Ast_If_Statement& if_stmt, Scope& scope) {
  llvm::Function* current_function = ir_builder.GetInsertBlock()->getParent();
  llvm::Value* condition = codegen_expression(*if_stmt.cond, scope);

  /* Create and insert the 'then' block into the function */
  llvm::BasicBlock* then_block = llvm::BasicBlock::Create(
    llvm_context, "then_block", current_function);

  llvm::BasicBlock* else_block = nullptr;

  llvm::BasicBlock* end_block = llvm::BasicBlock::Create(
    llvm_context, "if_end");

  if (if_stmt.has_else) {
    else_block = llvm::BasicBlock::Create(llvm_context, "else_block");
    ir_builder.CreateCondBr(condition, then_block, else_block);
  } else {
    ir_builder.CreateCondBr(condition, then_block, end_block);
  }

  /* then */
  ir_builder.SetInsertPoint(then_block);
  codegen_statement(*if_stmt.then_block, scope);
  ir_builder.CreateBr(end_block);

  /* else */
  if (if_stmt.has_else) {
    // Now add the else block!

    current_function->getBasicBlockList().push_back(else_block);
    ir_builder.SetInsertPoint(else_block);
    codegen_statement(*if_stmt.else_block, scope);
    ir_builder.CreateBr(end_block);
  }

  /* end */
  current_function->getBasicBlockList().push_back(end_block);
  ir_builder.SetInsertPoint(end_block);
}

void LLVMCodeGen::codegen_statement(Ast_Expression_Statement& expr_stmt, Scope& scope) {
  (void) codegen_expression(*expr_stmt.expression, scope);
}

void LLVMCodeGen::codegen_statement(Ast_Return_Statement& return_stmt, Scope& scope) {
  Symbol* return_local = scope.lookup_first(FUNCTION_RETURN_LOCAL);
  assert(return_local && "return local must exist to codegen return statement!");

  // If this is not a SymbolMetaLocal something is _very_ wrong!
  auto return_meta = static_cast<SymbolMetaLocal*>(return_local->meta.get());

  llvm::Value* return_value = codegen_expression(*return_stmt.expression, scope);
  ir_builder.CreateStore(return_value, return_meta->alloca);
}

/* Expressions */

llvm::Value* LLVMCodeGen::codegen_expression(Ast_Expression& expr, Scope& scope) {
  return std::visit([&](auto& expr) {
    return codegen_expression(expr, scope);
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

  return ir_builder.CreateCall(callee, call_args,
    function_type.procedure ? "" : "call_ret");
}

llvm::Value* LLVMCodeGen::codegen_expression(Ast_Literal& literal, Scope& scope) {
  (void) scope; // Not needed
  switch (literal.literal_type) {
    case PrimativeTypeTag::INTEGER:
      return llvm::ConstantInt::get(llvm_context,
        llvm::APInt(
          /*bits:*/ literal.size_bytes(),
          /*value:*/ literal.as_int32(),
          /*signed:*/ true));
    case PrimativeTypeTag::FLOAT32:
      return llvm::ConstantFP::get(llvm_context,
        llvm::APFloat(/*value:*/ literal.as_float32()));
    case PrimativeTypeTag::FLOAT64:
      return llvm::ConstantFP::get(llvm_context,
          llvm::APFloat(/*value:*/ literal.as_float64()));
    case PrimativeTypeTag::STRING:
      assert(false && "string literal codegen not implemented");
      return nullptr;
    case PrimativeTypeTag::BOOL:
      return llvm::ConstantInt::get(llvm_context,
        llvm::APInt(/*bits*/ literal.size_bytes(),
                    /*value: */ literal.as_bool(),
                    /*signed: */ false));
    default:
      assert(false && "fix me! codegen for unknown literal type");
  }
}

llvm::Value* LLVMCodeGen::codegen_expression(Ast_Identifier& ident, Scope& scope) {
  using namespace mpark::patterns;
  Symbol* symbol = scope.lookup_first_name(ident);
  assert(symbol->kind == Symbol::LOCAL && "only locals implemented");

  return match(symbol->meta)(
    pattern(some(as<SymbolMetaLocal>(arg))) = [&](auto& meta) {
      auto load_inst = ir_builder.CreateLoad(meta.alloca,
        formatxx::format_string("load_{}", ident.name));
      return static_cast<llvm::Value*>(load_inst);
    },
    pattern(_) = []{
      assert(false && "fix me! unknown symbol meta");
      return static_cast<llvm::Value*>(nullptr);
    }
  );
}

static Type_Ptr extract_type(std::weak_ptr<Type> weak_type_ptr) {
  if (auto type_ptr = weak_type_ptr.lock()) {
    return type_ptr;
  }
  assert(false && "fix me! expression type imformation is missing!");
}

llvm::Value* LLVMCodeGen::codegen_expression(Ast_Unary_Operation& unary, Scope& scope) {
  using namespace mpark::patterns;
  llvm::Value* operand = codegen_expression(*unary.operand, scope);
  auto& unary_primative = std::get<PrimativeType>(extract_type(unary.operand->type)->v);

  return match(unary_primative.tag, unary.operation)(
    pattern(anyof(PrimativeTypeTag::INTEGER, PrimativeTypeTag::FLOAT64), Ast_Operator::PLUS) = [&]{
      return operand;
    },
    pattern(PrimativeTypeTag::INTEGER, Ast_Operator::MINUS) = [&]{
      return ir_builder.CreateSub(
        llvm::ConstantInt::get(llvm_context,
          llvm::APInt(primative_size(PrimativeTypeTag::INTEGER), 0, true)),
        operand,
        "int_minus");
    },
    pattern(PrimativeTypeTag::INTEGER, Ast_Operator::BITWISE_NOT) = [&]{
      return ir_builder.CreateXor(
        operand,
        llvm::ConstantInt::get(llvm_context,
          llvm::APInt(primative_size(PrimativeTypeTag::INTEGER), -1, true)),
        "int_bitwise_not");
    },
    pattern(PrimativeTypeTag::FLOAT64, Ast_Operator::MINUS) = [&]{
      return ir_builder.CreateFSub(
        llvm::ConstantFP::get(llvm_context, llvm::APFloat(0.0)),
        operand,
        "float_minus");
    }
  );
}

llvm::Value* LLVMCodeGen::codegen_expression(Ast_Binary_Operation& binop, Scope& scope) {
  using namespace mpark::patterns;
  auto binop_type = extract_type(binop.left->type);
  auto& binop_primative = std::get<PrimativeType>(binop_type->v);

  llvm::Value* left = codegen_expression(*binop.left, scope);
  llvm::Value* right = codegen_expression(*binop.right, scope);

  return match(binop_primative.tag, binop.operation)(
    /* Basic integer operations */
    pattern(PrimativeTypeTag::INTEGER, Ast_Operator::PLUS) = [&]{
      return ir_builder.CreateAdd(left, right, "int_add");
    },
    pattern(PrimativeTypeTag::INTEGER, Ast_Operator::MINUS) = [&]{
      return ir_builder.CreateSub(left, right, "int_sub");
    },
    pattern(PrimativeTypeTag::INTEGER, Ast_Operator::DIVIDE) = [&]{
      return ir_builder.CreateSDiv(left, right, "int_div");
    },
    pattern(PrimativeTypeTag::INTEGER, Ast_Operator::TIMES) = [&]{
      return ir_builder.CreateMul(left, right, "int_mult");
    },
    pattern(PrimativeTypeTag::INTEGER, Ast_Operator::MODULO) = [&]{
      return ir_builder.CreateSRem(left, right, "int_mod");
    },
    pattern(PrimativeTypeTag::INTEGER, Ast_Operator::LESS_THAN) = [&]{
      return ir_builder.CreateICmpSLT(left, right, "int_lt");
    },
    pattern(PrimativeTypeTag::INTEGER, Ast_Operator::GREATER_THAN) = [&]{
      return ir_builder.CreateICmpUGT(left, right, "int_gt");
    },
    pattern(PrimativeTypeTag::INTEGER, Ast_Operator::BITWISE_AND) = [&]{
      return ir_builder.CreateAnd(left, right, "int_bitwise_and");
    },
    pattern(PrimativeTypeTag::INTEGER, Ast_Operator::BITWISE_OR) = [&]{
      return ir_builder.CreateOr(left, right, "int_bitwise_or");
    },
    pattern(PrimativeTypeTag::INTEGER, Ast_Operator::GREATER_EQUAL) = [&]{
      return ir_builder.CreateICmpSGE(left, right, "int_ge");
    },
    pattern(PrimativeTypeTag::INTEGER, Ast_Operator::LESS_EQUAL) = [&]{
      return ir_builder.CreateICmpSLE(left, right, "int_le");
    },
    pattern(PrimativeTypeTag::INTEGER, Ast_Operator::EQUAL_TO) = [&]{
      return ir_builder.CreateICmpEQ(left, right, "int_eq");
    },
    pattern(PrimativeTypeTag::INTEGER, Ast_Operator::NOT_EQUAL_TO) = [&]{
      return ir_builder.CreateICmpNE(left, right, "int_ne");
    },
    /*
      Basic float operations
      TODO: Understand floating point ordering!
      Read: https://stackoverflow.com/a/38516544/3818491
      Something to do with NaN (just using unordered operations for now)
    */
    pattern(PrimativeTypeTag::FLOAT64, Ast_Operator::PLUS) = [&]{
      return ir_builder.CreateFAdd(left, right, "float_add");
    },
    pattern(PrimativeTypeTag::FLOAT64, Ast_Operator::MINUS) = [&]{
      return ir_builder.CreateFSub(left, right, "float_sub");
    },
    pattern(PrimativeTypeTag::FLOAT64, Ast_Operator::DIVIDE) = [&]{
      return ir_builder.CreateFDiv(left, right, "float_div");
    },
    pattern(PrimativeTypeTag::FLOAT64, Ast_Operator::TIMES) = [&]{
      return ir_builder.CreateFMul(left, right, "float_mult");
    },
    pattern(PrimativeTypeTag::FLOAT64, Ast_Operator::LESS_THAN) = [&]{
      return ir_builder.CreateFCmpULT(left, right, "float_lt");
    },
    pattern(PrimativeTypeTag::FLOAT64, Ast_Operator::GREATER_THAN) = [&]{
      return ir_builder.CreateFCmpUGT(left, right, "float_gt");
    },
    pattern(PrimativeTypeTag::FLOAT64, Ast_Operator::GREATER_EQUAL) = [&]{
      return ir_builder.CreateFCmpUGE(left, right, "float_ge");
    },
    pattern(PrimativeTypeTag::FLOAT64, Ast_Operator::LESS_EQUAL) = [&]{
      return ir_builder.CreateFCmpULE(left, right, "float_le");
    },
    pattern(PrimativeTypeTag::FLOAT64, Ast_Operator::EQUAL_TO) = [&]{
      return ir_builder.CreateFCmpUEQ(left, right, "float_eq");
    },
    pattern(PrimativeTypeTag::FLOAT64, Ast_Operator::NOT_EQUAL_TO) = [&]{
      return ir_builder.CreateFCmpUNE(left, right, "float_ne");
    },
    pattern(_, _) = [&]{
      llvm::errs() << formatxx::format_string(
        ":( binary operation {} not implemented for type {}\n",
        token_type_to_string(static_cast<TokenType>(binop.operation)),
        type_to_string(binop_type.get()));
      assert(false);
      return static_cast<llvm::Value*>(nullptr);
    }
  );
}
