#include <cassert>
#include <iterator>
#include <algorithm>

#include <mpark/patterns.hpp>
#include <formatxx/std_string.h>

#include "ast/types.h"
#include "llvm_codegen.h"
#include "codegen/codegen.h"
#include "ast/ast_builder.h"
#include "parser/token_helpers.h"

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

  llvm_module->print(llvm::errs(), nullptr);
}

void LLVMCodeGen::create_module() {
  this->llvm_module = std::make_unique<llvm::Module>(
    file_ast.filename, llvm_context);
  /* TODO: set machine target */
  /* TODO: set up optimizations */
}

/* Types */

llvm::Type* LLVMCodeGen::map_primative_to_llvm(PrimativeType::Tag primative) {
  switch (primative) {
    case PrimativeType::INTEGER:
      return llvm::Type::getInt32Ty(llvm_context);
    case PrimativeType::FLOAT32:
      return llvm::Type::getFloatTy(llvm_context);
    case PrimativeType::FLOAT64:
      return llvm::Type::getDoubleTy(llvm_context);
    case PrimativeType::STRING:
      return llvm::Type::getInt8PtrTy(llvm_context);
    case PrimativeType::BOOL:
      return llvm::Type::getInt1Ty(llvm_context);
    default:
      assert(false && "fix me! mapping unknown primative to LLVM");
  }
}

std::vector<llvm::Type*> LLVMCodeGen::map_arg_types_to_llvm(
  std::vector<Ast_Argument> const & args,
  Scope& scope
) {
  std::vector<llvm::Type*> arg_types;
  arg_types.reserve(args.size());

  std::transform(args.begin(), args.end(), std::back_inserter(arg_types),
    [&](auto const & arg) {
      return map_type_to_llvm(arg.type.get(), scope);
    });

  return arg_types;
}

llvm::Type* LLVMCodeGen::map_pod_to_llvm(Ast_Pod_Declaration const & pod_type, Scope& scope) {
  std::vector<llvm::Type*> field_types = map_arg_types_to_llvm(pod_type.fields, scope);
  return llvm::StructType::create(llvm_context,
    field_types,
    pod_type.identifier.name);
}

llvm::Type* LLVMCodeGen::map_type_to_llvm(Type const * type, Scope& scope) {
  using namespace mpark::patterns;
  if (!type) {
    return llvm::Type::getVoidTy(llvm_context);
  }
  return match(type->v)(
    pattern(as<PrimativeType>(arg)) = [&](auto const & primative){
      return map_primative_to_llvm(primative.tag);
    },
    pattern(as<Ast_Pod_Declaration>(arg)) = [&](auto const & pod_type) {
      Symbol* pod_symbol = scope.lookup_first_name(pod_type.identifier);
      assert(pod_symbol && pod_symbol->kind == Symbol::TYPE);
      if (!pod_symbol->meta) {
        pod_symbol->meta = std::make_shared<SymbolMetaCompoundType>(
          map_pod_to_llvm(pod_type, scope));
      }
      return static_cast<SymbolMetaCompoundType*>(pod_symbol->meta.get())->type;
    },
    pattern(_) = []{
      assert(false && "not implemented");
      return static_cast<llvm::Type*>(nullptr);
    }
  );
}

bool LLVMCodeGen::block_terminates_here() {
  auto last_block = ir_builder.GetInsertBlock();
  if (!last_block->empty()) {
    llvm::Instruction* last_inst = last_block->getTerminator();
    if (last_inst) {
      llvm::BranchInst* last_branch = llvm::dyn_cast<llvm::BranchInst>(last_inst);
      if (last_branch && last_branch->isUnconditional()) {
        return true;
      }
    }
  }
  return false;
}

void LLVMCodeGen::create_exit_br(llvm::BasicBlock* target) {
  if (!block_terminates_here()) {
    ir_builder.CreateBr(target);
  }
}

/* Functions */

llvm::Function* LLVMCodeGen::get_current_function() {
  return ir_builder.GetInsertBlock()->getParent();
}

llvm::Function* LLVMCodeGen::get_function(Ast_Function_Declaration& func) {
  if (auto llvm_func = llvm_module->getFunction(func.identifier.name)) {
    return llvm_func;
  } else {
    return codegen_function_header(func);
  }
}

llvm::Function* LLVMCodeGen::codegen_function_header(Ast_Function_Declaration& func) {
  llvm::Type* return_type = map_type_to_llvm(func.return_type.get(), func.body.scope);

  llvm::FunctionType* func_type = llvm::FunctionType::get(
    return_type, map_arg_types_to_llvm(func.arguments, func.body.scope), /*vararg:*/ false);

  llvm::Function* llvm_func = llvm::Function::Create(
    func_type,
    llvm::Function::ExternalLinkage,
    func.identifier.name,
    llvm_module.get());

  uint arg_idx = 0;
  for (auto& arg: llvm_func->args()) {
    arg.setName(func.arguments.at(arg_idx).name.name);
    ++arg_idx;
  }

  return llvm_func;
}

llvm::AllocaInst* LLVMCodeGen::create_entry_alloca(
  llvm::Function* func, Scope& scope, Type* type, std::string name
) {
  /*
    Create an alloca for a (local) symbol at a functions entry.
    When using allocas for locals you must put them in the entry block
    for the mem2reg optimization to promote them to registers
  */
  llvm::IRBuilder<> entry_ir_builder(
    &func->getEntryBlock(),
    func->getEntryBlock().begin());

  llvm::Type* llvm_type = map_type_to_llvm(type, scope);
  llvm::AllocaInst* alloca = entry_ir_builder.CreateAlloca(
    llvm_type, /*array size:*/ 0, name);

  return alloca;
}

llvm::AllocaInst* LLVMCodeGen::create_entry_alloca(llvm::Function* func, Symbol* symbol) {
  assert("must be a local symbol" && symbol->is_local());
  llvm::AllocaInst* alloca = create_entry_alloca(
    func, *symbol->scope, symbol->type.get(), symbol->name.name);
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

  llvm::BasicBlock* function_return = llvm::BasicBlock::Create(
    llvm_context, "return");

  llvm::AllocaInst* return_alloca = nullptr;
  // Create function return value
  auto* return_symbol = &func.body.scope.add(Symbol(
    SymbolName(FUNCTION_RETURN_LOCAL),
    func.return_type,
    Symbol::LOCAL));
  if (/* functions */ !func.procedure) {
    return_alloca = create_entry_alloca(llvm_func, return_symbol);
  }
  return_symbol->meta = std::make_shared<SymbolMetaReturn>(return_alloca, function_return);

  codegen_expression(func.body, func.body.scope);
  create_exit_br(function_return);

  llvm_func->getBasicBlockList().push_back(function_return);
  ir_builder.SetInsertPoint(function_return);

  llvm::Value* return_value = nullptr;
  if (return_alloca) {
    return_value = ir_builder.CreateLoad(return_alloca, "load_return_value");
  }

  ir_builder.CreateRet(return_value);

  assert("function should be valid IR" && !llvm::verifyFunction(*llvm_func, &llvm::errs()));
}

/* Statements */

void LLVMCodeGen::codegen_statement(Ast_Statement& stmt, Scope& scope) {
  std::visit([&](auto& stmt) {
    codegen_statement(stmt, scope);
  }, stmt.v);
}

void LLVMCodeGen::codegen_statement(Ast_Expression_Statement& expr_stmt, Scope& scope) {
  (void) codegen_expression(*expr_stmt.expression, scope);
}

void LLVMCodeGen::codegen_statement(Ast_Return_Statement& return_stmt, Scope& scope) {
  Symbol* return_local = scope.lookup_first(FUNCTION_RETURN_LOCAL);
  assert(return_local && "return local must exist to codegen return statement!");

  // If this is not a SymbolMetaLocal something is _very_ wrong!
  auto return_meta = static_cast<SymbolMetaReturn*>(return_local->meta.get());

  if (return_stmt.expression) {
    llvm::Value* return_value = codegen_expression(*return_stmt.expression, scope);
    ir_builder.CreateStore(return_value, return_meta->alloca);
  }
  ir_builder.CreateBr(return_meta->return_block);
}

void LLVMCodeGen::codegen_statement(Ast_Assign& assign, Scope& scope) {
  using namespace mpark::patterns;

  llvm::Value* target_ptr = match(assign.target->v)(
    pattern(as<Ast_Identifier>(arg)) = [&](auto& variable_name) -> llvm::Value* {
      Symbol* variable = scope.lookup_first_name(variable_name);
      assert(variable->is_local());
      auto variable_meta = static_cast<SymbolMetaLocal*>(variable->meta.get());
      return variable_meta->alloca;
    },
    pattern(as<Ast_Field_Access>(arg)) = [&](auto& field) -> llvm::Value* {
      // Only support simple field accesses right now (checked in sema)
      assert(field.field_index >= 0);
      auto& pod_variable = std::get<Ast_Identifier>(field.object->v);
      Symbol* pod = scope.lookup_first_name(pod_variable);
      assert(pod->is_local());
      auto pod_meta = static_cast<SymbolMetaLocal*>(pod->meta.get());
      auto pod_type = extract_type(field.object->meta.type);
      return ir_builder.CreateConstGEP2_32(map_type_to_llvm(pod_type.get(), scope),
        pod_meta->alloca, 0, field.field_index);
    }
  );

  llvm::Value* expression_value = codegen_expression(*assign.expression, scope);
  ir_builder.CreateStore(expression_value, target_ptr);
}

void LLVMCodeGen::codegen_statement(Ast_Variable_Declaration& var_decl, Scope& scope) {
  llvm::Function* current_function = get_current_function();

  llvm::Value* initializer = nullptr;
  if (var_decl.initializer) {
    initializer = codegen_expression(*var_decl.initializer, scope);
  }

  /*
    Have to add the symbol again here as it's removed when it goes out of scope,
    when checking sementics, otherwise resolving shadowed variables would be
    nightmare.

    It's also very important the symbol is added to the scope after the initializer
    has been generated or something like:

    x := 0;
    x := x + 1;

    Could segfault or crash somehow (as it'll try to use the undefined x in the expression)
  */
  auto& local_symbol = scope.add(Symbol(var_decl.variable, var_decl.type, Symbol::LOCAL));
  llvm::AllocaInst* alloca = create_entry_alloca(current_function, &local_symbol);

  if (initializer) {
    ir_builder.CreateStore(initializer, alloca);
  }
}

#define LOOP_RANGE_END   "!end_range"

void LLVMCodeGen::codegen_statement(Ast_For_Loop& for_loop, Scope& scope) {
  /*
    TODO: Move loop desugaring into parser

    FIXME! Broken for loop (messy test codegen)

    No support for early return in for loops currently.
  */
  llvm::Value* start_value = codegen_expression(*for_loop.start_range, scope);
  llvm::Value* end_value = codegen_expression(*for_loop.end_range, scope);

  auto& body = for_loop.body;
  auto& loop_value_symbol = body.scope.add(Symbol(
    for_loop.loop_value, for_loop.value_type, Symbol::LOCAL));

  llvm::Function* current_function = get_current_function();
  llvm::AllocaInst* loop_value = create_entry_alloca(current_function, &loop_value_symbol);
  ir_builder.CreateStore(start_value, loop_value);

  auto loop_range_type = extract_type(for_loop.end_range->meta.type);
  auto& loop_end_range_symbol = body.scope.add(Symbol(
    SymbolName(LOOP_RANGE_END), loop_range_type, Symbol::LOCAL));
  llvm::AllocaInst* range_end = create_entry_alloca(current_function, &loop_end_range_symbol);

  ir_builder.CreateStore(end_value, range_end);

  llvm::BasicBlock* for_check = llvm::BasicBlock::Create(
    llvm_context, "for_check", current_function);

  llvm::BasicBlock* for_body = llvm::BasicBlock::Create(
    llvm_context, "for_body");

  llvm::BasicBlock* for_end = llvm::BasicBlock::Create(
    llvm_context, "for_end");

  ir_builder.CreateBr(for_check);
  ir_builder.SetInsertPoint(for_check);

  /* For loop check -- should stop or keep looping */
  Ast_Binary_Operation loop_cond;
  loop_cond.operation = Ast_Operator::LESS_THAN;
  loop_cond.left = to_expr_ptr(for_loop.loop_value);
  loop_cond.right = make_ident(LOOP_RANGE_END);
  loop_cond.left->meta.type = loop_cond.right->meta.type = for_loop.start_range->meta.type;

  llvm::Value* loop_check = codegen_expression(loop_cond, body.scope);
  ir_builder.CreateCondBr(loop_check, for_body, for_end);

  /* For loop body */
  // Probably could safely add directly
  current_function->getBasicBlockList().push_back(for_body);
  ir_builder.SetInsertPoint(for_body);

  codegen_expression(body, scope);

  llvm::BasicBlock* for_inc = llvm::BasicBlock::Create(
    llvm_context, "for_inc", current_function);

  create_exit_br(for_inc);

  ir_builder.SetInsertPoint(for_inc);

  /* For loop increment */
  // FIXME: Temp hack till I figure out proper ranges!
  auto& primative_loop_type = std::get<PrimativeType>(loop_range_type->v);
  auto loop_inc_literal = make_literal(primative_loop_type.tag, "");
  loop_inc_literal->meta.const_value = 1;

  Ast_Binary_Operation next_loop_value;
  next_loop_value.operation = Ast_Operator::PLUS;
  next_loop_value.left = to_expr_ptr(for_loop.loop_value);
  next_loop_value.right = loop_inc_literal;
  next_loop_value.left->meta.type = next_loop_value.right->meta.type = for_loop.start_range->meta.type;

  Ast_Assign inc_loop;
  inc_loop.target = to_expr_ptr(for_loop.loop_value);
  inc_loop.expression = to_expr_ptr(next_loop_value);

  codegen_statement(inc_loop, body.scope);
  // End FIXME

  // Loop back to the start
  ir_builder.CreateBr(for_check);
  body.scope.destroy_locals();

  current_function->getBasicBlockList().push_back(for_end);
  ir_builder.SetInsertPoint(for_end);
}

/* Expressions */

llvm::Value* LLVMCodeGen::codegen_expression(Ast_Expression& expr, Scope& scope) {
  return std::visit([&](auto& expr) {
    return codegen_expression(expr, scope);
  }, expr.v);
}

llvm::Value* LLVMCodeGen::codegen_expression(Ast_Block& block, Scope& scope) {
  llvm::Function* current_function = get_current_function();

  (void) scope; // Not needed
  auto statements_in_block = block.statements.size();
  if (block.has_final_expr) {
    statements_in_block -= 1;
  }
  for (uint stmt_idx = 0; stmt_idx < statements_in_block; stmt_idx++) {
    codegen_statement(*block.statements.at(stmt_idx), block.scope);
    if (block_terminates_here()) {
      break;
    }
  }

  if (auto final_expr = block.get_final_expr()) {
    /*
      This basic block is kind of a hack but it allows for expressions like:
      {
        return 10;
        0
      }
      to codegen without error
      (really the AST should remove unreachable code before codegen -- but I don't do that yet)
    */
    llvm::BasicBlock* block_eval = llvm::BasicBlock::Create(
      llvm_context, "block_eval", current_function);
    create_exit_br(block_eval);
    ir_builder.SetInsertPoint(block_eval);
    return codegen_expression(*final_expr, block.scope);
  }

  return nullptr;
}

llvm::Value* LLVMCodeGen::codegen_expression(Ast_If_Expr& if_expr, Scope& scope) {
  llvm::Function* current_function = get_current_function();
  llvm::Value* condition = codegen_expression(*if_expr.cond, scope);

  /* Create and insert the 'then' block into the function */
  llvm::BasicBlock* then_block = llvm::BasicBlock::Create(
    llvm_context, "then_block", current_function);

  llvm::BasicBlock* else_block = nullptr;

  llvm::BasicBlock* end_block = llvm::BasicBlock::Create(
    llvm_context, "if_end");

  if (if_expr.has_else) {
    else_block = llvm::BasicBlock::Create(llvm_context, "else_block");
    ir_builder.CreateCondBr(condition, then_block, else_block);
  } else {
    ir_builder.CreateCondBr(condition, then_block, end_block);
  }

  /* then */
  ir_builder.SetInsertPoint(then_block);
  llvm::Value* then_value = codegen_expression(*if_expr.then_block, scope);
  create_exit_br(end_block);
  // So the incoming edges into the Phi node are correct
  // As nesting ifs change the current basic block.
  then_block = ir_builder.GetInsertBlock();

  /* else */
  llvm::Value* else_value = nullptr;
  if (if_expr.has_else) {
    // Now add the else block!

    current_function->getBasicBlockList().push_back(else_block);
    ir_builder.SetInsertPoint(else_block);
    else_value = codegen_expression(*if_expr.else_block, scope);
    create_exit_br(end_block);
    else_block = ir_builder.GetInsertBlock();
  }

  /* end */
  current_function->getBasicBlockList().push_back(end_block);
  ir_builder.SetInsertPoint(end_block);

  if (then_value && else_value) {
    auto if_type = extract_type(if_expr.then_block->meta.type);
    llvm::PHINode* phi = ir_builder.CreatePHI(
      map_type_to_llvm(if_type.get(), scope), 2, "if_expr_selection");
    phi->addIncoming(then_value, then_block);
    phi->addIncoming(else_value, else_block);
    return phi;
  }
  return nullptr;
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
    case PrimativeType::INTEGER:
      return llvm::ConstantInt::get(llvm_context,
        llvm::APInt(
          /*bits:*/ literal.size_bytes(),
          /*value:*/ literal.as_int32(),
          /*signed:*/ true));
    case PrimativeType::FLOAT32:
      return llvm::ConstantFP::get(llvm_context,
        llvm::APFloat(/*value:*/ literal.as_float32()));
    case PrimativeType::FLOAT64:
      return llvm::ConstantFP::get(llvm_context,
          llvm::APFloat(/*value:*/ literal.as_float64()));
    case PrimativeType::STRING:
      assert(false && "string literal codegen not implemented");
      return nullptr;
    case PrimativeType::BOOL:
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
  assert(symbol->is_local() && "only locals implemented");

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
llvm::Value* LLVMCodeGen::codegen_expression(Ast_Unary_Operation& unary, Scope& scope) {
  using namespace mpark::patterns;
  llvm::Value* operand = codegen_expression(*unary.operand, scope);
  auto unary_type = extract_type(unary.operand->meta.type);
  auto& unary_primative = std::get<PrimativeType>(unary_type->v);

  return match(unary_primative.tag, unary.operation)(
    pattern(anyof(PrimativeType::INTEGER, PrimativeType::FLOAT64), Ast_Operator::PLUS) = [&]{
      return operand;
    },
    pattern(PrimativeType::INTEGER, Ast_Operator::MINUS) = [&]{
      return ir_builder.CreateSub(
        llvm::ConstantInt::get(llvm_context,
          llvm::APInt(PrimativeType::type_size(PrimativeType::INTEGER), 0, true)),
        operand,
        "int_minus");
    },
    pattern(PrimativeType::INTEGER, Ast_Operator::BITWISE_NOT) = [&]{
      return ir_builder.CreateXor(
        operand,
        llvm::ConstantInt::get(llvm_context,
          llvm::APInt(PrimativeType::type_size(PrimativeType::INTEGER), -1, true)),
        "int_bitwise_not");
    },
    pattern(PrimativeType::FLOAT64, Ast_Operator::MINUS) = [&]{
      return ir_builder.CreateFSub(
        llvm::ConstantFP::get(llvm_context, llvm::APFloat(0.0)),
        operand,
        "float_minus");
    },
    pattern(PrimativeType::BOOL, Ast_Operator::LOGICAL_NOT) = [&]{
      return ir_builder.CreateNot(operand, "bool_logical_not");
    },
    pattern(_, _) = [&]{
      llvm::errs() << formatxx::format_string(
        ":( unary operation {} not implemented for type {}\n",
        token_type_to_string(static_cast<TokenType>(unary.operation)),
        type_to_string(*unary_type));
      assert(false);
      return static_cast<llvm::Value*>(nullptr);
    }
  );
}

llvm::Value* LLVMCodeGen::codegen_expression(Ast_Binary_Operation& binop, Scope& scope) {
  using namespace mpark::patterns;
  auto binop_type = extract_type(binop.left->meta.type);
  auto& binop_primative = std::get<PrimativeType>(binop_type->v);

  llvm::Value* left = codegen_expression(*binop.left, scope);
  llvm::Value* right = codegen_expression(*binop.right, scope);

  return match(binop_primative.tag, binop.operation)(
    /* Basic integer operations */
    pattern(PrimativeType::INTEGER, Ast_Operator::PLUS) = [&]{
      return ir_builder.CreateAdd(left, right, "int_add");
    },
    pattern(PrimativeType::INTEGER, Ast_Operator::MINUS) = [&]{
      return ir_builder.CreateSub(left, right, "int_sub");
    },
    pattern(PrimativeType::INTEGER, Ast_Operator::DIVIDE) = [&]{
      return ir_builder.CreateSDiv(left, right, "int_div");
    },
    pattern(PrimativeType::INTEGER, Ast_Operator::TIMES) = [&]{
      return ir_builder.CreateMul(left, right, "int_mult");
    },
    pattern(PrimativeType::INTEGER, Ast_Operator::MODULO) = [&]{
      return ir_builder.CreateSRem(left, right, "int_mod");
    },
    pattern(PrimativeType::INTEGER, Ast_Operator::LESS_THAN) = [&]{
      return ir_builder.CreateICmpSLT(left, right, "int_lt");
    },
    pattern(PrimativeType::INTEGER, Ast_Operator::GREATER_THAN) = [&]{
      return ir_builder.CreateICmpSGT(left, right, "int_gt");
    },
    pattern(PrimativeType::INTEGER, Ast_Operator::BITWISE_AND) = [&]{
      return ir_builder.CreateAnd(left, right, "int_bitwise_and");
    },
    pattern(PrimativeType::INTEGER, Ast_Operator::BITWISE_OR) = [&]{
      return ir_builder.CreateOr(left, right, "int_bitwise_or");
    },
    pattern(PrimativeType::INTEGER, Ast_Operator::GREATER_EQUAL) = [&]{
      return ir_builder.CreateICmpSGE(left, right, "int_ge");
    },
    pattern(PrimativeType::INTEGER, Ast_Operator::LESS_EQUAL) = [&]{
      return ir_builder.CreateICmpSLE(left, right, "int_le");
    },
    pattern(PrimativeType::INTEGER, Ast_Operator::EQUAL_TO) = [&]{
      return ir_builder.CreateICmpEQ(left, right, "int_eq");
    },
    pattern(PrimativeType::INTEGER, Ast_Operator::NOT_EQUAL_TO) = [&]{
      return ir_builder.CreateICmpNE(left, right, "int_ne");
    },
    /*
      Basic float operations
      TODO: Understand floating point ordering!
      Read: https://stackoverflow.com/a/38516544/3818491
      Something to do with NaN (just using unordered operations for now)
    */
    pattern(PrimativeType::FLOAT64, Ast_Operator::PLUS) = [&]{
      return ir_builder.CreateFAdd(left, right, "float_add");
    },
    pattern(PrimativeType::FLOAT64, Ast_Operator::MINUS) = [&]{
      return ir_builder.CreateFSub(left, right, "float_sub");
    },
    pattern(PrimativeType::FLOAT64, Ast_Operator::DIVIDE) = [&]{
      return ir_builder.CreateFDiv(left, right, "float_div");
    },
    pattern(PrimativeType::FLOAT64, Ast_Operator::TIMES) = [&]{
      return ir_builder.CreateFMul(left, right, "float_mult");
    },
    pattern(PrimativeType::FLOAT64, Ast_Operator::LESS_THAN) = [&]{
      return ir_builder.CreateFCmpULT(left, right, "float_lt");
    },
    pattern(PrimativeType::FLOAT64, Ast_Operator::GREATER_THAN) = [&]{
      return ir_builder.CreateFCmpUGT(left, right, "float_gt");
    },
    pattern(PrimativeType::FLOAT64, Ast_Operator::GREATER_EQUAL) = [&]{
      return ir_builder.CreateFCmpUGE(left, right, "float_ge");
    },
    pattern(PrimativeType::FLOAT64, Ast_Operator::LESS_EQUAL) = [&]{
      return ir_builder.CreateFCmpULE(left, right, "float_le");
    },
    pattern(PrimativeType::FLOAT64, Ast_Operator::EQUAL_TO) = [&]{
      return ir_builder.CreateFCmpUEQ(left, right, "float_eq");
    },
    pattern(PrimativeType::FLOAT64, Ast_Operator::NOT_EQUAL_TO) = [&]{
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

llvm::Value* LLVMCodeGen::codegen_expression(Ast_Field_Access& access, Scope& scope) {
  using namespace mpark::patterns;
  auto pod_type = extract_type(access.object->meta.type);

  llvm::Value* pod_ptr = match(access.object->v)(
    pattern(as<Ast_Identifier>(arg)) = [&](auto& variable_name) -> llvm::Value* {
      Symbol* pod_var = scope.lookup_first_name(variable_name);
      assert(pod_var && pod_var->is_local());
      auto meta = static_cast<SymbolMetaLocal*>(pod_var->meta.get());
      return meta->alloca;
    },
    pattern(as<Ast_Field_Access>(_)) = []() -> llvm::Value* {
      assert(false && "todo getting values in nested structures");
      return nullptr;
    },
    pattern(_) = [&]() -> llvm::Value* {
      // _assuming_ this is an rvalue (such as a call return / expr return)
      llvm::Value* pod_temp = codegen_expression(*access.object, scope);
      llvm::Function* current_function = get_current_function();
      llvm::AllocaInst* temp = create_entry_alloca(
        current_function, scope, pod_type.get(), "temp");
      ir_builder.CreateStore(pod_temp, temp);
      return temp;
    });

  llvm::Value* field_ptr = ir_builder.CreateConstGEP2_32(map_type_to_llvm(pod_type.get(), scope),
    pod_ptr, 0, access.field_index);

  return ir_builder.CreateLoad(field_ptr);
}

/* JIT tools */

llvm::orc::VModuleKey LLVMCodeGen::jit_current_module() {
  assert(llvm_module && "module to jit cannot be null!");
  if (!jit_engine) {
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();
    jit_engine = std::make_unique<llvm::orc::KaleidoscopeJIT>();
  }
  llvm_module->setDataLayout(jit_engine->getTargetMachine().createDataLayout());
  return jit_engine->addModule(std::move(llvm_module));
}

void* CodeGen::find_jit_symbol(std::string name) {
  return static_cast<LLVMCodeGen*>(impl.get())->jit_find_symbol(name);
}

void* LLVMCodeGen::jit_find_symbol(std::string name) {
  if (!jit_module_handle) {
    jit_module_handle = jit_current_module();
  }

  auto symbol_adress = jit_engine->findSymbol(name).getAddress();
  if (auto err = symbol_adress.takeError()) {
    llvm::logAllUnhandledErrors(std::move(err), llvm::errs(), "[JIT Error] ");
    assert(false && "failed to get jit-ed symbol :(");
  }

  return reinterpret_cast<void*>(symbol_adress.get());
}

LLVMCodeGen::~LLVMCodeGen() {
  // Not sure it this is needed
  if (jit_engine && jit_module_handle) {
    jit_engine->removeModule(*jit_module_handle);
  }
}
