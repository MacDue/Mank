#include <cassert>
#include <iterator>
#include <algorithm>

#include <mpark/patterns.hpp>
#include <formatxx/std_string.h>

#include "sema/types.h"
#include "ast/ast_builder.h"
#include "parser/token_helpers.h"

#include "llvm_codegen.h"
#include "codegen/codegen.h"
#include "codegen/builtins.h"
#include "codegen/mangle.h"

CodeGen::CodeGen(Ast_File& file_ast)
  : impl{std::make_unique<LLVMCodeGen>(file_ast)} {}

LLVMCodeGen::LLVMCodeGen(Ast_File& file_ast)
  : file_ast{file_ast}, mank_ctx{file_ast.ctx}, ast_builder{file_ast}
{
  this->create_module();

  for (auto func: file_ast.functions) {
    this->codegen_function_body(*func);
  }

  llvm_module->print(llvm::outs(), nullptr);
  llvm::outs() << ";--fin\n";
}

void LLVMCodeGen::create_module() {
  this->llvm_module = std::make_unique<llvm::Module>(
    file_ast.filename, llvm_context);
  /* TODO: set machine target */
  /* TODO: set up optimizations */
}

llvm::Value* LLVMCodeGen::get_source_filename() {
  if (this->source_filename) {
    return this->source_filename;
  }
  return this->source_filename = ir_builder.CreateGlobalStringPtr(
    file_ast.filename.c_str(), "!mank_source");
}

llvm::Function* LLVMCodeGen::get_external(
  llvm::StringRef name,
  llvm::Type* return_type,
  llvm::ArrayRef<llvm::Type*> arg_types
) {
  llvm::Function* func = llvm_module->getFunction(name);
  if (func) {
    return func;
  }

  llvm::FunctionType* func_type = llvm::FunctionType::get(
     return_type, arg_types, false);

  func = llvm::Function::Create(
    func_type,
    llvm::Function::ExternalLinkage,
    name,
    llvm_module.get());

  return func;
}

llvm::Function* LLVMCodeGen::get_gc_malloc() {
  return get_external(Builtin::GC_MALLOC,
    llvm::Type::getInt8PtrTy(llvm_context),
    { llvm::Type::getInt64Ty(llvm_context) });
}

llvm::Type* LLVMCodeGen::get_string_ty(Scope& scope) {
  Symbol* str_sym = scope.lookup_first("str");
  assert(str_sym && match_types(str_sym->type, PrimativeType::get(PrimativeType::STRING)));

  if (!str_sym->meta) {
    llvm::Type* llvm_str_type = llvm::StructType::create(
      llvm_context,
      { llvm::Type::getInt64Ty(llvm_context), llvm::Type::getInt8PtrTy(llvm_context) },
      "!str"
    );
    str_sym->meta = std::make_shared<SymbolMetaCompoundType>(llvm_str_type);
    return llvm_str_type;
  } else {
    return static_cast<SymbolMetaCompoundType*>(str_sym->meta.get())->type;
  }
}

llvm::Function* LLVMCodeGen::get_str_concat_internal() {
  return get_external(
    Builtin::MANK_STR_CONCAT_INTERNAL,
    llvm::Type::getInt8PtrTy(llvm_context),
    {
      llvm::Type::getInt64Ty(llvm_context),
      llvm::Type::getInt8PtrTy(llvm_context),
      llvm::Type::getInt64Ty(llvm_context),
      llvm::Type::getInt8PtrTy(llvm_context),
      llvm::Type::getInt64PtrTy(llvm_context)
    });
}

llvm::Function* LLVMCodeGen::get_init_vec(Scope& scope) {
  return get_external(
    Builtin::MANK_VEC_INIT,
    llvm::Type::getVoidTy(llvm_context),
    {
      llvm::PointerType::get(get_vector_ty(scope), 0),
      llvm::Type::getInt64Ty(llvm_context)
    });
}

llvm::Function* LLVMCodeGen::get_vec_push_back(Scope& scope) {
  return get_external(
    Builtin::MANK_VEC_PUSH_BACK,
    llvm::Type::getVoidTy(llvm_context),
    {
      llvm::PointerType::get(get_vector_ty(scope), 0),
      llvm::Type::getInt8PtrTy(llvm_context)
    });
}

llvm::Function* LLVMCodeGen::get_vec_pop_back(Scope& scope) {
  return get_external(
    Builtin::MANK_VEC_POP_BACK,
    llvm::Type::getVoidTy(llvm_context),
    { llvm::PointerType::get(get_vector_ty(scope), 0) });
}

llvm::Function* LLVMCodeGen::get_vec_fill(Scope& scope) {
  return get_external(
    Builtin::MANK_VEC_FILL,
    llvm::Type::getVoidTy(llvm_context),
    {
      llvm::PointerType::get(get_vector_ty(scope), 0),
      llvm::Type::getInt8PtrTy(llvm_context),
      llvm::Type::getInt64Ty(llvm_context)
    });
}

llvm::Function* LLVMCodeGen::get_bounds_error() {
  return get_external(
    Builtin::MANK_BOUNDS_ERROR,
    llvm::Type::getVoidTy(llvm_context),
    {
      llvm::Type::getInt64Ty(llvm_context),   // length
      llvm::Type::getInt64Ty(llvm_context),   // index
      llvm::Type::getInt8PtrTy(llvm_context), // filename
      llvm::Type::getInt32Ty(llvm_context),   // line
      llvm::Type::getInt32Ty(llvm_context)    // col
    });
}

std::pair<llvm::Value*, llvm::Value*>
LLVMCodeGen::extract_string_info(Ast_Expression& expr, Scope& scope) {
  auto str_type = std::get_if<PrimativeType>(&expr.meta.type->v);
  assert(str_type && str_type->is_string_type());
  auto str_extract = get_value_extractor(expr, scope);
  return std::make_pair(
    str_extract.get_value({Builtin::String::LENGTH}, "str_length"),
    str_extract.get_value({Builtin::String::DATA}, "str_ptr"));
}

llvm::Value* LLVMCodeGen::fix_string_length(llvm::Value* length) {
  // FIXME: This should be removed when mank supports size_t as a type
  return ir_builder.CreateIntCast(
    length, map_primative_to_llvm(PrimativeType::INTEGER), true, "str_length");
}

llvm::Value* LLVMCodeGen::create_string_concat(
  Ast_Expression& s1, Ast_Expression& s2, Scope& scope
) {
  auto [l1, p1] = extract_string_info(s1, scope);
  auto [l2, p2] = extract_string_info(s2, scope);

  llvm::Function* current_function = get_current_function();
  llvm::Value* str_concat_len_ptr = create_entry_alloca(
    current_function, llvm::Type::getInt64Ty(llvm_context), "str_concat_len");

  llvm::Value* new_str = ir_builder.CreateCall(get_str_concat_internal(),
    { l1, p1, l2, p2, str_concat_len_ptr }, "str_concat");

  return create_string(new_str,
    ir_builder.CreateLoad(str_concat_len_ptr, "str_concat_len"), scope);
}

llvm::Value* LLVMCodeGen::create_char_string_cast(llvm::Value* char_value, Scope& scope) {
  auto get_str_cast = [&]{
    return get_external(
      Builtin::MANK_STR_CAST_INTERNAL,
      llvm::Type::getInt8PtrTy(llvm_context),
      { llvm::Type::getInt8Ty(llvm_context) });
  };

  llvm::Value* new_str = ir_builder.CreateCall(get_str_cast(), { char_value }, "str_char_cast");
  return create_string(new_str,
    llvm::ConstantInt::get(llvm::Type::getInt64Ty(llvm_context), 1), scope);
}

LLVMCodeGen::ExpressionExtract::ExpressionExtract(
  LLVMCodeGen* codegen, Ast_Expression& expr, Scope& scope
): codegen{codegen}, is_lvalue{expr.is_lvalue()}
{
  if (is_lvalue) {
    value_or_address = codegen->address_of(expr, scope);
  } else {
    value_or_address = codegen->codegen_expression(expr, scope);
  }
}

// hack
LLVMCodeGen::ExpressionExtract::ExpressionExtract(
  LLVMCodeGen* codegen, llvm::Value* value, bool is_lvalue
): codegen{codegen}, is_lvalue{is_lvalue}, value_or_address{value} {}

llvm::Value* LLVMCodeGen::ExpressionExtract::get_value(
  std::vector<unsigned> const & idx_list, llvm::Twine const & name
) {
  llvm::Value* value = this->get_bind(idx_list, name);
  if (is_lvalue) {
    value = codegen->ir_builder.CreateLoad(value, name);
  }
  return value;
}

llvm::Value* LLVMCodeGen::ExpressionExtract::get_bind(
  std::vector<unsigned> const & idx_list, llvm::Twine const & name
) {
  if (is_lvalue) {
    llvm::Value* value_addr = codegen->ir_builder.CreateGEP(
      value_or_address, codegen->make_idx_list_for_gep(idx_list), name);
    return value_addr;
  } else {
    return codegen->ir_builder.CreateExtractValue(value_or_address, idx_list, name);
  }
}

llvm::Value* LLVMCodeGen::ExpressionExtract::get_bind(
  std::vector<unsigned> const & idx_list, Type_Ptr bound_to, llvm::Twine const & name
) {
  llvm::Value* value;
  if (is_reference_type(bound_to)) {
    value = get_bind(idx_list, name + "_ref");
  } else {
    value = get_value(idx_list, name + "_value");
  }
  return value;
}

/* Types */

llvm::Type* LLVMCodeGen::map_primative_to_llvm(PrimativeType::Tag primative) {
  switch (primative) {
    case PrimativeType::INTEGER:
      return llvm::Type::getInt32Ty(llvm_context);
    case PrimativeType::INTEGER64:
      return llvm::Type::getInt64Ty(llvm_context);
    case PrimativeType::FLOAT32:
      return llvm::Type::getFloatTy(llvm_context);
    case PrimativeType::FLOAT64:
      return llvm::Type::getDoubleTy(llvm_context);
    // case PrimativeType::STRING:
    //   return llvm::Type::getInt8PtrTy(llvm_context);
    case PrimativeType::BOOL:
      return llvm::Type::getInt1Ty(llvm_context);
    case PrimativeType::CHAR:
    case PrimativeType::UNSIGNED_BYTE:
      return llvm::Type::getInt8Ty(llvm_context);
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

llvm::Type* LLVMCodeGen::map_lambda_type_to_llvm(LambdaType const & lambda_type, Scope& scope) {
  std::vector<llvm::Type*> arg_types;
  // The lambda env pointer
  std::transform(lambda_type.argument_types.begin(), lambda_type.argument_types.end(),
    std::back_inserter(arg_types), [&](auto& arg_type){
      return map_type_to_llvm(arg_type.get(), scope);
    });
  arg_types.push_back(llvm::Type::getInt8PtrTy(llvm_context));

  llvm::Type* lambda_func_type = llvm::FunctionType::get(
    map_type_to_llvm(lambda_type.return_type.get(), scope), arg_types, false);

  return llvm::StructType::get(llvm_context, {
      llvm::Type::getInt8PtrTy(llvm_context),
      llvm::PointerType::get(lambda_func_type, 0)
    }, "lambda");
}

llvm::Type* LLVMCodeGen::map_pod_to_llvm(Ast_Pod_Declaration const & pod_type, Scope& scope) {
  std::vector<llvm::Type*> field_types = map_arg_types_to_llvm(pod_type.fields, scope);
  return llvm::StructType::create(llvm_context,
    field_types,
    pod_type.identifier.name);
}

#define MANK_VEC_BULTIN "!vec"

llvm::Type* LLVMCodeGen::get_vector_ty(Scope& scope) {
  /*
    struct __mank_vec_header {
      size_t
        type_size,
        capacity,
        length;
    };

    struct __mank_vec {
      void* data; // negative indexed header
    };
  */

  Symbol& sym = [&]() -> Symbol& {
    if (auto sym = scope.lookup_first(MANK_VEC_BULTIN)) {
      return *sym;
    }
    // Add to the root scope
    return file_ast.scope.add(Symbol(
      SymbolName(MANK_VEC_BULTIN), nullptr, Symbol::TYPE));
  }();

  // llvm::Type* size_t_llvm = llvm::Type::getInt64Ty(llvm_context);
  if (!sym.meta) {
    llvm::Type* vec_type = llvm::StructType::create(llvm_context, {
      // size_t_llvm, /* type_size */
      // size_t_llvm, /* capacity */
      // size_t_llvm, /* length */
      llvm::Type::getInt8PtrTy(llvm_context) /* data */
    }, MANK_VEC_BULTIN);
    sym.meta = std::make_shared<SymbolMetaCompoundType>(vec_type);
  }

  return static_cast<SymbolMetaCompoundType*>(sym.meta.get())->type;
}

llvm::Type* LLVMCodeGen::map_type_to_llvm(Type const * type, Scope& scope) {
  using namespace mpark::patterns;
  if (!type) {
    return llvm::Type::getVoidTy(llvm_context);
  }
  return match(type->v)(
    pattern(as<PrimativeType>(arg)) = [&](auto const & primative){
      if (primative.tag == PrimativeType::STRING) {
        return get_string_ty(scope);
      }
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
    pattern(as<FixedSizeArrayType>(arg)) = [&](auto const & array_type) -> llvm::Type* {
      return llvm::ArrayType::get(
        map_type_to_llvm(array_type.element_type.get(), scope), array_type.size);
    },
    pattern(as<ReferenceType>(arg)) = [&](auto const & reference_type) -> llvm::Type* {
      return llvm::PointerType::get(
        map_type_to_llvm(reference_type.references.get(), scope), 0);
    },
    pattern(as<LambdaType>(arg)) = [&](auto const & lambda_type) -> llvm::Type* {
      return map_lambda_type_to_llvm(lambda_type, scope);
    },
    pattern(as<TupleType>(arg)) = [&](auto const & tuple_type) -> llvm::Type* {
      std::vector<llvm::Type*> element_types;
      std::transform(tuple_type.element_types.begin(), tuple_type.element_types.end(),
        std::back_inserter(element_types),
        [&](auto const & element_type){ return map_type_to_llvm(element_type.get(), scope); });
      return llvm::StructType::get(llvm_context, element_types);
    },
    pattern(as<CellType>(arg)) = [&](auto const & cell_type) -> llvm::Type* {
      return map_type_to_llvm(cell_type.ref.get(), scope);
    },
    pattern(as<ListType>(_)) = [&]() -> llvm::Type* {
      return get_vector_ty(scope);
    },
    pattern(_) = []() -> llvm::Type* {
      assert(false && "not implemented");
      return nullptr;
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

llvm::Value* LLVMCodeGen::get_local(Ast_Identifier& ident, Scope& scope) {
  Symbol* variable = scope.lookup_first_name(ident);
  assert(variable && variable->is_local());

  size_t closure_element_idx;
  ClosureInfo* closure_info = nullptr;
  if (!current_closure_info.empty()) {
    // FIXME: Crappy slow solution
    closure_info = &current_closure_info.top();
    auto result = std::find(
      closure_info->closure->begin(),
      closure_info->closure->end(),
      variable);
    if (result != closure_info->closure->end()) {
      closure_element_idx = std::distance(closure_info->closure->begin(), result);
    } else {
      closure_info = nullptr;
    }
  }

  if (!closure_info) {
    auto variable_meta = static_cast<SymbolMetaLocal*>(variable->meta.get());
    return variable_meta->alloca;
  } else {
    return ir_builder.CreateConstGEP2_32(
      closure_info->closure_type, closure_info->closure_ptr, 0, closure_element_idx,
      formatxx::format_string("captured_{}", variable->name.name));
  }
}

/* Functions */

llvm::Function* LLVMCodeGen::get_current_function() {
  return ir_builder.GetInsertBlock()->getParent();
}

llvm::Function* LLVMCodeGen::get_function(Ast_Function_Declaration& func) {
  if (auto llvm_func = llvm_module->getFunction(ABI::mangle(func))) {
    return llvm_func;
  } else {
    return codegen_function_header(func);
  }
}

#define LAMBDA_ENV "!lambda_env"

llvm::Function* LLVMCodeGen::codegen_function_header(Ast_Function_Declaration& func) {
  llvm::Type* return_type = map_type_to_llvm(func.return_type.get(), func.body.scope);

  if (func.lambda) {
    // Add the lambda environment parameter
    auto env_type = ast_builder.make_reference(PrimativeType::get(PrimativeType::UNSIGNED_BYTE));
    SymbolName env_name(LAMBDA_ENV);
    func.arguments.push_back(Ast_Argument{.type = env_type, .name = env_name});
    func.body.scope.add(Symbol(env_name, env_type, Symbol::INPUT));
  }

  llvm::FunctionType* func_type = llvm::FunctionType::get(
    return_type, map_arg_types_to_llvm(func.arguments, func.body.scope), /*vararg:*/ false);

  llvm::Function* llvm_func = llvm::Function::Create(
    func_type,
    llvm::Function::ExternalLinkage,
    ABI::mangle(func),
    llvm_module.get());

  uint arg_idx = 0;
  for (auto& arg: llvm_func->args()) {
    arg.setName(func.arguments.at(arg_idx).name.name);
    ++arg_idx;
  }

  return llvm_func;
}

llvm::AllocaInst* LLVMCodeGen::create_entry_alloca(
  llvm::Function* func, llvm::Type* type, std::string name
) {
  /*
    Create an alloca for a (local) symbol at a functions entry.
    When using allocas for locals you must put them in the entry block
    for the mem2reg optimization to promote them to registers
  */
  llvm::IRBuilder<> entry_ir_builder(
    &func->getEntryBlock(),
    func->getEntryBlock().begin());

  llvm::AllocaInst* alloca = entry_ir_builder.CreateAlloca(
    type, /*array size:*/ 0, name);

  return alloca;
}

llvm::AllocaInst* LLVMCodeGen::create_entry_alloca(
  llvm::Function* func, Scope& scope, Type* type, std::string name
) {
  return create_entry_alloca(func, map_type_to_llvm(type, scope), name);
}

llvm::AllocaInst* LLVMCodeGen::create_entry_alloca(llvm::Function* func, Symbol* symbol) {
  assert("must be a local symbol" && symbol->is_local());
  llvm::AllocaInst* alloca = create_entry_alloca(
    func, *symbol->scope, symbol->type.get(), symbol->name.name);
  symbol->meta = std::make_shared<SymbolMetaLocal>(alloca);
  return alloca;
}

#define FUNCTION_RETURN_LOCAL "!return_value"

void LLVMCodeGen::codegen_function_body(Ast_Function_Declaration& func, llvm::Function* llvm_func) {
  // I think this is safe after the function is checked
  func.procedure = match_types(func.return_type, nullptr);

  if (!llvm_func) {
    llvm_func = this->get_function(func);
  }

  assert("function must not already be generated" && llvm_func->empty());

  // Create & insert body block (entry block)
  llvm::BasicBlock* function_body = llvm::BasicBlock::Create(
    llvm_context, "body", llvm_func);
  ir_builder.SetInsertPoint(function_body);

  // Create allocas for the locals (the arguments) & store them
  for (auto& arg: llvm_func->args()) {
    if (!current_closure_info.empty() && arg.getName() == LAMBDA_ENV) {
      auto& closure_info = current_closure_info.top();
      if (closure_info.closure_type) {
        llvm::Value* closure_ptr = ir_builder.CreateBitCast(&arg,
          llvm::PointerType::get(closure_info.closure_type, 0), "closure");
        closure_info.closure_ptr = closure_ptr;
      }
    } else {
      Symbol* local_arg = func.body.scope.lookup_first(std::string(arg.getName()));
      llvm::AllocaInst* arg_alloca = create_entry_alloca(llvm_func, local_arg);
      // Store passed argument
      ir_builder.CreateStore(&arg, arg_alloca);
    }
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

  // If this is not a SymbolMetaReturn something is _very_ wrong!
  auto return_meta = static_cast<SymbolMetaReturn*>(return_local->meta.get());

  if (return_stmt.expression) {
    llvm::Value* return_value = codegen_bind(
      *return_stmt.expression, return_local->type, scope);
    ir_builder.CreateStore(return_value, return_meta->alloca);
  }
  ir_builder.CreateBr(return_meta->return_block);
}

void LLVMCodeGen::codegen_tuple_assign(
  Ast_Tuple_Literal& tuple_pattern, ExpressionExtract& tuple, std::vector<unsigned> idxs, Scope& scope
) {
  using namespace mpark::patterns;
  uint gep_idx = 0;
  for (auto el: tuple_pattern.elements) {
    idxs.push_back(gep_idx);
    match(el->v) (
      pattern(as<Ast_Tuple_Literal>(arg)) = [&](auto& next_pattern) {
        codegen_tuple_assign(next_pattern, tuple, idxs, scope);
      },
      pattern(_) = [&]{
        llvm::Value* tuple_el = tuple.get_value(idxs, "tuple_element");
        llvm::Value* target_ptr = address_of(*el, scope);
        ir_builder.CreateStore(tuple_el, target_ptr);
      }
    );
    idxs.pop_back();
    ++gep_idx;
  }
}

LLVMCodeGen::ExpressionExtract LLVMCodeGen::get_tuple_extractor(
  Ast_Expression& tuple, Scope& scope
) {
  tuple.fix_tuple_hack();
  return get_value_extractor(tuple, scope);
}

void LLVMCodeGen::codegen_statement(Ast_Assign& assign, Scope& scope) {
  if (auto tuple_pattern = std::get_if<Ast_Tuple_Literal>(&assign.target->v)) {
    auto tuple_extract = get_tuple_extractor(*assign.expression, scope);
    codegen_tuple_assign(*tuple_pattern, tuple_extract, {}, scope);
    return;
  }
  llvm::Value* target_ptr = address_of(*assign.target, scope);
  llvm::Value* expression_value = codegen_expression(*assign.expression, scope);
  ir_builder.CreateStore(expression_value, target_ptr);
}

void LLVMCodeGen::codegen_statement(Ast_Variable_Declaration& var_decl, Scope& scope) {
  using namespace mpark::patterns;
  llvm::Function* current_function = get_current_function();

  llvm::Value* initializer = nullptr;

  bool special_init = false;
  Ast_Expression_List* agg_initializer = nullptr;
  Ast_Pod_Literal* pod_initializer = nullptr;

  if (var_decl.initializer) {
    match(var_decl.initializer->v)(
      pattern(anyof(as<Ast_Array_Literal>(arg), as<Ast_Tuple_Literal>(arg))) =
      [&](auto& agg) { agg_initializer = agg.get_raw_self(); },
      pattern(as<Ast_Pod_Literal>(arg)) =
      [&](auto& pod) { pod_initializer = pod.get_raw_self(); },
      pattern(_) = []{}
    );

    special_init = agg_initializer || pod_initializer;

    /*
      (Non-const) array inits are not simple expressions.
      Best I know is they have to be compiled to a bunch of geps + stores.
    */
    if (!special_init) {
      initializer = codegen_bind(*var_decl.initializer, var_decl.type, scope);
    }
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
  } else if (special_init) {
    // "Remove" the symbol temporarily to avoid issues in array init.
    auto name_backup = local_symbol.name.name;
    local_symbol.name.name.clear();
    // needs to be done before sym addd
    if (agg_initializer) {
      initialize_aggregate(alloca, *agg_initializer, scope);
    } else if (pod_initializer) {
      initialize_pod(alloca, *pod_initializer, scope);
    } else {
      assert(false && "fix me! unknown special initializer");
    }
    local_symbol.name.name = name_backup;
  }
}

#define LOOP_RANGE_END "!end_range"

// TODO: Turn into simpler loop before codegen
void LLVMCodeGen::codegen_statement(Ast_For_Loop& for_loop, Scope& scope) {
  /*
    TODO: Move loop desugaring into parser

    FIXME! Broken for loop (messy test codegen)

    No support for early return in for loops currently.
  */
  llvm::Value* start_value = codegen_expression(*for_loop.start_range, scope);
  llvm::Value* end_value = codegen_expression(*for_loop.end_range, scope);

  auto& body = for_loop.body;
  auto& loop_variable_symbol = body.scope.add(Symbol(
    for_loop.loop_variable, for_loop.type, Symbol::LOCAL));

  llvm::Function* current_function = get_current_function();
  llvm::AllocaInst* loop_variable = create_entry_alloca(current_function, &loop_variable_symbol);
  ir_builder.CreateStore(start_value, loop_variable);

  auto loop_range_type = remove_reference(for_loop.end_range->meta.type);
  auto& loop_end_range_symbol = body.scope.add(Symbol(
    SymbolName(LOOP_RANGE_END), loop_range_type, Symbol::LOCAL));
  llvm::AllocaInst* range_end = create_entry_alloca(current_function, &loop_end_range_symbol);

  ir_builder.CreateStore(end_value, range_end);

  llvm::BasicBlock* for_check = llvm::BasicBlock::Create(
    llvm_context, "for_check", current_function);

  llvm::BasicBlock* for_body = llvm::BasicBlock::Create(
    llvm_context, "for_body");

  llvm::BasicBlock* for_inc = llvm::BasicBlock::Create(
    llvm_context, "for_inc");

  llvm::BasicBlock* for_end = llvm::BasicBlock::Create(
    llvm_context, "for_end");

  push_loop_info(for_end, for_inc);

  ir_builder.CreateBr(for_check);
  ir_builder.SetInsertPoint(for_check);

  /* For loop check -- should stop or keep looping */
  Ast_Binary_Operation loop_cond;
  loop_cond.operation = Ast_Operator::LESS_THAN;
  loop_cond.left = mank_ctx.new_expr(for_loop.loop_variable);
  loop_cond.right = ast_builder.make_ident(LOOP_RANGE_END);
  loop_cond.left->meta.type = loop_cond.right->meta.type = loop_range_type;

  llvm::Value* loop_check = codegen_expression(loop_cond, body.scope);
  ir_builder.CreateCondBr(loop_check, for_body, for_end);

  /* For loop body */
  // Probably could safely add directly
  current_function->getBasicBlockList().push_back(for_body);
  ir_builder.SetInsertPoint(for_body);

  codegen_expression(body, scope);

  create_exit_br(for_inc);
  current_function->getBasicBlockList().push_back(for_inc);
  ir_builder.SetInsertPoint(for_inc);

  /* For loop increment */
  // FIXME: Temp hack till I figure out proper ranges!
  auto& primative_loop_type = std::get<PrimativeType>(loop_range_type->v);
  auto loop_inc_literal = ast_builder.make_literal(primative_loop_type.tag, "");
  loop_inc_literal->meta.const_value = 1;

  Ast_Binary_Operation next_loop_value;
  next_loop_value.operation = Ast_Operator::PLUS;
  next_loop_value.left = mank_ctx.new_expr(for_loop.loop_variable);
  next_loop_value.right = loop_inc_literal;
  next_loop_value.left->meta.type = next_loop_value.right->meta.type = loop_range_type;

  Ast_Assign inc_loop;
  inc_loop.target = mank_ctx.new_expr(for_loop.loop_variable);
  inc_loop.expression = mank_ctx.new_expr(next_loop_value);

  codegen_statement(inc_loop, body.scope);
  // End FIXME

  // Loop back to the start
  ir_builder.CreateBr(for_check);
  body.scope.destroy_locals();

  current_function->getBasicBlockList().push_back(for_end);
  ir_builder.SetInsertPoint(for_end);
  pop_loop_info();
}

void LLVMCodeGen::codegen_statement(Ast_Loop& loop, Scope& scope) {
  llvm::Function* current_function = get_current_function();
  llvm::BasicBlock* loop_body = llvm::BasicBlock::Create(
    llvm_context, "loop_body", current_function);
  llvm::BasicBlock* loop_end = llvm::BasicBlock::Create(
    llvm_context, "loop_end");
  push_loop_info(loop_end, loop_body);
  ir_builder.CreateBr(loop_body); // enter loop
  ir_builder.SetInsertPoint(loop_body);
  codegen_expression(loop.body, scope);
  create_exit_br(loop_body); // repeat?
  current_function->getBasicBlockList().push_back(loop_end);
  ir_builder.SetInsertPoint(loop_end);
  pop_loop_info();
}

void LLVMCodeGen::codegen_statement(Ast_While_Loop& while_loop, Scope& scope) {
  llvm::Function* current_function = get_current_function();
  llvm::BasicBlock* while_check = llvm::BasicBlock::Create(
    llvm_context, "while_check", current_function);
  llvm::BasicBlock* while_body = llvm::BasicBlock::Create(
    llvm_context, "while_body");
  llvm::BasicBlock* while_end = llvm::BasicBlock::Create(
    llvm_context, "while_end");
  push_loop_info(while_end, while_check);
  ir_builder.CreateBr(while_check);
  ir_builder.SetInsertPoint(while_check);
  llvm::Value* cond = codegen_expression(*while_loop.cond, scope);
  ir_builder.CreateCondBr(cond, while_body, while_end);
  current_function->getBasicBlockList().push_back(while_body);
  ir_builder.SetInsertPoint(while_body);
  codegen_expression(while_loop.body, scope);
  create_exit_br(while_check);
  current_function->getBasicBlockList().push_back(while_end);
  ir_builder.SetInsertPoint(while_end);
  pop_loop_info();
}

void LLVMCodeGen::codegen_statement(Ast_Loop_Control& loop_control, Scope& scope) {
  (void) scope;
  assert(!current_loop_info.empty() && "must be in a loop!");
  auto& loop_info = get_loop_info();
  switch (loop_control.type) {
    case Ast_Loop_Control::BREAK:
      create_exit_br(loop_info.loop_end);
      break;
    case Ast_Loop_Control::CONTINUE:
      create_exit_br(loop_info.loop_head);
      break;
    default:
      assert(false && "fix me! unknown loop control");
  }
}

// FIXME: This function is dumb
void LLVMCodeGen::codegen_value_bind(
  Ast_Bind& bind,
  Type_Ptr value_type,
  Ast_Identifier& bound_name,
  ExpressionExtract& aggregate,
  std::vector<unsigned> const & idxs,
  Scope& scope,
  llvm::Twine const & codename
) {
  llvm::Value* value = aggregate.get_bind(idxs, bind.type, codename);
  if (value_type) {
    // FIXME: I don't think this should be here
    value = dereference(value, value_type);
  }
  codegen_value_bind(bind, bound_name, value, scope);
}

void LLVMCodeGen::codegen_value_bind(
  Ast_Bind& bind, Ast_Identifier& bound_name, llvm::Value* value, Scope& scope
) {
  llvm::Function* current_function = get_current_function();
  auto& local_symbol = scope.add(Symbol(bound_name, bind.type, Symbol::LOCAL));
  llvm::AllocaInst* alloca = create_entry_alloca(current_function, &local_symbol);
  // This will generate worse IR than using a var decl
  // (since if you have an array it will copy it rather than inplace init)
  // but llvm should be able to figure that out.
  ir_builder.CreateStore(value, alloca);
}

void LLVMCodeGen::codegen_tuple_bindings(
  Ast_Tuple_Binds& tuple_binds, ExpressionExtract& tuple,
  std::vector<unsigned> idxs, Scope& scope
){
  using namespace mpark::patterns;
  uint gep_idx = 0;
  for (auto& binding: tuple_binds.binds) {
    idxs.push_back(gep_idx);
    match(binding) (
      pattern(as<Ast_Bind>(arg)) = [&](auto& bind){
        codegen_value_bind(bind, nullptr, bind.name, tuple, idxs, scope, "bound_tuple");
      },
      pattern(as<Ast_Tuple_Binds>(arg)) = [&](auto& nested_binds) {
        codegen_tuple_bindings(nested_binds, tuple, idxs, scope);
      },
      pattern(as<Ast_Pod_Binds>(arg)) = [&](auto& nested_binds) {
        codegen_pod_bindings(nested_binds, tuple, idxs, scope);
      }
    );
    idxs.pop_back();
    ++gep_idx;
  }
}

void LLVMCodeGen::codegen_pod_bindings(
  Ast_Pod_Binds& pod_binds, ExpressionExtract& pod,
  std::vector<unsigned> idxs, Scope& scope
) {
  using namespace mpark::patterns;
  // (could be a string or an array -- special cases)
  auto agg_type = remove_reference(pod_binds.pod_type);

  auto get_nested_extractor = [&](bool reference_field) {
    if (reference_field) {
      llvm::Value* ptr = pod.get_value(idxs, "field_ptr");
      ExpressionExtract nested_pod(this, ptr, true);
      return std::make_pair(nested_pod, std::vector<unsigned>{});
    }
    return std::make_pair(pod, idxs);
  };

  for (auto& field_bind: pod_binds.binds) {
    Type_Ptr field_type;
    bool field_is_ref = false;
    // for special fields who's values are hallucinated
    llvm::Value* value_override = nullptr;
    idxs.push_back(field_bind.field_index);

    // FIXME! Can't think of a better way right now :(
    match(agg_type->v)(
      pattern(as<Ast_Pod_Declaration>(arg)) = [&](auto& pod_type){
        field_type = pod_type.get_field_type(field_bind.field_index);
        field_is_ref = is_reference_type(field_type);
      },
      pattern(anyof(as<PrimativeType>(arg), as<ListType>(arg))) = [&](auto& vec_like){
        using T = std::decay_t<decltype(vec_like)>;
        uint length_offset;
        auto constexpr is_vec = std::is_same_v<T, ListType>;
        if constexpr (is_vec) {
          length_offset = Builtin::Vector::DATA;
        } else {
          // strings
          assert(vec_like.is_string_type());
          length_offset = Builtin::String::LENGTH;
        }
        idxs.back() = length_offset;
        // Type passed to get_bind() does not matter (just needs to not be a ref)
        value_override = pod.get_bind(idxs, PrimativeType::int_ty(), "length");
        value_override = is_vec
          ? get_vector_length(value_override)
          : fix_string_length(value_override);
      }, pattern(as<FixedSizeArrayType>(arg)) = [&](auto& array_type) {
        value_override = create_llvm_idx(array_type.size);
      }
    );

    match(field_bind.replacement)(
      pattern(as<Ast_Bind>(arg)) = [&](auto& bind){
        if (!value_override) {
          codegen_value_bind(bind, field_type,
            *field_bind.bound_name, pod, idxs, scope, "pod_bind");
        } else {
          codegen_value_bind(bind, *field_bind.bound_name, value_override, scope);
        }
      },
      pattern(as<Ast_Pod_Binds>(arg)) = [&](auto& nested_binds){
        auto [nested_pod, nested_idxs] = get_nested_extractor(field_is_ref);
        codegen_pod_bindings(nested_binds, nested_pod, nested_idxs, scope);
      },
      pattern(as<Ast_Tuple_Binds>(arg)) = [&](auto& nested_binds){
        auto [nested_pod, nested_idxs] = get_nested_extractor(field_is_ref);
        codegen_tuple_bindings(nested_binds, nested_pod, nested_idxs, scope);
      }
    );
    idxs.pop_back();
  }
}

void LLVMCodeGen::codegen_statement(Ast_Structural_Binding& bindings, Scope& scope) {
  using namespace mpark::patterns;
  match(bindings.bindings)(
    pattern(as<Ast_Tuple_Binds>(arg)) = [&](auto& tuple_binds){
      auto tuple_extract = get_tuple_extractor(*bindings.initializer, scope);
      codegen_tuple_bindings(tuple_binds, tuple_extract, {}, scope);
    },
    pattern(as<Ast_Pod_Binds>(arg)) = [&](auto& pod_binds) {
      auto pod_extract = get_value_extractor(*bindings.initializer, scope);
      codegen_pod_bindings(pod_binds, pod_extract, {}, scope);
    }
  );
}

/* Expressions */

llvm::Value* LLVMCodeGen::dereference(llvm::Value* value, Type_Ptr type) {
  if (type && is_reference_type(type)) {
    // We want to dereference it
    return ir_builder.CreateLoad(value, "dereference");
  } else {
    return value;
  }
}

llvm::Value* LLVMCodeGen::address_of(Ast_Expression& expr, Scope& scope) {
  using namespace mpark::patterns;
  // must be lvalue
  // index access, field access, variable
  assert(expr.is_lvalue() && "fix me! expression must be lvalue to have address!");

  return match(expr.v)(
    pattern(as<Ast_Identifier>(arg)) = [&](auto& variable) -> llvm::Value* {
      auto type = variable.get_meta().type;
      llvm::Value* variable_alloca = get_local(variable, scope);
      return dereference(variable_alloca, type);
    },
    pattern(as<Ast_Field_Access>(arg)) = [&](auto& access) -> llvm::Value* {
      std::vector<uint> idx_list;
      auto& source_object = flatten_nested_pod_accesses(access, idx_list);
      llvm::Value* source_address = address_of(source_object, scope);
      llvm::Value* field_value = ir_builder.CreateGEP(
        source_address, make_idx_list_for_gep(idx_list), access.field.name);
      auto pod_type = std::get<Ast_Pod_Declaration>(remove_reference(access.object->meta.type)->v);
      auto type = pod_type.fields.at(idx_list.back()).type;
      return dereference(field_value, type);
    },
    pattern(as<Ast_Index_Access>(arg)) = [&](auto& index) -> llvm::Value* {
      if (index.object->meta.type
        && std::holds_alternative<ListType>(remove_reference(index.object->meta.type)->v)
      ) {
        return index_vector(index, scope);
      } else {
        std::vector<llvm::Value*> idx_list;
        idx_list.push_back(create_llvm_idx(0));
        auto& source_object = flatten_nested_array_indexes(index, scope, idx_list);
        llvm::Value* source_address = address_of(source_object, scope);
        return ir_builder.CreateGEP(
          source_address, idx_list, "index_access");
      }
    },
    pattern(as<Ast_Call>(arg)) = [&](auto& call) -> llvm::Value* {
      // Must be a reference returned (so that is an address)
      return codegen_expression(call, scope);
    },
    pattern(anyof(as<Ast_Block>(arg), as<Ast_If_Expr>(arg))) =
    [&](auto& block_expr) -> llvm::Value* {
      return codegen_expression(block_expr, scope, true);
    },
    pattern(as<Ast_Unary_Operation>(arg)) = [&](auto& ref_unary) -> llvm::Value* {
      return address_of(*ref_unary.operand, scope);
    },
    pattern(as<Ast_Spawn>(arg)) = [&](auto& spawn) -> llvm::Value* {
      (void) spawn;
      return nullptr; // todo
    },
    pattern(_) = []() -> llvm::Value* {
      assert(false && "fix me! address_of not implemented for lvalue");
      return nullptr;
    }
  );
}

llvm::Value* LLVMCodeGen::codegen_bind(
  Ast_Expression& expr, Type_Ptr bound_to, Scope& scope
) {
  if (is_reference_type(bound_to)) {
    return address_of(expr, scope);
  } else {
    return codegen_expression(expr, scope);
  }
}

llvm::Value* LLVMCodeGen::codegen_expression(Ast_Expression& expr, Scope& scope, bool as_lvalue) {
  return std::visit([&](auto& expr) {
    using T = std::decay_t<decltype(expr)>;
    // Kinda a hack
    if constexpr (std::is_same_v<T, Ast_Block> || std::is_same_v<T, Ast_If_Expr>) {
      return codegen_expression(expr, scope, as_lvalue);
    }
    return codegen_expression(expr, scope);
  }, expr.v);
}

llvm::Value* LLVMCodeGen::codegen_expression(Ast_Block& block, Scope& scope, bool as_lvalue) {
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

    if (!as_lvalue) {
      return codegen_expression(*final_expr, block.scope);
    } else {
      assert(block.get_meta().value_type == Expression_Meta::LVALUE);
      return address_of(*final_expr, block.scope);
    }
  }

  return nullptr;
}

llvm::Value* LLVMCodeGen::codegen_expression(Ast_If_Expr& if_expr, Scope& scope, bool as_lvalue) {
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
  llvm::Value* then_value = codegen_expression(*if_expr.then_block, scope, as_lvalue);
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
    else_value = codegen_expression(*if_expr.else_block, scope, as_lvalue);
    create_exit_br(end_block);
    else_block = ir_builder.GetInsertBlock();
  }

  /* end */
  current_function->getBasicBlockList().push_back(end_block);
  ir_builder.SetInsertPoint(end_block);

  if (then_value && else_value) {
    auto if_type = if_expr.get_meta().type;
    if (!as_lvalue) {
      if_type = remove_reference(if_type);
    }
    llvm::PHINode* phi = ir_builder.CreatePHI(
      map_type_to_llvm(if_type.get(), scope), 2, "if_expr_selection");
    phi->addIncoming(then_value, then_block);
    phi->addIncoming(else_value, else_block);
    return phi;
  }
  return nullptr;
}

llvm::Value* LLVMCodeGen::codegen_builtin_vector_calls(
  Ast_Call& call, Ast_Function_Declaration& func_type, Scope& scope
) {
  /* Pretty much just some prototype vector stuff */
  using namespace Builtin;
  using namespace mpark::patterns;

  auto expr_to_void_pointer = [&](Expr_Ptr expr, auto name) {
    llvm::Value* expr_ptr = nullptr;
    // Passed as a pointer (so must have an address)
    if (expr->is_lvalue()) {
      expr_ptr = address_of(*expr, scope);
    } else {
      expr_ptr = create_entry_alloca(
        get_current_function(), scope, expr->meta.type.get(), name);
      ir_builder.CreateStore(
        codegen_expression(*expr, scope), expr_ptr);
    }

    expr_ptr = ir_builder.CreateBitCast(
      expr_ptr, llvm::Type::getInt8PtrTy(llvm_context));

    return expr_ptr;
  };

  return match(func_type.identifier.name)(
    pattern("new_vec") = [&]() -> llvm::Value* {
      auto& vec_type = std::get<ListType>(call.get_meta().type->v);
      llvm::Type* llvm_vec_type = get_vector_ty(scope);
      llvm::Type* el_type = map_type_to_llvm(vec_type.element_type.get(), scope);

      llvm::Value* element_size = llvm::ConstantExpr::getSizeOf(el_type);
      llvm::Value* vec_ptr = create_entry_alloca(
        get_current_function(), llvm_vec_type, "vec_temp");

      llvm::Function* vec_init = get_init_vec(scope);
      ir_builder.CreateCall(vec_init, { vec_ptr, element_size });

      return ir_builder.CreateLoad(vec_ptr, "new_vec");
    },
    pattern("push_back") = [&]() -> llvm::Value* {
      auto vec = call.arguments.at(0);
      auto element_to_insert = call.arguments.at(1);

      llvm::Value* to_insert_ptr = expr_to_void_pointer(element_to_insert, "to_insert");
      llvm::Function* vec_push_back = get_vec_push_back(scope);
      return ir_builder.CreateCall(vec_push_back,
        { address_of(*vec, scope), to_insert_ptr });
    },
    pattern("pop_back") = [&]() -> llvm::Value* {
      auto vec = call.arguments.at(0);
      llvm::Function* vec_pop_back = get_vec_pop_back(scope);
      return ir_builder.CreateCall(vec_pop_back, { address_of(*vec, scope) });
    },
    pattern("fill_vec") = [&]() -> llvm::Value* {
      auto vec = call.arguments.at(0);
      auto fill = call.arguments.at(1);
      auto count = call.arguments.at(2);

      llvm::Value* fill_ptr = expr_to_void_pointer(fill, "fill");
      llvm::Value* count_value = codegen_expression(*count, scope);
      count_value = ir_builder.CreateIntCast(
        count_value, llvm::Type::getInt64Ty(llvm_context), false);

      llvm::Function* vec_fill = get_vec_fill(scope);
      return ir_builder.CreateCall(vec_fill,
        { address_of(*vec, scope), fill_ptr, count_value});
    },
    pattern(_) = []() -> llvm::Value* {
      // llvm create_entry_alloca()
      assert(false && "todo! Only hack special case vector generics done");
      return nullptr;
    }
  );
}

llvm::Value* LLVMCodeGen::codegen_expression(Ast_Call& call, Scope& scope) {
  auto callee_type = remove_reference(call.callee->meta.type);
  LambdaType* lambda_type = std::get_if<LambdaType>(&callee_type->v);
  Ast_Function_Declaration* function_type = nullptr;

  llvm::Value* callee;
  llvm::Value* env_ptr = nullptr;
  if (!lambda_type) {
    function_type = std::get<Ast_Function_Declaration>(callee_type->v).get_raw_self();
    if (function_type->generic) {
      /*
        We special case the vector stuff and handle it here.
        Vectors are fake generic as they're type erased at runtime.
      */
      return codegen_builtin_vector_calls(call, *function_type, scope);
    }
    callee = this->get_function(*function_type);
  } else {
    auto expr_result = get_value_extractor(*call.callee, scope);
    env_ptr = expr_result.get_value({0}, "env_ptr");
    callee = expr_result.get_value({1}, "lambda_func");
  }

  std::vector<llvm::Value*> call_args;
  call_args.reserve(call.arguments.size());

  uint arg_idx = 0;
  for (auto& arg: call.arguments) {
    Type_Ptr arg_type;
    if (function_type) {
      arg_type = function_type->arguments.at(arg_idx).type;
    } else {
      arg_type = lambda_type->argument_types.at(arg_idx);
    }
    call_args.push_back(codegen_bind(*arg, arg_type, scope));
    ++arg_idx;
  }

  if (lambda_type) {
    // Pass lambda env
    call_args.push_back(env_ptr);
  }

  bool has_return = (lambda_type && lambda_type->return_type.get())
    || (function_type && !function_type->procedure);

  return ir_builder.CreateCall(callee, call_args, has_return ? "call_ret" : "");
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
    case PrimativeType::INTEGER64:
      return llvm::ConstantInt::get(llvm_context,
        llvm::APInt(
          /*bits:*/ literal.size_bytes(),
          /*value:*/ literal.as_int64(),
          /*signed:*/ true));
    case PrimativeType::FLOAT32:
      return llvm::ConstantFP::get(llvm_context,
        llvm::APFloat(/*value:*/ literal.as_float32()));
    case PrimativeType::FLOAT64:
      return llvm::ConstantFP::get(llvm_context,
          llvm::APFloat(/*value:*/ literal.as_float64()));
    case PrimativeType::STRING: {
      auto str_value = literal.as_string();
      llvm::Value* raw_str = ir_builder.CreateGlobalStringPtr(str_value, "!const_str_init");
      llvm::Value* length = llvm::ConstantInt::get(
        llvm::Type::getInt64Ty(llvm_context), str_value.length());
      return create_string(raw_str, length, scope);
    }
    case PrimativeType::BOOL:
      return llvm::ConstantInt::get(llvm_context,
        llvm::APInt(/*bits:*/ literal.size_bytes(),
                    /*value: */ literal.as_bool(),
                    /*signed: */ false));
    case PrimativeType::CHAR:
      return llvm::ConstantInt::get(llvm_context,
        llvm::APInt(/*bits*/ literal.size_bytes(),
                    /*value:*/ literal.as_char(),
                    /*signed*/ true));
    default:
      assert(false && "fix me! codegen for unknown literal type");
  }
}

llvm::Value* LLVMCodeGen::codegen_expression(Ast_Identifier& ident, Scope& scope) {
  Symbol* symbol = scope.lookup_first_name(ident);
  assert(symbol->is_local() && "only locals implemented");

  if (ident.name.at(0) == '%') {
    // Special pre-codegened values
    auto* meta_ptr = symbol->meta.get();
    if (ident.name == Builtin::MANK_BOUNDSCHECK) {
      // Little hack for adding bounds checks
      auto& boundscheck = *static_cast<SymbolMetaBoundsCheck*>(meta_ptr);
      raise_mank_builtin_bounds_error(
        boundscheck.length, boundscheck.index, boundscheck.location);
      return nullptr;
    }
    return static_cast<SymbolMetaLocal*>(meta_ptr)->alloca;
  }

  llvm::Value* variable_address = address_of(*ident.get_self().class_ptr(), scope);
  return ir_builder.CreateLoad(variable_address,
    formatxx::format_string("load_{}", ident.name));
}

llvm::Value* LLVMCodeGen::codegen_expression(Ast_Unary_Operation& unary, Scope& scope) {
  using namespace mpark::patterns;
  llvm::Value* operand = codegen_expression(*unary.operand, scope);

  // Special case for ref operations (if evaluted to a value they're just the operand)
  if (unary.operation == Ast_Operator::REF) {
    return operand;
  }

  auto unary_type = remove_reference(unary.operand->meta.type);
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

Expr_Ptr LLVMCodeGen::simplify_short_circuit(Ast_Binary_Operation& short_circuit) {
  // Just convert short circuited operations to if exprs (ez)
  bool or_not_and = short_circuit.operation == Ast_Operator::LOGICAL_OR;
  Expr_Ptr lhs = ast_builder.make_boolean(or_not_and, true);
  Expr_Ptr rhs = short_circuit.right;
  if (!or_not_and) {
    std::swap(lhs, rhs);
  }
  auto sc_if = ast_builder.make_if(short_circuit.left, lhs, rhs);
  std::get<Ast_If_Expr>(sc_if->v).get_meta().type = PrimativeType::get(PrimativeType::BOOL);
  return sc_if;
}

llvm::Value* LLVMCodeGen::codegen_expression(Ast_Binary_Operation& binop, Scope& scope) {
  using namespace mpark::patterns;
  auto binop_type = remove_reference(binop.left->meta.type);
  auto& binop_primative = std::get<PrimativeType>(binop_type->v);

  llvm::Value *left = nullptr, *right = nullptr;

  // both string and bools are special cases:
    // - string concat -> special function call
    // - bools -> short circuit code gen
    // so don't codegen their lhs/rhs (yet)
  if (
    !(binop_primative.tag == PrimativeType::STRING
      || binop_primative.tag == PrimativeType::BOOL)
  ) {
    left = codegen_expression(*binop.left, scope);
    right = codegen_expression(*binop.right, scope);
  }

  #define INT_TYPE anyof(PrimativeType::INTEGER, PrimativeType::INTEGER64, PrimativeType::CHAR)
  return match(binop_primative.tag, binop.operation)(
    /* Basic integer operations */
    pattern(INT_TYPE, Ast_Operator::PLUS) = [&]{
      return ir_builder.CreateAdd(left, right, "int_add");
    },
    pattern(INT_TYPE, Ast_Operator::MINUS) = [&]{
      return ir_builder.CreateSub(left, right, "int_sub");
    },
    pattern(INT_TYPE, Ast_Operator::DIVIDE) = [&]{
      return ir_builder.CreateSDiv(left, right, "int_div");
    },
    pattern(INT_TYPE, Ast_Operator::TIMES) = [&]{
      return ir_builder.CreateMul(left, right, "int_mult");
    },
    pattern(INT_TYPE, Ast_Operator::MODULO) = [&]{
      return ir_builder.CreateSRem(left, right, "int_mod");
    },
    pattern(INT_TYPE, Ast_Operator::LESS_THAN) = [&]{
      return ir_builder.CreateICmpSLT(left, right, "int_lt");
    },
    pattern(INT_TYPE, Ast_Operator::GREATER_THAN) = [&]{
      return ir_builder.CreateICmpSGT(left, right, "int_gt");
    },
    pattern(INT_TYPE, Ast_Operator::BITWISE_AND) = [&]{
      return ir_builder.CreateAnd(left, right, "int_bitwise_and");
    },
    pattern(INT_TYPE, Ast_Operator::BITWISE_OR) = [&]{
      return ir_builder.CreateOr(left, right, "int_bitwise_or");
    },
    pattern(INT_TYPE, Ast_Operator::GREATER_EQUAL) = [&]{
      return ir_builder.CreateICmpSGE(left, right, "int_ge");
    },
    pattern(INT_TYPE, Ast_Operator::LESS_EQUAL) = [&]{
      return ir_builder.CreateICmpSLE(left, right, "int_le");
    },
    pattern(INT_TYPE, Ast_Operator::EQUAL_TO) = [&]{
      return ir_builder.CreateICmpEQ(left, right, "int_eq");
    },
    pattern(INT_TYPE, Ast_Operator::NOT_EQUAL_TO) = [&]{
      return ir_builder.CreateICmpNE(left, right, "int_ne");
    },
    pattern(INT_TYPE, Ast_Operator::LEFT_SHIFT) = [&]{
      return ir_builder.CreateShl(left, right, "int_shl");
    },
    pattern(INT_TYPE, Ast_Operator::RIGHT_SHIFT) = [&]{
      // >> = arithmetic shift right (add >>> for logical ?)
      return ir_builder.CreateAShr(left, right, "int_ashr");
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
    /* String ops */
    pattern(PrimativeType::STRING, Ast_Operator::PLUS) = [&]{
      return create_string_concat(*binop.left, *binop.right, scope);
    },
    pattern(PrimativeType::BOOL,
      anyof(Ast_Operator::LOGICAL_AND, Ast_Operator::LOGICAL_OR)
    ) = [&]{
      return codegen_expression(*simplify_short_circuit(binop), scope);
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

Ast_Expression& LLVMCodeGen::flatten_nested_pod_accesses(
  Ast_Field_Access& access, std::vector<uint>& idx_list
) {
  Ast_Expression* base_expr;

  auto pior_is_reference = [](Ast_Field_Access& pior_access) {
    auto pior_type = remove_reference(pior_access.object->meta.type);
    auto pior_pod = std::get<Ast_Pod_Declaration>(pior_type->v);
    return is_reference_type(pior_pod.get_field_type(pior_access.field_index));
  };

  Ast_Field_Access* pior_access;
  if ((pior_access = std::get_if<Ast_Field_Access>(&access.object->v))
    && !pior_is_reference(*pior_access)
  ) {
    base_expr = &flatten_nested_pod_accesses(*pior_access, idx_list);
  } else {
    base_expr = access.object.get();
  }
  idx_list.push_back(access.field_index);
  return *base_expr;
}

llvm::Value* LLVMCodeGen::create_llvm_idx(uint value) {
  auto constexpr FIELD_IDX_BITS = 32;
  return llvm::ConstantInt::get(llvm_context,
    llvm::APInt(FIELD_IDX_BITS, value, value));
}

std::vector<llvm::Value*> LLVMCodeGen::make_idx_list_for_gep(
  std::vector<uint> const & idx_list
) {
  // For some reason there's no GEP that takes a list of uint (unlike extractvalue)
  // So we have to map uint -> llvm::Value* (just constant ints)
  std::vector<llvm::Value*> llvm_idx_list;
  llvm_idx_list.reserve(idx_list.size() + 1);
  llvm_idx_list.push_back(create_llvm_idx(0)); // First base index for GEP
  std::transform(idx_list.begin(), idx_list.end(), std::back_inserter(llvm_idx_list),
    std::bind1st(std::mem_fn(&LLVMCodeGen::create_llvm_idx), this));

  return llvm_idx_list;
}

llvm::Value* LLVMCodeGen::get_vector_length(llvm::Value* data_ptr) {
  data_ptr = ir_builder.CreateBitCast(data_ptr,
    llvm::Type::getInt64PtrTy(llvm_context));
  llvm::Value* length_ptr = ir_builder.CreateConstInBoundsGEP1_32(
    llvm::Type::getInt64Ty(llvm_context), data_ptr, static_cast<uint>(Builtin::Vector::LENGTH));
  llvm::Value* vec_length = ir_builder.CreateLoad(length_ptr, "length");
  return fix_string_length(vec_length);
}

llvm::Value* LLVMCodeGen::get_special_field_value(
  Type_Ptr agg_type, Ast_Expression& agg, Scope& scope
) {
  using namespace mpark::patterns;
  return match(agg_type->v)(
    pattern(as<FixedSizeArrayType>(arg)) = [&](auto& array_type) {
      return create_llvm_idx(array_type.size);
    },
    pattern(as<PrimativeType>(_)) = [&]() -> llvm::Value* {
      auto [str_length, _] = extract_string_info(agg, scope);
      return fix_string_length(str_length);
    },
    pattern(as<ListType>(_)) = [&]() -> llvm::Value* {
      llvm::Value* vec_data = get_value_extractor(agg, scope)
        .get_value({static_cast<uint>(Builtin::Vector::DATA)}, "data");
      return get_vector_length(vec_data);
    },
    pattern(_) = []() -> llvm::Value* { return nullptr; });
}

llvm::Value* LLVMCodeGen::codegen_expression(Ast_Field_Access& access, Scope& scope) {
  auto pod_type = remove_reference(access.object->meta.type);

  // FIXME: Special case, hardcoded lengths.
  llvm::Value* special_value = get_special_field_value(pod_type, *access.object, scope);
  if (special_value) {
    return special_value;
  }

  auto accessed_type = std::get<Ast_Pod_Declaration>(pod_type->v)
    .get_field_type(access.field_index);

  std::vector<uint> idx_list;
  auto& source_object = flatten_nested_pod_accesses(access, idx_list);
  llvm::Value* field_value = get_value_extractor(source_object, scope).get_value(idx_list, access.field.name);

  // TODO: Must be better way...
  return dereference(field_value, accessed_type);
}

void LLVMCodeGen::initialize_aggregate(llvm::Value* ptr, Ast_Expression_List& values, Scope& scope) {
  using namespace mpark::patterns;
  auto agg_type = values.get_type();
  llvm::Type* llvm_agg_type = map_type_to_llvm(agg_type.get(), scope);
  uint gep_idx = 0;
  for (auto el: values.elements) {
    llvm::Value* element_ptr = ir_builder.CreateConstGEP2_32(
      llvm_agg_type, ptr, 0, gep_idx, "agg_element");
    match(el->v)(
      pattern(anyof(as<Ast_Array_Literal>(arg), as<Ast_Tuple_Literal>(arg))) =
      [&](auto& nested) {
        initialize_aggregate(element_ptr, nested, scope);
      },
      pattern(_) = [&]{
        llvm::Value* value = codegen_expression(*el, scope);
        ir_builder.CreateStore(value, element_ptr);
      }
    );
    ++gep_idx;
  }
}

void LLVMCodeGen::initialize_pod(llvm::Value* ptr, Ast_Pod_Literal& initializer, Scope& scope) {
  using namespace mpark::patterns;
  auto pod_type = initializer.pod;
  llvm::Type* llvm_pod_type = map_type_to_llvm(pod_type.get(), scope);
  for (auto& init: initializer.fields) {
    llvm::Value* field_ptr = ir_builder.CreateConstGEP2_32(
      llvm_pod_type, ptr, 0, init.field_index, "pod_field");
    match(init.initializer->v)(
      pattern(as<Ast_Pod_Literal>(arg)) =
      [&](auto& nested_pod){
        initialize_pod(field_ptr, nested_pod, scope);
      },
      pattern(anyof(as<Ast_Array_Literal>(arg), as<Ast_Tuple_Literal>(arg))) =
      [&](auto& nested_agg) {
        initialize_aggregate(field_ptr, nested_agg, scope);
      },
      pattern(_) = [&]{
        auto field_type = std::get<Ast_Pod_Declaration>(pod_type->v).get_field_type(init.field_index);
        llvm::Value* value = codegen_bind(*init.initializer, field_type, scope);
        ir_builder.CreateStore(value, field_ptr);
      }
    );
  }
}

llvm::Value* LLVMCodeGen::codegen_expression(Ast_Expression_List& array_like, Scope& scope) {
  /*
    I'm not sure this is much use as feature and may remove it
    It'd also be better codegen-ed with insertvalue, but that's more special cases

    This is the same for tuple expressions and array expressions.
    The LLVM types differ but gepping and setting elements is identical.
  */
  auto agg_type = array_like.get_type();
  llvm::AllocaInst* agg_alloca = create_entry_alloca(
    get_current_function(), scope, agg_type.get(), "agg_temp");
  initialize_aggregate(agg_alloca, array_like, scope);
  return ir_builder.CreateLoad(agg_alloca, "agg_expr");
}

void LLVMCodeGen::raise_mank_builtin_bounds_error(
  llvm::Value* length, llvm::Value* index,
  SourceLocation const & location
) {
  llvm::Value* source_line = create_llvm_idx(location.start_line);
  llvm::Value* source_col = create_llvm_idx(location.start_column);

  ir_builder.CreateCall(
    get_bounds_error(), { length, index, get_source_filename(), source_line, source_col });
}

void LLVMCodeGen::insert_bounds_check(
  llvm::Value* length, llvm::Value* index,
  Ast_Index_Access const & access, Scope& scope
) {
  auto create_precodegend_local = [&](std::string name, llvm::Value* value) {
    name = '%' + name;
    auto& sym = scope.add(Symbol(
      SymbolName(name), nullptr, Symbol::LOCAL));
    sym.meta = std::make_shared<SymbolMetaLocal>(value);
    return ast_builder.make_ident(name);
  };

  // Ensure length/index are compatible types & match error call
  length = ir_builder.CreateIntCast(
    length, llvm::Type::getInt64Ty(llvm_context), false);
  index = ir_builder.CreateIntCast(
    index, llvm::Type::getInt64Ty(llvm_context), true /* for now */);

  auto i64_ty = PrimativeType::get(PrimativeType::INTEGER64);

  auto idx = create_precodegend_local("idx", index);
  Ast_Binary_Operation _idx_less_than_zero, _idx_greater_than_length;
  // 0 > idx
  _idx_less_than_zero.left = ast_builder.make_integer64(0, true);
  _idx_less_than_zero.left->meta.type = i64_ty;
  _idx_less_than_zero.right = idx;
  _idx_less_than_zero.operation = Ast_Operator::GREATER_THAN;
  auto idx_less_than_zero = mank_ctx.new_expr(_idx_less_than_zero);
  idx_less_than_zero->meta.type = PrimativeType::bool_ty();
  // ||
  // idx >= length
  _idx_greater_than_length.left = idx;
  _idx_greater_than_length.left->meta.type = i64_ty;
  _idx_greater_than_length.right = create_precodegend_local("length", length);
  _idx_greater_than_length.operation = Ast_Operator::GREATER_EQUAL;
  auto idx_greater_than_length = mank_ctx.new_expr(_idx_greater_than_length);

  auto block = ast_builder.make_block(false);
  auto& err_body = std::get<Ast_Block>(block->v);

  // setting up magic %boundscheck "variable"
  err_body.scope.set_parent(scope);
  auto builtin_boundscheck = ast_builder.make_ident(Builtin::MANK_BOUNDSCHECK);
  auto& boundscheck = err_body.scope.add(Symbol(
    std::get<Ast_Identifier>(builtin_boundscheck->v), nullptr, Symbol::LOCAL));
  boundscheck.meta = std::make_shared<SymbolMetaBoundsCheck>(
    AstHelper::extract_location(access), length, index);

  err_body.statements.push_back(
    ast_builder.make_expr_stmt(builtin_boundscheck));

  /*
    if (out of bounds) {
      bounds_error()
    }
  */
  auto bounds_check = ast_builder.make_if_stmt(
    ast_builder.make_binary(
      Ast_Operator::LOGICAL_OR, idx_less_than_zero, idx_greater_than_length),
      block);

  codegen_statement(*bounds_check, scope);
}

Ast_Expression& LLVMCodeGen::flatten_nested_array_indexes(
  Ast_Index_Access& index, Scope& scope, std::vector<llvm::Value*>& idx_list
) {
  Ast_Expression* base_expr;

  auto pior_access_is_fixed_array = [](Ast_Index_Access& pior_index) {
    return std::holds_alternative<FixedSizeArrayType>(
      remove_reference(pior_index.object->meta.type)->v);
  };

  Ast_Index_Access* pior_index;
  if ((pior_index = std::get_if<Ast_Index_Access>(&index.object->v))
    && pior_access_is_fixed_array(*pior_index)
  ) {
    base_expr = &flatten_nested_array_indexes(*pior_index, scope, idx_list);
  } else {
    base_expr = index.object.get();
  }
  llvm::Value* idx = codegen_expression(*index.index, scope);
  auto object_type = remove_reference(index.object->meta.type)->v;
  insert_bounds_check(
    create_llvm_idx(std::get<FixedSizeArrayType>(object_type).size),
    idx, index, scope);
  idx_list.push_back(idx);
  return *base_expr;
}

llvm::Value* LLVMCodeGen::index_vector(Ast_Index_Access vector_index, Scope& scope) {
  // Will explode if not a vector index.
  auto& vec_type = std::get<ListType>(remove_reference(vector_index.object->meta.type)->v);
  llvm::Value* vec_data_ptr = get_value_extractor(
    *vector_index.object, scope).get_value({Builtin::Vector::DATA}, "vec_data_ptr");
  llvm::Value* index = codegen_expression(*vector_index.index, scope);
  insert_bounds_check(get_vector_length(vec_data_ptr), index, vector_index, scope);
  llvm::Type* el_type = map_type_to_llvm(vec_type.element_type.get(), scope);
  vec_data_ptr = ir_builder.CreateBitCast(
    vec_data_ptr, llvm::PointerType::get(el_type, 0));
  return ir_builder.CreateGEP(vec_data_ptr, { index }, "vec_index");
}

llvm::Value* LLVMCodeGen::codegen_expression(Ast_Index_Access& index, Scope& scope) {
  using namespace mpark::patterns;
  llvm::Value* element_ptr = match(remove_reference(index.object->meta.type)->v)(
    pattern(as<PrimativeType>(arg)) = [&](auto& str_type) -> llvm::Value* {
      // string indexes -- special case :(
      assert(str_type.is_string_type());
      auto [str_length, raw_str_pointer] = extract_string_info(*index.object, scope);
      llvm::Value* str_index = codegen_expression(*index.index, scope);
      insert_bounds_check(str_length, str_index, index, scope);
      return ir_builder.CreateGEP(raw_str_pointer, { str_index }, "str_index");
    },
    pattern(as<ListType>(_)) = [&]() -> llvm::Value* {
      // Pretty much the same as strings :(
      return index_vector(index, scope);
    },
    pattern(_) = [&]() -> llvm::Value* {
      std::vector<llvm::Value*> idx_list;
      idx_list.push_back(create_llvm_idx(0));
      auto& source_object = flatten_nested_array_indexes(index, scope, idx_list);

      llvm::Value* source_address = nullptr;
      if (source_object.is_lvalue()) {
        source_address = address_of(source_object, scope);
      } else {
        // TODO: Find better way?
        source_address = create_entry_alloca(
          get_current_function(), scope, source_object.meta.type.get(), "index_temp");
        ir_builder.CreateStore(codegen_expression(source_object, scope), source_address);
      }
      return ir_builder.CreateGEP(source_address, idx_list, "index_access");
    }
  );

  return ir_builder.CreateLoad(element_ptr, "load_element");
}

llvm::Value* LLVMCodeGen::create_lambda(
  llvm::Type* lambda_type, llvm::Function* body, llvm::Value* env_ptr
) {
  // Create lambda struct
  llvm::Value* lambda_details = ir_builder.CreateInsertValue(
    llvm::UndefValue::get(lambda_type), env_ptr, {0});
  lambda_details = ir_builder.CreateInsertValue(lambda_details, body, {1});
  return lambda_details;
}

llvm::Value* LLVMCodeGen::create_string(
  llvm::Value* raw_str_ptr, llvm::Value* length, Scope& scope
) {
  // size, ptr
  llvm::Value* string = ir_builder.CreateInsertValue(
    llvm::UndefValue::get(get_string_ty(scope)), length, {Builtin::String::LENGTH});
  string = ir_builder.CreateInsertValue(string, raw_str_ptr, {Builtin::String::DATA});
  return string;
}

llvm::Value* LLVMCodeGen::create_heap_alloc(
  llvm::Type* type, llvm::Twine const & name, llvm::Value** raw_ptr
) {
  llvm::BasicBlock* current_block = ir_builder.GetInsertBlock();
  llvm::Constant* type_size = llvm::ConstantExpr::getSizeOf(type);
  llvm::Type* llvm_i64 = llvm::Type::getInt64Ty(llvm_context);
  type_size = llvm::ConstantExpr::getTruncOrBitCast(type_size, llvm_i64);
  llvm::Instruction* type_malloc = llvm::CallInst::CreateMalloc(
    current_block,
    llvm_i64, // (think this is the pointer type?)
    type, type_size, nullptr, get_gc_malloc());
  if (raw_ptr) {
    *raw_ptr = &current_block->back();
  }
  ir_builder.Insert(type_malloc, name);
  return type_malloc;
}

llvm::Value* LLVMCodeGen::codegen_expression(Ast_Lambda& lambda, Scope& scope) {
  auto lambda_type = lambda.get_type();
  llvm::Type* llvm_lambda_type = map_type_to_llvm(lambda_type.get(), scope);
  llvm::Function* lambda_func = nullptr;
  llvm::Value* env_ptr = llvm::ConstantPointerNull::get(llvm::Type::getInt8PtrTy(llvm_context));

  if (lambda.top_level_wrapper) {
    // Only need to generate one top level wrapper per (top level) function.
    // The env is obviously always null.
    lambda_func = llvm_module->getFunction(lambda.identifier.name);
    if (lambda_func) {
      return create_lambda(llvm_lambda_type, lambda_func, env_ptr);
    }
  }

  ClosureInfo closure_info {
    .parent = &scope,
    .closure = &lambda.closure,
    .closure_type = nullptr
  };

  llvm::BasicBlock* saved_block = ir_builder.GetInsertBlock();
  lambda.generate_closure();
  if (!lambda.closure.empty()) {
    // Create closure type
    std::vector<llvm::Type*> closure_types;
    std::transform(lambda.closure.begin(), lambda.closure.end(),
      std::back_inserter(closure_types),
      [&](Symbol* capture){
        return map_type_to_llvm(capture->type.get(), scope);
      });
    closure_info.closure_type = llvm::StructType::create(
      llvm_context, closure_types, "!lambda_closure");

    // Malloc closure
    llvm::Value* closure = create_heap_alloc(
      closure_info.closure_type, "closure_malloc", &env_ptr);

    // Copy captures into the closure
    size_t capture_idx = 0;
    for (Symbol* capture: lambda.closure) {
      llvm::Value* capture_addr = ir_builder.CreateConstGEP2_32(
        closure_info.closure_type, closure, 0, capture_idx, "closure_element");
      ir_builder.CreateStore(
        ir_builder.CreateLoad(get_local(capture->name, scope), "capture"), capture_addr);
      ++capture_idx;
    }
  }

  this->current_closure_info.push(closure_info);
  lambda_func = codegen_function_header(lambda);
  codegen_function_body(lambda, lambda_func);
  this->current_closure_info.pop(); // remove closure info!
  ir_builder.SetInsertPoint(saved_block);

  return create_lambda(llvm_lambda_type, lambda_func, env_ptr);
}

llvm::Value* LLVMCodeGen::codegen_expression(Ast_Pod_Literal& pod, Scope& scope) {
  llvm::AllocaInst* pod_alloca = create_entry_alloca(
    get_current_function(), scope, pod.pod.get(), "pod_temp");
  initialize_pod(pod_alloca, pod, scope);
  return ir_builder.CreateLoad(pod_alloca, "pod_expr");
}

llvm::Value* LLVMCodeGen::do_cast(
  llvm::Value* value,
  Type_Ptr source_type,
  Type_Ptr target_type,
  Scope& scope
) {
  using namespace mpark::patterns;
  return match(source_type->v, target_type->v)(
    pattern(as<PrimativeType>(arg), as<PrimativeType>(arg)) = [&](auto& s, auto& t){
      auto has_int_storage = [](PrimativeType& p) {
        return p.tag == PrimativeType::CHAR || p.is_integer_type() || p.is_boolean_type();
      };
      llvm::Type* llvm_target = !t.is_string_type()
        ? map_primative_to_llvm(t.tag) : nullptr;
      if (has_int_storage(s) && has_int_storage(t)) {
        return ir_builder.CreateIntCast(value, llvm_target, s.is_signed());
      } else if (has_int_storage(s) && t.is_float_type()) {
        if (s.is_signed()) {
          return ir_builder.CreateSIToFP(value, llvm_target);
        } else {
          return ir_builder.CreateUIToFP(value, llvm_target);
        }
      } else if (s.is_float_type() && has_int_storage(t)) {
        if (t.is_signed()) {
          return ir_builder.CreateFPToSI(value, llvm_target);
        } else {
          return ir_builder.CreateFPToUI(value, llvm_target);
        }
      } else if (s.is_float_type() && t.is_float_type()) {
        return ir_builder.CreateFPCast(value, llvm_target);
      } else if (t.is_string_type()) {
        if (s.tag != PrimativeType::CHAR) {
          // FIXME: hack
          value = do_cast(
            value, source_type, PrimativeType::get(PrimativeType::CHAR), scope);
        }
        return create_char_string_cast(value, scope);
      } else {
        assert(false && "fix me! invalid primative cast");
      }
    } // TODO: special casts
  );
}

llvm::Value* LLVMCodeGen::codegen_expression(Ast_As_Cast& as_cast, Scope& scope) {
  auto source_type = remove_reference(as_cast.object->meta.type);
  auto target_type = as_cast.type;
  llvm::Value* source_value = codegen_expression(*as_cast.object, scope);
  return do_cast(source_value, source_type, target_type, scope);
}

Expr_Ptr LLVMCodeGen::mank_builtin_array_set(
  Type_Ptr array_type, Expr_Ptr initializer, Scope& scope
) {
  /*
    <I really don't have a good method of making these 'generic' builtins>
    // FIXME: Replace with builtin stdlib generic function when possible
    {
      !array:<array_type>;
      !init:<init_type> = <init>;
      !i:i32 = 0;
      loop {
        if !i >= <len> {
          break;
        }
        !array[!i] = !init;
        !i = !i + 1;
      }

    }
  */
  auto fixed_type = std::get<FixedSizeArrayType>(array_type->v);
  Ast_Loop init_loop;
  Ast_Loop_Control _break;
  _break.type = Ast_Loop_Control::BREAK;

  auto idx = ast_builder.make_ident("!i");
  auto array = ast_builder.make_ident("!array");
  auto array_idx = ast_builder.make_index(array, idx);
  array->meta.type = array_type;

  idx->meta.type = PrimativeType::int_ty();
  array_idx->meta.value_type
    = array->meta.value_type
    = idx->meta.value_type
    = Expression_Meta::LVALUE;

  auto end_of_array_check = ast_builder.make_binary(Ast_Operator::GREATER_EQUAL,
    idx, ast_builder.make_integer(fixed_type.size, true));

  auto inc_idx = ast_builder.make_binary(
    Ast_Operator::PLUS, idx, ast_builder.make_integer(1, true));

  init_loop.body = ast_builder.make_body(false,
    ast_builder.make_if_stmt(end_of_array_check,
      ast_builder.make_block(false,
        mank_ctx.new_stmt(_break))),
    ast_builder.make_assignment(array_idx, ast_builder.make_ident("!init")),
    ast_builder.make_assignment(idx, inc_idx));
  auto array_set_loop = mank_ctx.new_stmt(init_loop);

  auto array_set = ast_builder.make_block(true,
    ast_builder.make_var_decl("!array", array_type, nullptr),
    ast_builder.make_var_decl("!init", fixed_type.element_type, initializer),
    ast_builder.make_var_decl("!i", PrimativeType::int_ty(), ast_builder.make_integer(0, true)),
    array_set_loop, ast_builder.make_expr_stmt(array));

  // Fix scopes
  auto& array_set_scope = std::get<Ast_Block>(array_set->v).scope;
  auto& init_loop_scope = std::get<Ast_Loop>(array_set_loop->v).body.scope;
  array_set_scope.set_parent(scope);
  init_loop_scope.set_parent(array_set_scope);

  return array_set;
}

llvm::Value* LLVMCodeGen::codegen_expression(Ast_Array_Repeat& array_repeat, Scope& scope) {
  return codegen_expression(
    *mank_builtin_array_set(array_repeat.get_meta().type, array_repeat.initializer, scope), scope);
}

llvm::Value* LLVMCodeGen::codegen_expression(Ast_Spawn& spawn, Scope& scope) {
  auto& cell_type = std::get<CellType>(spawn.get_type()->v);
  llvm::Type* llvm_type = map_type_to_llvm(cell_type.contains.get(), scope);

  llvm::Value* spawn_ptr = create_heap_alloc(llvm_type, "spawn");

  //TODO: init

  return spawn_ptr;
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
