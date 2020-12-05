#include <boost/range/combine.hpp>
#include <boost/range/adaptor/reversed.hpp>

#include "sema/macros.h"
#include "sema/sema_errors.h"

#include "ast/ast_builder.h"

namespace Macros {

Ast_Expression_Type builtin_curry(
  Ast_Call& curry_call, AstBuilder& builder, Infer& infer
) {
  (void) infer; // not used

  if (curry_call.arguments.size() != 1) {
    throw_sema_error_at(curry_call, "curry expects one function");
  }

  auto given = curry_call.arguments.at(0);
  auto given_type = given->meta.type;
  auto lambda_type = std::get_if<LambdaType>(&given_type->v);

  if (!lambda_type) {
    throw_sema_error_at(given, "expected a lambda");
  }

  if (lambda_type->argument_types.size() <= 1) {
    throw_sema_error_at(given, "lambda must take at least two arguments to be curried");
  }

  bool returns_value = lambda_type->return_type != nullptr;

  Ast_Call final_call;
  final_call.location = {};
  final_call.callee = given;
  std::vector<Ast_Identifier> arg_names;

  size_t arg_idx = 0;
  for (auto& _: lambda_type->argument_types){
    Ast_Identifier arg_name(AstBuilder::to_internal_name(arg_idx));
    arg_names.push_back(arg_name);
    final_call.arguments.emplace_back(builder.to_expr_ptr(arg_name));
    ++arg_idx;
  }

  Type_Ptr current_return = lambda_type->return_type;
  LambdaType curry_type;
  Ast_Lambda curried;
  curried.procedure = !current_return;
  curried.body = AstBuilder::make_body(returns_value,
    builder.make_expr_stmt(builder.to_expr_ptr(final_call), returns_value));

  for (auto arg_info: boost::adaptors::reverse(
    boost::combine(arg_names, lambda_type->argument_types))
  ) {
    Ast_Argument arg;
    boost::tie(arg.name, arg.type) = arg_info;
    // curry parm / return
    curried.return_type = current_return;
    curried.arguments = { arg };
    curried.location = {};
    curry_type.return_type = current_return;
    curry_type.argument_types = { arg.type };

    if (arg.name.name == arg_names[0].name) {
      // Don't make next lambda for first arg
      break;
    }

    Ast_Lambda next_curry;
    next_curry.body = AstBuilder::make_body(true,
      builder.make_expr_stmt(builder.to_expr_ptr(curried), true));
    next_curry.return_type = builder.to_type_ptr(curry_type);

    current_return = next_curry.return_type;
    curried = next_curry;
  }

  curry_call.get_self().class_ptr()->set_value_type(Expression_Meta::RVALUE);
  return curried;
}

}
