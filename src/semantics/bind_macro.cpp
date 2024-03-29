#include "sema/types.h"
#include "sema/macros.h"

#include "ast/ast_builder.h"
#include "errors/compiler_errors.h"

namespace Macros {

Ast_Expression_Type builtin_bind(
  Ast_Call& bind_call, AstBuilder& builder, Infer& infer, Lexer*
) {
  auto arg_count = bind_call.arguments.size();
  if (arg_count <= 0) {
    throw_error_at(bind_call, "bind needs some arguments");
  }

  LambdaType* lambda_type;
  auto bound_count = arg_count - 1;
  size_t current_bind = 0;
  std::vector<Type_Ptr> bind_types;
  bind_types.reserve(bind_call.arguments.size());
  bool first_arg = true;

  std::transform(
    bind_call.arguments.begin(),
    bind_call.arguments.end(), std::back_inserter(bind_types),
    [&](auto& arg) -> Type_Ptr {
      auto arg_type = arg->meta.type;
      if (first_arg) {
        lambda_type = std::get_if<LambdaType>(&arg_type->v);
        if (!lambda_type) {
          throw_error_at(arg, "cannot bind non-lambda type");
        } else if (bound_count > lambda_type->argument_types.size()) {
          throw_error_at(bind_call, "too many binds for lambda");
        }
        first_arg = false;
        return nullptr;
      }
      auto& target_type = lambda_type->argument_types.at(current_bind);
      infer.match_or_constrain_types_at(arg, target_type, arg_type,
        "cannot bind {} to {}");
      ++current_bind;
      return arg_type;
    });

  Ast_Lambda bound_lambda;
  bound_lambda.identifier.name = "!bind";
  bound_lambda.return_type = lambda_type->return_type;
  bound_lambda.procedure = !lambda_type->return_type;

  Ast_Call bound_call;
  bound_call.callee = bind_call.arguments.at(0);
  bound_call.arguments = std::vector<Expr_Ptr>(
      bind_call.arguments.begin() + 1, bind_call.arguments.end());

  for (auto idx = bound_count; idx < lambda_type->argument_types.size(); idx++) {
    auto arg_name = AstBuilder::to_internal_name(idx);
    bound_lambda.arguments.push_back(Ast_Argument{
      .type = lambda_type->argument_types.at(idx),
      .name = arg_name
    });
    bound_call.arguments.push_back(builder.to_expr_ptr(arg_name));
  }

  bound_lambda.body = AstBuilder::make_body(true,
    builder.make_expr_stmt(builder.to_expr_ptr(bound_call), true));
  bound_lambda.location = {};

  bind_call.get_self().class_ptr()->set_value_type(Expression_Meta::RVALUE);
  return bound_lambda;
}

}
