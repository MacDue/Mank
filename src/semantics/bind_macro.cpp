#include "sema/types.h"
#include "sema/macros.h"
#include "sema/sema_errors.h"

#include "ast/ast_builder.h"

namespace Macros {

Ast_Expression builtin_bind(Ast_Call& bind_call, Infer& constraints) {
  // auto arg_count = bind_call.arguments.size();
  // if (arg_count <= 0) {
  //   throw_sema_error_at(bind_call, "bind needs some arguments");
  // }

  // LambdaType* lambda_type;
  // auto bound_count = arg_count - 1;
  // size_t current_bind = 0;
  // std::vector<Type_Ptr> bind_types;
  // bind_types.reserve(bind_call.arguments.size());
  // bool first_arg = true;

  // std::transform(
  //   bind_call.arguments.begin(),
  //   bind_call.arguments.end(), std::back_inserter(bind_types),
  //   [&](auto& arg) -> Type_Ptr {
  //     auto arg_type = extract_type(arg->meta.type);
  //     if (first_arg) {
  //       lambda_type = std::get_if<LambdaType>(&arg_type->v);
  //       if (!lambda_type) {
  //         throw_sema_error_at(arg, "cannot bind non-lambda type");
  //       } else if (bound_count > lambda_type->argument_types.size()) {
  //         throw_sema_error_at(bind_call, "too many binds for lambda");
  //       }
  //       first_arg = false;
  //       return nullptr;
  //     }
  //     auto& target_type = lambda_type->argument_types.at(current_bind);
  //     // FIXME: Spanned constraints
  //     if (!match_types(target_type, arg_type, constraints)) {
  //       throw_sema_error_at(arg, "cannot bind {} to {}",
  //         type_to_string(arg_type.get()), type_to_string(target_type.get()));
  //     }
  //     ++current_bind;
  //     return arg_type;
  //   });

  // Ast_Lambda bound_lambda;
  // bound_lambda.identifier.name = "!bind";
  // bound_lambda.return_type = lambda_type->return_type;
  // bound_lambda.procedure = !lambda_type->return_type;

  // Ast_Call bound_call;
  // bound_call.callee = bind_call.arguments.at(0);
  // bound_call.arguments = std::vector<Expression_Ptr>(
  //     bind_call.arguments.begin() + 1, bind_call.arguments.end());

  // for (auto idx = bound_count; idx < lambda_type->argument_types.size(); idx++) {
  //   auto arg_name = to_internal_name(idx);
  //   bound_lambda.arguments.push_back(Ast_Argument{
  //     .type = lambda_type->argument_types.at(idx),
  //     .name = arg_name
  //   });
  //   bound_call.arguments.push_back(to_expr_ptr(arg_name));
  // }

  // bound_lambda.body = make_body(true, make_expr_stmt(to_expr_ptr(bound_call), true));
  // bound_lambda.location = {};

  // Ast_Expression expansion_result = bound_lambda;
  // expansion_result.set_value_type(Expression_Meta::RVALUE);
  // return expansion_result;
}

}
