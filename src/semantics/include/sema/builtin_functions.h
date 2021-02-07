#pragma once

struct Scope;
struct AstBuilder;
struct AstContext;

namespace Builtin {
  void add_builtins_to_scope(Scope& scope, AstContext& ctx, AstBuilder& builder);

  void add_test_runner_main(Ast_File& file);
}
