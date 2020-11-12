#include <unordered_set>

#include "ast/lambda.h"
#include "ast/visitor.h"

struct ClosureVisitor: BaseAstVisitor {
  Scope& lambda_scope;
  Closure& closure;
  std::unordered_set<std::string> captured;

  ClosureVisitor(Scope& lambda_scope, Closure& closure)
    : lambda_scope{lambda_scope}, closure{closure} {}

  void visit(Ast_Identifier& identifier) override {
    Symbol* symbol = lambda_scope.lookup_first_name(identifier);
    if (!symbol || !symbol->is_local()) {
      return;
    }
    // If this symbol is used & occurs in a scope outside the lambda
    // add it it to the closure
    if (symbol->scope->get_level() < lambda_scope.get_level()) {
      if (!captured.contains(identifier.name)) {
        closure.push_back(symbol);
        captured.insert(identifier.name);
      }
    }
  }
};

void Ast_Lambda::generate_closure() {
  assert(this->body.scope.get_parent() && "lambda must have parent!");
  ClosureVisitor closure_visitor(this->body.scope, this->closure);
  closure_visitor(*this);
}
