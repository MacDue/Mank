#pragma once

#include <set>
#include <map>
#include <utility>
#include <optional>
#include <stdexcept>

#include "ast/types.h"
#include "errors/compiler_errors.h"
#include "errors/compiler_message.h"

// make it take a std::function add_message
class Infer {
  AstContext& ctx;
  // inline std::vector<CompilerMessage>* hack_backtrack_infer = nullptr;
  // subs reasoning = map tvar id -> source location, tvar id
  // then when unify fails apply current subs to reasoning & track to where it first go it's concrete type
  std::map<int32_t, std::vector<std::pair<SourceLocation, Type_Ptr>>> unify_reasoning;
public:
  Infer(AstContext& ctx): ctx{ctx} {}

  class UnifyError: public std::exception {
    std::string error_message;
  public:
    UnifyError(std::string error_message)
      : error_message{error_message} {}

    char const * what() const noexcept override {
      return error_message.c_str();
    }
  };

  using ConstraintOrigin = std::optional<SourceLocation>;
  static inline char const * DEFAULT_ERROR = "FIXME: add a error message!";
  class Constraint {
    ConstraintOrigin origin;
    Type_Ptr t1, t2;
    char const * error_template;

    std::set<TypeVar> tvars_closure;

    Constraint(SourceLocation origin, Type_Ptr t1, Type_Ptr t2,
      char const * error_template = DEFAULT_ERROR
    ): origin{origin}, t1{t1}, t2{t2}, error_template{error_template} {}

    // Only use for child constraints (easier to tie to t1/t2 than ctor them)
    Constraint(Type_Ptr t1, Type_Ptr t2)
      : origin{std::nullopt}, t1{t1}, t2{t2},
        error_template{"child constraint! You should not see this!"} {}

    Constraint(): Constraint(nullptr, nullptr) {}

    void init();

    friend class Infer;
  };

  class MakeConstraint {
    Infer& infer;
    SourceLocation origin;
    char const * error_template;

    MakeConstraint(Infer& infer, SourceLocation origin, char const * error_template)
      : infer{infer}, origin{origin}, error_template{error_template} {}
    friend class Infer;

    public:
      inline void operator()(Type_Ptr t1, Type_Ptr t2) const {
        infer.add_constraint(origin, t1, t2, error_template);
      }
  };

  inline MakeConstraint or_constrain(
    SourceLocation origin, char const * error_template = DEFAULT_ERROR
  ) {
    return MakeConstraint(*this, origin, error_template);
  }

  using ConstraintSet = std::vector<Constraint>;
  using Substitution = std::map<TypeVar, Type_Ptr>;

  ConstraintSet type_constraints;

  inline void reset_inference() {
    type_constraints = {};
    unify_reasoning = {};
  }

  Substitution unify_and_apply();

  /* Helpers */

  void add_constraint(SourceLocation, Type_Ptr t1, Type_Ptr t2,
    char const * error_template = "DEBUG: infer failed a = {}, b = {}");

  void generate_call_constraints(Type_Ptr& callee_type, Ast_Call& call);

  void generate_tuple_assign_constraints(Ast_Assign& tuple_assign);

  std::optional<Constraint> generate_tuple_destructure_constraints(
    TupleBinding const & bindings, Type_Ptr& init_type, SourceLocation loc);
private:
  Constraint const * top_failed_constraint = nullptr;
  [[ noreturn ]] void throw_unify_error(Constraint const & constraint);

  /* Sub */
  Type_Ptr substitute(
    Type_Ptr current_type, TypeVar tvar, Type_Ptr replacement,
    Substitution const & subs);

  /* Apply */
  Type_Ptr apply_type(Type_Ptr type, Substitution const & subs);
  void apply_constraint(Constraint& c, Substitution const & subs);
  void apply_subs(ConstraintSet& constraints, Substitution const & subs);

  /* Unify */
  using UnifyResult = std::pair<Substitution, std::optional<CompilerError>>;
  Substitution unify_var(TypeVar tvar, Type_Ptr type, ConstraintOrigin origin);
  Substitution try_unify_sub_constraints(Constraint const & parent, ConstraintSet&& sub_constraints);
  Substitution unify_one(Constraint const & c);
  UnifyResult unify(ConstraintSet&& constraints);

  /* Merge */
  void merge_left(Substitution& target, Substitution const & other);
};
