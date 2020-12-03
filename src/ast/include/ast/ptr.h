#pragma once

#include <vector>

#include "ast/defs.h"

class ContextData;

template<typename T>
class AstPtr {
  std::vector<T>* objs = nullptr;
  size_t idx;

  AstPtr(std::vector<T>* objs, size_t idx)
    : objs{objs}, idx{idx} {};

  friend class ContextData;
public:
  AstPtr() = default;
  AstPtr(std::nullptr_t): AstPtr() {};

  T& operator*() const {
    // This reference is only a temp & should not be stored, only the AstPtr
    assert(objs != nullptr);
    return (*objs)[idx];
  }

  operator bool() const {
    return objs != nullptr;
  }

  T* get() const {
    if (*this) {
      return &(**this);
    }
    return nullptr;
  }

  T* operator -> () const {
    return get();
  }
};

template <typename TClass, typename TPtr>
class SpAstPtr {
  AstPtr<TClass> ptr;
public:
  SpAstPtr() = default;
  SpAstPtr(AstPtr<TClass> ptr)
    : ptr{ptr} {}

  SpAstPtr& operator=(AstPtr<TClass> const & ptr) {
    this->ptr = ptr;
    return *this;
  }

  TPtr& operator*() const {
    auto tptr = get();
    assert(tptr != nullptr);
    return *tptr;
  }

  TPtr* get() const {
    auto cptr = ptr.get();
    if (cptr) {
      return std::get_if<TPtr>(&cptr->v);
    }
    return nullptr;
  }

  TPtr* operator -> () const {
    return get();
  }

  AstPtr<TClass> class_ptr() const {
    return ptr;
  }

  operator bool() const {
    return get() != nullptr;
  }
};

using Type_Ptr = AstPtr<Type>;
using Expr_Ptr = AstPtr<Ast_Expression>;
using Stmt_Ptr = AstPtr<Ast_Statement>;
