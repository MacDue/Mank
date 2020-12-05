#pragma once

#include <vector>

#include "ast/defs.h"

class ContextData;

/*
  This should be inlined to a raw pointer.
  Having it as my own class just allows me to experiment with implementations.

  It also gives _slightly_ more safey as only the AstContect can create non-null ptrs.
*/
template<typename T>
class AstPtr {
  // originally this was a vector index but I think deque will allow
  // direct pointers. I may try to opt this to a small < 8 bytes index maybe?
  T* ptr = nullptr;

  AstPtr(T* ptr): ptr{ptr} {};
  friend class ContextData;
public:
  AstPtr() = default;
  AstPtr(std::nullptr_t): AstPtr() {};

  T& operator*() const {
    assert(ptr != nullptr);
    return *ptr;
  }

  operator bool() const {
    return ptr != nullptr;
  }

  bool operator==(AstPtr<T> const & other) {
    return this->ptr == other.ptr;
  }

  bool operator!=(AstPtr<T> const & other) {
    return !operator==(other);
  }

  T* get() const {
    return ptr;
  }

  T* operator -> () const {
    return get();
  }
};

/*
  Self/Specialised pointer.
  Every node will have one of these.
  It provides an easy way to get a pointer to the TClass (wrapper) too.
  & the "self" type.
*/
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
