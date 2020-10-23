#include <cassert>

#include <mpark/patterns.hpp>
#include <formatxx/std_string.h>

#include "ast/types.h"

/* String helpers */

std::string type_to_string(Type& type) {
  using namespace std::string_literals;
  using namespace mpark::patterns;
  return match(type.v)(
    pattern(as<UncheckedType>(arg)) = [](auto& unchecked_type) {
      return formatxx::format_string("unchecked type - {}", unchecked_type.identifier.name);
    },
    pattern(as<PrimativeType>(arg)) = [](auto& primative_type) {
      return std::string(primative_type.name());
    },
    pattern(as<Ast_Pod_Declaration>(arg)) = [](auto& pod_type) {
      // std::string pod_str = "pod {";
      // for (auto it = pod_type.members.begin(); it != pod_type.members.end(); it++) {
      //   if (it != pod_type.members.begin()) {
      //     pod_str += " , ";
      //   }
      //   pod_str += type_to_string(*it->type);
      // }
      // pod_str += "}";
      // return pod_str;
      return formatxx::format_string("pod {}", pod_type.identifier.name);
    },
    pattern(_) = []{
      return "???"s;
    });
}

std::string type_to_string(Type* type) {
  if (type) {
    return type_to_string(*type);
  } else {
    return "Void";
  }
}

/* Helpers */

Type_Ptr extract_type_nullable(std::weak_ptr<Type> weak_type_ptr) {
  if (auto type_ptr = weak_type_ptr.lock()) {
    return type_ptr;
  }
  return nullptr;
}

Type_Ptr extract_type(std::weak_ptr<Type> weak_type_ptr) {
  if (auto type_ptr = extract_type_nullable(weak_type_ptr)) {
    return type_ptr;
  }
  assert(false && "fix me! expression type imformation is missing!");
}
