#pragma once

#include <map>
#include <string>

#include "ast/node.h"
#include "sema/sema_errors.h"

struct PodInfo {
  // Type + Resolved index
  using FieldInfo = std::pair<Type_Ptr, uint>;

  SpAstPtr<Type, Ast_Pod_Declaration> type;
  std::map<std::string_view, FieldInfo> fields;

  inline FieldInfo get_field_or_fail(Ast_Identifier const & field) const {
    if (!fields.contains(field.name)) {
      throw_sema_error_at(field, "{} contains no field named \"{}\"",
        type->identifier.name, field.name);
    }
    return fields.at(field.name);
  }
};

using ResolvedPodInfoMap = std::map<std::string_view, PodInfo>;
