#pragma once

#include <tsl/ordered_map.h>

#include "ast/node.h"
#include "errors/compiler_errors.h"

DEF_TYPE(PodType) {
  struct Field {
    uint index;
    Type_Ptr type;
    Ast_Identifier name;
  };

  Ast_Identifier identifier;
  tsl::ordered_map<std::string, Field> fields;

  inline bool has_field(Ast_Identifier const & field) const {
    return fields.contains(field.name);
  }

  inline void add_field(Ast_Identifier name, Type_Ptr type, uint index) {
    fields.insert({
      name.name, Field{.index = index, .type = type, .name = name }
    });
  }

  inline void assert_has_field(Ast_Identifier const & field) const {
    if (!has_field(field)) {
      throw_error_at(field, "{} has no field name \"{}\"",
        identifier.name, field.name);
    }
  }

  inline Field& get_field(Ast_Identifier const & field) {
    assert_has_field(field);
    return fields.at(field.name);
  }

  inline Field const & get_field(Ast_Identifier const & field) const {
    assert_has_field(field);
    return fields.at(field.name);
  }

  inline Type_Ptr get_field_type(Ast_Identifier const & field) const {
    return get_field(field).type;
  }

  inline Type_Ptr get_field_type(uint index) const {
    return fields.nth(index)->second.type;
  }
};
