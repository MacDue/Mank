#pragma once

#include <tsl/ordered_map.h>
#include <optional>

#include "ast/node.h"
#include "ast/pod_type.h"
#include "ast/tuple_type.h"
#include "errors/compiler_errors.h"

DEF_TYPE(EnumType) {
  using Data = std::variant<TupleType, PodType>;
  struct Member {
    uint ordinal;
    Ast_Identifier tag;
    std::optional<Data> data;
  };

  Ast_Identifier identifier;
  tsl::ordered_map<std::string, Member> members;

  inline bool has_member(Ast_Identifier const & tag) const {
    return members.contains(tag.name);
  }

  inline void add_member(
    Ast_Identifier tag, uint ordinal, std::optional<Data> data = std::nullopt
  ) {
    members.insert({
      tag.name, Member { .ordinal = ordinal, .tag = tag, .data = data }
    });
  }

  inline void assert_has_member(Ast_Identifier const & tag) const {
    if (!has_member(tag)) {
      throw_error_at(tag, "enum {} contains no member named \"{}\"",
        identifier.name, tag.name);
    }
  }

  inline Member& get_member(Ast_Identifier const & tag) {
    assert_has_member(tag);
    return members.at(tag.name);
  }

  inline Member const & get_member(Ast_Identifier const & tag) const {
    assert_has_member(tag);
    return members.at(tag.name);
  }
};
