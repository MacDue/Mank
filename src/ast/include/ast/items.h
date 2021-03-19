#pragma once

#include <vector>
#include <optional>
#include "ast/node.h"

DEF_ITEM(Ast_Pod_Declaration) {
  Ast_Identifier identifier;
  std::vector<Ast_Argument> fields;
};

DEF_ITEM(Ast_Enum_Declaration) {
  struct Member {
    struct TupleData {
      std::vector<Type_Ptr> elements;
    };

    struct PodData {
      std::vector<Ast_Argument> fields;
    };

    using Data = std::variant<TupleData, PodData>;
    Ast_Identifier tag;
    std::optional<Data> data;
  };

  Ast_Identifier identifier;
  std::vector<Member> members;
};

DEF_ITEM(Ast_Type_Alias) {
  Ast_Identifier alias;
  Type_Ptr type;
};

using Ast_Item_Type = std::variant<
  Ast_Pod_Declaration,
  Ast_Enum_Declaration,
  Ast_Type_Alias
>;

class Ast_Item {
  Ast_Item(Ast_Item_Type v)
    : v{std::move(v)} {};
  friend class AstContext;
public:
  Ast_Item_Type v;
  Type_Ptr declared_type = nullptr;
};
