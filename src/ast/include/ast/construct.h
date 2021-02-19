#pragma once

#include <vector>

#include "ast/node.h"
#include "ast/scope.h"
#include "ast/block.h"
#include "ast/context.h"
#include "ast/common_ptrs.h"
#include "ast/generic_type.h"

using TypeParam = SpAstPtr<Type, GenericType>;

DEF_TYPE(Ast_Function_Declaration), Ast_Node {
  bool external   :1 = false,
       c_function :1 = false,
       procedure  :1 = false,
       lambda     :1 = false,
       generic    :1 = false,
       test       :1 = false;
  Ast_Identifier identifier;
  std::vector<Ast_Argument> arguments;
  std::vector<TypeParam> type_parameters;
  Type_Ptr return_type;
  Ast_Block body;
};

DEF_TYPE(Ast_Pod_Declaration), Ast_Node {
  Ast_Identifier identifier;
  std::vector<Ast_Argument> fields;

  inline Type_Ptr get_field_type(size_t field_index) {
    return fields.at(field_index).type;
  }
};

struct EnumMember {
  struct TupleData {
    std::vector<Type_Ptr> elements;
  };

  struct PodData {
    std::vector<Ast_Argument> fields;
  };

  using Data = std::variant<TupleData, PodData>;

  uint ordinal;
  Ast_Identifier tag;
  std::optional<Data> data;
};

DEF_TYPE(Ast_Enum_Declaration), Ast_Node {
  Ast_Identifier identifier;
  std::vector<EnumMember> members;
};

struct Ast_File {
  Scope scope;
  std::string filename;
  std::vector<Function_Ptr> functions;
  std::vector<Pod_Ptr> pods;
  std::vector<Enum_Ptr> enums;
  std::vector<Const_Ptr> global_consts;
  AstContext ctx; // owns EVERYTHING

  Ast_File(std::string filename)
    : filename{filename} {}

  Ast_File(Ast_File&& file) = default;
};
