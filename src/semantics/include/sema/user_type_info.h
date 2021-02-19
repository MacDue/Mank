#pragma once

#include <map>
#include <string>
#include <variant>

#include "ast/node.h"
#include "ast/construct.h"
#include "sema/sema_errors.h"

namespace UserTypes {
  template <typename UserType, typename MemberInfo>
  struct TypeInfo {
    SpAstPtr<Type, UserType> type;
    std::map<std::string_view, MemberInfo> members;

    bool has_member(Ast_Identifier const & member) const {
      return members.contains(member.name);
    }

    void add_member(Ast_Identifier const & member, MemberInfo info) {
      assert(!has_member(member));
      members.insert({member.name, info});
    }

    inline MemberInfo get_member_or_fail(Ast_Identifier const & member) const {
      if (!has_member(member)) {
        throw_sema_error_at(member, "{} contains no {} named \"{}\"",
          type->identifier.name, MemberInfo::name, member.name);
      }
      return members.at(member.name);
    }
  };

  struct PodFieldInfo {
    uint index;
    Type_Ptr type;
    static constexpr auto name = "field";
  };

  using PodInfo = TypeInfo<Ast_Pod_Declaration, PodFieldInfo>;

  struct EnumMemberInfo {
    uint ordinal;
    EnumMember::Data const * member;
    static constexpr auto name = "member";
  };

  using EnumInfo = TypeInfo<Ast_Enum_Declaration, EnumMemberInfo>;

  template<typename TypeInfo, typename... Rest> class TypeMapBase;

  template <typename TypeInfo>
  class TypeMapBase<TypeInfo> {
    using InfoMap = std::map<std::string_view, TypeInfo>;
    protected:
      InfoMap& get_info_map() { return info_map; }
      InfoMap const & get_info_map() const { return info_map; }
    private:
      InfoMap info_map;
  };

  template <typename TypeInfo, typename... Rest>
  struct TypeMapBase: public TypeMapBase<TypeInfo>, TypeMapBase<Rest...> {
    template <typename T>
    void add(Ast_Identifier const & ident, T info) {
      TypeMapBase<T>::get_info_map().insert({ ident.name, info });
    }

    template <typename T>
    bool contains(Ast_Identifier const & ident) const {
      return TypeMapBase<T>::get_info_map().contains(ident.name);
    }

    template <typename T>
    T& get(Ast_Identifier const & ident) {
      return TypeMapBase<T>::get_info_map().at(ident.name);
    }

    template <typename T>
    T const & get(Ast_Identifier const & ident) const {
      return TypeMapBase<T>::get_info_map().at(ident.name);
    }
  };

  using TypeMap = TypeMapBase<PodInfo, EnumInfo>;
}
