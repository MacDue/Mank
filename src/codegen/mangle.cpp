#include <formatxx/std_string.h>

#include "ast/ast.h"
#include "codegen/mangle.h"

namespace ABI {

std::string mangle(Ast_Function_Declaration const & func) {
  // For now just do a very simple no standard mangle
  // Mank currently does not have any overloading so it's not an issue (yet)
  // TODO: More complex version [will be needed for namespaces and stuff]
  if (!func.c_function) {
    return formatxx::format_string("__mank_{}{}",
      func.test ? "test__" : "_", func.identifier.name);
  } else {
    return func.identifier.name;
  }
}

}
