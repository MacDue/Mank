#pragma once

#include <string>
#include "ast/construct.h"

namespace ABI {

std::string mangle(Ast_Function_Declaration const & func);

}
