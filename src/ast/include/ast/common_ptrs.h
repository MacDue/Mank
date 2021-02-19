#pragma once

#include "ast/ptr.h"

/* Specialized pointers */
using Function_Ptr = SpAstPtr<Type, Ast_Function_Declaration>;
using Pod_Ptr = SpAstPtr<Type, Ast_Pod_Declaration>;
using Enum_Ptr = SpAstPtr<Type, Ast_Enum_Declaration>;
using Const_Ptr = SpAstPtr<Ast_Statement, Ast_Constant_Declaration>;
