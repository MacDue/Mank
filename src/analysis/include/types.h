#pragma once

#include <string>
#include "primative_type.h"

struct Type;

using Type_Ptr = std::shared_ptr<Type>;

char const * literal_type_to_string(PrimativeTypeTag type);

std::string type_to_string(Type& type);
std::string type_to_string(Type* type);

Type_Ptr extract_type_nullable(std::weak_ptr<Type> weak_type_ptr);
Type_Ptr extract_type(std::weak_ptr<Type> weak_type_ptr);
