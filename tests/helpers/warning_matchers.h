#pragma once

#include <string>
#include "../catch/catch.hpp"

#include "compiler_message.h"

using WarningsMatcher = Catch::Matchers::Generic::PredicateMatcher<
  std::vector<CompilerMessage>
>;

WarningsMatcher HasWarning(std::string needle);
