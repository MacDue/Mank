#pragma once

struct SourceLocation {
  uint start_line = 0;
  uint start_column = 0;
  uint end_line = 0;
  uint end_column = 0;
  uint start_char_idx = 0;
  uint end_char_idx = 0;
};
