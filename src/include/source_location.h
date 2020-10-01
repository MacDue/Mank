#pragma once

struct SourceLocation {
  uint start_line;
  uint start_column;
  uint end_line;
  uint end_column;
  uint start_char_idx;
  uint end_char_idx;
};
