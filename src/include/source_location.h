#pragma once

struct SourceLocation {
  int start_line;
  int start_column;
  int end_line;
  int end_column;
  int start_char_idx;
  int end_char_idx;
};
