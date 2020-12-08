#pragma once

struct SourceLocation {
  uint start_line = 0;
  uint start_column = 0;
  uint end_line = 0;
  uint end_column = 0;
  uint start_char_idx = 0;
  uint end_char_idx = 0;

  inline bool is_empty() {
    return end_char_idx - start_char_idx <= 0;
  }

  inline bool operator==(SourceLocation const & other) {
    return start_char_idx == other.start_char_idx
      && end_char_idx == other.end_char_idx;
  }
};
