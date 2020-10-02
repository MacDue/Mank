#include <vector>
#include <sstream>
#include <ostream>

#include "lexer.h"
#include "compiler_errors.h"

#define ANSI_FORMAT "\033["
#define ANSI_SEPERATOR ";"
#define ANSI_END "m"

#define ANSI_CODE_RESET "0"
#define ANSI_CODE_BOLD "1"
#define ANSI_CODE_RED "31"

#define ANSI_RESET ANSI_FORMAT ANSI_CODE_RESET ANSI_END
#define ANSI_BOLD ANSI_FORMAT ANSI_CODE_BOLD ANSI_END
#define ANSI_RED ANSI_FORMAT ANSI_CODE_RED ANSI_END
#define ANSI_BOLD_RED ANSI_FORMAT ANSI_CODE_BOLD ANSI_SEPERATOR ANSI_CODE_RED ANSI_END

std::vector<std::string> split_lines(std::string_view source) {
  std::vector<std::string> lines;
  std::istringstream is{std::string(source)}; // :(
  while (!is.eof()) {
    std::string line;
    std::getline(is, line);
    lines.emplace_back(line);
  }
  return lines;
}

void annotate_lines(std::vector<std::string>& lines, SourceLocation loc) {
  uint line_num = loc.start_line + 1;
  for (auto& line: lines) {
    bool at_start = line_num == loc.start_line + 1;
    uint marker_start = at_start ? loc.start_column : 0;
    uint marker_end = line_num == loc.end_line + 1 ? loc.end_column : line.length();
    std::string error_pointer;
    if (marker_end > marker_start) {
      uint pointer_start = marker_start;
      if (!at_start) {
        auto first_non_space = line.find_first_not_of(" \t");
        if (first_non_space != std::string::npos) {
          pointer_start = first_non_space;
        }
      }
      line.insert(marker_end, ANSI_RESET);
      line.insert(marker_start, ANSI_BOLD_RED);
      if (pointer_start <= marker_end) {
        error_pointer = std::string(pointer_start, ' ');
        error_pointer += ANSI_BOLD_RED;
        error_pointer += at_start ? '^' : '~';
        error_pointer += std::string(marker_end - pointer_start - 1, '~') + ANSI_RESET;
      }
    }
    line = formatxx::format_string(
      "{:5} | {}\n"
      "{:5} | {}\n",
      line_num, line,
      "", error_pointer);
    line_num += 1;
  }
}

std::ostream& operator<< (std::ostream& os, CompilerError const & error) {
  os << formatxx::format_string(
    ANSI_BOLD "{}:{}:{}: " ANSI_RED "error:" ANSI_RESET" {}\n",
      error.source_lexer ? error.source_lexer->input_source_name() : "<unknown>",
      error.location.start_line + 1, error.location.start_column + 1, error.what());
  if (error.source_lexer) {
    auto source_code = error.source_lexer->extract_lines(error.location);
    auto lines = split_lines(source_code);
    annotate_lines(lines, error.location);
    for (auto annotated_line: lines) {
      os << annotated_line;
    }
  }
  return os;
}
