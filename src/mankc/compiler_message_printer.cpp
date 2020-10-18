#include <vector>
#include <sstream>
#include <utility>

#include "parser/lexer.h"

#include "compiler_message_printer.h"

#define ANSI_FORMAT "\033["
#define ANSI_SEPERATOR ";"
#define ANSI_END "m"

#define ANSI_CODE_RESET "0"
#define ANSI_CODE_BOLD "1"
#define ANSI_CODE_RED "31"
#define ANSI_CODE_CYAN "35"

#define ANSI_RESET ANSI_FORMAT ANSI_CODE_RESET ANSI_END
#define ANSI_BOLD ANSI_FORMAT ANSI_CODE_BOLD ANSI_END
#define ANSI_RED ANSI_FORMAT ANSI_CODE_RED ANSI_END
#define ANSI_BOLD_COLOUR(COLOUR_CODE) ANSI_FORMAT ANSI_CODE_BOLD ANSI_SEPERATOR COLOUR_CODE ANSI_END
#define ANSI_BOLD_RED ANSI_BOLD_COLOUR(ANSI_CODE_RED)
#define ANSI_BOLD_CYAN ANSI_BOLD_COLOUR(ANSI_CODE_CYAN)

using ANSIColour = char const *;

static std::vector<std::string> split_lines(std::string_view source) {
  std::vector<std::string> lines;
  std::istringstream is{std::string(source)}; // :(
  while (!is.eof()) {
    std::string line;
    std::getline(is, line);
    lines.emplace_back(line);
  }
  return lines;
}

static void annotate_lines(std::vector<std::string>& lines, SourceLocation loc, ANSIColour colour) {
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
      line.insert(marker_start, colour);
      if (pointer_start <= marker_end) {
        error_pointer = std::string(pointer_start, ' ');
        error_pointer += colour;
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

// pair: printed name, message colour
static std::pair<char const* /*name*/, ANSIColour>
message_type_formatting_info(CompilerMessage::Type type)
{
  switch (type) {
    case CompilerMessage::ERROR:
      return {"error", ANSI_BOLD_RED};
    case CompilerMessage::WARNING:
      return {"warning", ANSI_BOLD_CYAN};
    default:
      return {"???", ANSI_BOLD};
  }
}

std::ostream& operator<< (std::ostream& os, CompilerMessage const & message) {
  auto [message_type_name, ansi_colour] = message_type_formatting_info(message.type);
  os << formatxx::format_string(ANSI_BOLD "{}:{}:{}: {}{}:" ANSI_RESET" {}\n",
      message.source_lexer ? message.source_lexer->input_source_name() : "<unknown>",
      message.location.start_line + 1, message.location.start_column + 1,
      ansi_colour, message_type_name, message.message);
  if (message.source_lexer) {
    auto source_code = message.source_lexer->extract_lines(message.location);
    auto lines = split_lines(source_code);
    annotate_lines(lines, message.location, ansi_colour);
    for (auto annotated_line: lines) {
      os << annotated_line;
    }
  }
  return os;
}
