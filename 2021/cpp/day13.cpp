#include <bitset>
#include <cstring>
#include <fstream>
#include <functional>
#include <iostream>
#include <memory>
#include <numeric>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#ifndef REAL_DATA
#define INPUT_FILE "sample.txt"
#define MAX_WIDTH 11
#define MAX_HEIGHT 15
#else
#define INPUT_FILE "inputs/day13.txt"
#define MAX_WIDTH 1311
#define MAX_HEIGHT 895
#endif

constexpr auto partial_parse_int(std::string_view source, std::size_t &target)
    -> std::string_view {
  std::size_t value = 0;
  for (; !source.empty() && std::isdigit(source[0]);
       source = source.substr(1)) {
    value = value * 10 + (source[0] - '0');
  }
  target = value;
  return source;
}

// NOTE: could be done better with a bitset

using point = std::pair<std::size_t, std::size_t>;

auto parse_point(std::string_view line) -> point {
  const auto comma_index = line.find(',');
  std::size_t x, y;
  partial_parse_int(line.substr(0, comma_index), x);
  partial_parse_int(line.substr(comma_index + 1), y);
  return {x, y};
}

enum class FoldAxis : char { X = 'y', Y = 'x' };
using fold = std::pair<FoldAxis, std::size_t>;

auto parse_fold(std::string_view line) -> fold {
  line = line.substr(11);
  const FoldAxis axis = (FoldAxis)line[0];
  std::size_t value;
  partial_parse_int(line.substr(2), value);
  return {axis, value};
}

using bitset = std::array<bool, MAX_WIDTH * MAX_HEIGHT>;

auto parse_input() -> std::pair<bitset, std::vector<fold>> {
  std::ifstream input(INPUT_FILE);
  std::vector<fold> folds;
  bitset points{false};
  std::string line_buf;
  bool in_folds = false;
  while (std::getline(input, line_buf)) {
    if (line_buf == "") {
      in_folds = true;
      continue;
    }
    const auto line = std::string_view(line_buf);
    if (in_folds) {
      const auto [axis, value] = parse_fold(line);
      folds.emplace_back(axis, value);
    } else {
      const auto [x, y] = parse_point(line);
      points[y * MAX_WIDTH + x] = true;
    }
  }
  return {points, folds};
}

auto operator<<(std::ostream &stream, const bitset &set) -> std::ostream & {
  for (std::size_t row = 0; row < MAX_HEIGHT; ++row) {
    if (row > 0)
      stream << '\n';
    const auto row_offset = row * MAX_WIDTH;
    for (std::size_t col = 0; col < MAX_WIDTH; ++col) {
      stream << (set[row_offset + col] ? '#' : '.');
    }
  }
  return stream;
}

auto process_fold(bitset &set, FoldAxis axis, std::size_t axis_position,
                  std::size_t &final_width, std::size_t &final_height) -> void {
  switch (axis) {
  case FoldAxis::X: {
    // to fold according to the X axis, we want:
    //  for each of the rows, OR the set with the current value
    for (std::size_t row = axis_position; row < MAX_HEIGHT; ++row) {
      // we want to mirror the values up
      const auto dist_to_target = row - axis_position;
      const auto target_row = row - 2 * dist_to_target;
      if (target_row >= MAX_HEIGHT)
        continue;
      for (std::size_t col = 0; col < MAX_WIDTH; ++col) {
        const auto current_index = row * MAX_WIDTH + col;
        const auto target_index = target_row * MAX_WIDTH + col;
        set[target_index] |= set[current_index];
        set[current_index] = false;
      }
    }
    final_height = axis_position;
  } break;
  case FoldAxis::Y: {

    // to fold according to the Y axis, we want kinda the same thing but using
    // columns
    for (std::size_t row = 0; row < MAX_HEIGHT; ++row) {
      const auto row_offset = row * MAX_WIDTH;
      for (std::size_t col = axis_position; col < MAX_WIDTH; ++col) {
        const auto dist_to_axis = col - axis_position;
        const auto target_col = col - 2 * dist_to_axis;
        if (target_col >= MAX_WIDTH)
          continue;
        const auto current_index = row_offset + col;
        const auto target_index = row_offset + target_col;
        set[target_index] |= set[current_index];
        set[current_index] = false;
      }
    }
    final_width = axis_position;
  } break;
  }
}

auto count_bits(const bitset &set) -> std::size_t {
  return std::accumulate(set.cbegin(), set.cend(), 0,
                         [](std::size_t a, bool b) { return b ? a + 1 : a; });
}

auto print_code(const bitset &code, std::size_t final_width,
                std::size_t final_height) {
  for (std::size_t row = 0; row < final_height; ++row) {
    if (row > 0)
      std::cout << '\n';
    const auto row_offset = row * MAX_WIDTH;
    for (std::size_t col = 0; col < final_width; ++col) {
      std::cout << (code[row_offset + col] ? "\x1b[48;5;255m" : "\x1b[0m")
                << ' ';
    }
  }
  std::cout << "\x1b[m";
}

auto main() -> int {
  auto [bitset, folds] = parse_input();

  std::size_t final_width = MAX_WIDTH;
  std::size_t final_height = MAX_HEIGHT;

  process_fold(bitset, folds[0].first, folds[0].second, final_width,
               final_height);
  std::cout << "Part 1: " << count_bits(bitset) << '\n';
  // complete the rest of folds
  for (std::size_t i = 1; i < folds.size(); ++i) {
    process_fold(bitset, folds[i].first, folds[i].second, final_width,
                 final_height);
  }
  print_code(bitset, final_width, final_height);
  std::cout << '\n';

  return 0;
}
