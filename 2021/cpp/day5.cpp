#include <cstring>
#include <iostream>
#include <memory>
#include <stdexcept>
#include <string>
#include <string_view>
#include <vector>

#define UTIL_INPUT_READER
#include "util.h"

struct Point {
    int x, y;
};

struct Line {
    Point start, end;

    constexpr auto is_vertical() const -> bool { return start.x == end.x; }
    constexpr auto is_horizontal() const -> bool { return start.y == end.y; }
};

auto operator<<(std::ostream& stream, const Point& p) -> std::ostream& {
    return stream << p.x << ',' << p.y;
}

auto operator<<(std::ostream& stream, const Line& line) -> std::ostream& {
    return stream << line.start << " -> " << line.end;
}

auto parse_point(std::string_view source) -> Point {
    auto found = source.find(',');
    return {std::atoi(source.substr(0, found).data()),
            std::atoi(source.substr(found + 1).data())};
}

auto parse_line(std::string_view source) -> Line {
    auto found = source.find("->");
    return {parse_point(source.substr(0, found)),
            parse_point(source.substr(found + 3))};
}

auto parse_input(std::string_view filename) -> util::InputReader<Line> {
    return util::InputReader<Line>(filename, parse_line);
}

#define MAT_W 1000

auto plot_line(int* matrix, const Line& line) -> void {
    if (line.is_vertical()) {
        const std::size_t col = line.start.x;
        const std::size_t start = std::min(line.start.y, line.end.y);
        const std::size_t end = std::max(line.start.y, line.end.y);
        for (std::size_t row = start; row <= end; ++row) {
            matrix[row * MAT_W + col]++;
        }
    } else if (line.is_horizontal()) {
        const std::size_t row = line.start.y;
        const std::size_t start = std::min(line.start.x, line.end.x);
        const std::size_t end = std::max(line.start.x, line.end.x);
        for (std::size_t col = start; col <= end; ++col) {
            matrix[row * MAT_W + col]++;
        }
    } else {
        // let's see how the slope is:
        // slope is always going to be 1 or -1 (45 degree)
        const int m = (line.end.y - line.start.y) / (line.end.x - line.start.x);
        // now the N value:
        const int n = line.end.y - line.end.x * m;

        const std::size_t col_start = std::min(line.end.x, line.start.x);
        const std::size_t col_end = std::max(line.end.x, line.start.x);

        for (std::size_t col = col_start; col <= col_end; ++col) {
            auto row = int(col) * m + n;
            matrix[row * MAT_W + col]++;
        }
    }
}

auto main() -> int {
    auto matrix = std::make_unique<int[]>(MAT_W * MAT_W);
    std::memset(matrix.get(), 0, MAT_W * MAT_W * sizeof(int));
    auto reader = parse_input("inputs/day5.txt");

    for (const auto& line : reader) {
        plot_line(matrix.get(), line);
    }

    std::size_t count = 0;
    for (std::size_t i = 0; i < MAT_W * MAT_W; ++i) {
        count += matrix[i] >= 2;
    }
    std::cout << count << '\n';

    return 0;
}