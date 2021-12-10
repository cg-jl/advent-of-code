#include <cstring>
#include <iostream>
#include <stdexcept>
#include <string_view>
#include <algorithm>
#include <vector>

#include "util.h"
//#include <unordered_map>
#include <optional>

// parser is ID
auto read_line(std::string_view line) -> std::string_view { return line; }

constexpr auto counterpart(char value) -> char {
    switch (value) {
        case ']':
            return '[';
        case ')':
            return '(';
        case '}':
            return '{';
        case '>':
            return '<';
        case '[':
            return ']';
        case '(':
            return ')';
        case '{':
            return '}';
        case '<':
            return '>';
        default:
            throw "unreachable";
    }
}

auto check_line(std::string_view line) -> std::optional<char> {
    std::vector<char> missing;

    for (const auto& ch : line) {
        switch (ch) {
            case '[':
            case '(':
            case '{':
            case '<':
                missing.push_back(ch);
                break;
            case ']':
            case ')':
            case '}':
            case '>':
                if (missing.size() == 0 || missing.back() != counterpart(ch))
                    return ch;
                missing.pop_back();
                break;
        }
    }
    return {};
}

constexpr inline auto get_points(char value) -> std::size_t {
    switch (value) {
        case ')':
            return 3;
        case ']':
            return 57;
        case '}':
            return 1197;
        case '>':
            return 25137;
    }
}

auto autocomplete(std::string_view line) -> std::string {
    std::string unclosed;

    for (const auto& ch : line) {
        switch (ch) {
            case '[':
            case '{':
            case '<':
            case '(':
                unclosed.push_back(counterpart(ch));
                break;
            // we know that the characters are valid
            default:
                unclosed.pop_back();
                break;
        }
    }

    std::reverse(unclosed.begin(), unclosed.end());

    return unclosed;
}

constexpr auto autocomplete_points(std::string_view autocompleted)
    -> std::size_t {
    std::size_t points = 0;
    for (const char& c : autocompleted) {
        points *= 5;
        switch (c) {
            case ')':
                points += 1;
                break;
            case ']':
                points += 2;
                break;
            case '}':
                points += 3;
                break;
            case '>':
                points += 4;
                break;
            default:
                break;
        }
    }
    return points;
}

auto main(int argc, char* const argv[]) -> int {
    if (argc != 2) {
        std::cerr << "usage: " << *argv << " <file>\n";
        return 1;
    }
    util::InputReader<std::string_view> reader(
        std::string_view(argv[1], std::strlen(argv[1])), read_line);

    // part 1
    // std::size_t points = 0;

    // for (const auto& line : reader) {
    //     const auto maybe_bad_char = check_line(line);
    //     if (!maybe_bad_char) continue;
    //     const auto bad_char = *maybe_bad_char;
    //     points += get_points(bad_char);
    // }

    // std::cout << "score: " << points << "\n";

    std::vector<std::size_t> points_list;
    for (const auto& line : reader) {
        if (check_line(line)) continue;  // discard corrupted line
        const auto ac = autocomplete(line);
        points_list.push_back(autocomplete_points(ac));
    }

    std::sort(points_list.begin(), points_list.end());

    std::cout << "score: " << points_list[points_list.size() / 2] << '\n';


    return 0;
}