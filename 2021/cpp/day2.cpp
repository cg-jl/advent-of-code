#include <fstream>
#include <iostream>
#include <stdexcept>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

enum class Action { Forward, Up, Down };

auto operator<<(std::ostream& stream, const Action& action) -> std::ostream& {
    switch (action) {
        case Action::Forward:
            return stream << "forward";
        case Action::Up:
            return stream << "up";
        case Action::Down:
            return stream << "down";

        default:
            break;
    }
}

auto parse_line(std::string_view line) -> std::pair<Action, int> {
    auto found = line.find(' ');
    auto a = line.substr(0, found);
    auto b = line.substr(found + 1);
    // debug
    Action action;

    switch (a[0]) {
        case 'f':
            action = Action::Forward;
            break;
        case 'u':
            action = Action::Up;
            break;
        case 'd':
            action = Action::Down;
            break;
        default:
            throw std::runtime_error("unreachable switch case");
            break;
    }

    return {action, std::atoi(b.data())};
}

// maybe do an input reader that uses iterators?
auto read_input() -> std::vector<std::pair<Action, int>> {
    std::vector<std::pair<Action, int>> values;

    std::ifstream stream("inputs/day2.txt");
    std::string line;
    while (std::getline(stream, line)) {
        values.push_back(parse_line(line));
    }

    return values;
}

template <typename Container>
requires std::input_iterator<typename Container::iterator>
auto simulate_part1(const Container& c) -> int {
    int x = 0;
    int y = 0;
    for (const auto& [action, value] : c) {
        switch (action) {
            case Action::Forward:
                x += value;
                break;
            case Action::Up:
                y -= value;
                break;
            case Action::Down:
                y += value;
                break;
        }
    }
    return x * y;
}

template <typename Container>
requires std::input_iterator<typename Container::iterator>
auto simulate_part2(const Container& c) -> int {
    int x = 0;
    int y = 0;
    int aim = 0;
    for (const auto& [action, value] : c) {
        switch (action) {
            case Action::Forward:
                x += value;
                y += value * aim;
                break;
            case Action::Up:
                aim -= value;
                break;
            case Action::Down:
                aim += value;
                break;
        }
    }
    return x * y;
}

auto main() -> int {
    const auto vec = read_input();
    std::cout << "Part 1: " << simulate_part1(vec) << '\n';
    std::cout << "Part 2: " << simulate_part2(vec) << '\n';

    return 0;
}