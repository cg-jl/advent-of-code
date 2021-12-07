#include <cstring>
#include <fstream>
#include <iostream>
#include <numeric>
#include <string>
#include <string_view>
#include <vector>

typedef unsigned char byte;

auto read_input(std::string_view line) -> std::vector<std::size_t> {
    std::vector<std::size_t> amt(9);
    std::memset(amt.data(), 0, amt.size());
    for (std::size_t found = line.find(','); found != line.npos;
         found = line.find(',')) {
        amt[std::atoi(line.substr(0, found).data())]++;
        line = line.substr(found + 1);
    }
    if (!line.empty()) {
        amt[std::atoi(line.data())]++;
    }
    return amt;
}

auto print_world(const std::vector<std::size_t>& world) -> void {
    std::cout << "------------------\n";
    for (std::size_t i = 0; i < world.size(); ++i) {
        std::cout << i << ": " << world[i] << '\n';
    }
    std::cout << "------------------\n";
}

auto day(std::vector<std::size_t>& world) -> void {
    const auto timeouted = world[0];

    // process the ones that are already there
    for (std::size_t i = 1; i <= 8; i++) {
        world[i - 1] = world[i];
    }

    // add the next generation
    world[8] = timeouted;
    // reset the other timers
    world[6] += timeouted;
}

auto main() -> int {
    std::string line;
    {
        std::ifstream input("inputs/day6.txt");
        if (!std::getline(input, line)) {
            std::cerr << "Sorry, couldn't get the values\n";
            return 1;
        }
    }
    auto world = read_input(line);
    for (int i = 0; i < 256; ++i) {
        day(world);
    }
    std::cout << std::accumulate<decltype(world)::const_iterator, std::size_t>(
                     world.cbegin(), world.cend(), 0)
              << '\n';
    return 0;
}