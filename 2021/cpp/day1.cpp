#include <iostream>
#include <limits>
#include <iterator>
#include <fstream>
#include <vector>
#include <string>

using usize = uint32_t;

auto get_data() -> std::vector<int> {
    std::vector<int> result;
    std::ifstream stream("inputs/day1.txt");
    std::string line;
    while (std::getline(stream, line)) {
        result.push_back(std::stoi(line));
    }
    return result;
    
}

auto print_data(const std::vector<int>& data) -> void {
    for (const auto& d : data) {
        std::cout << d << "\n";
    }
}


template<typename Container>  
    requires std::input_iterator<typename Container::const_iterator>
auto count_greater(const Container& data) -> usize {
    int last = std::numeric_limits<int>::max();
    usize count = 0;

    for (const auto& elem : data) {
        if (last < elem) {
            count++;
        }
        last = elem;
    }
    return count;
}

template<typename InContainer, typename OutContainer>
    requires std::input_iterator<typename InContainer::const_iterator> && std::output_iterator<typename OutContainer::iterator, int>
auto sum_triplets(const InContainer& input, OutContainer& out) -> void {

    const auto read_end = input.cend();
    const auto write_end = out.end();
    auto current_read1 = input.cbegin();
    auto current_read2 = input.cbegin(); ++current_read2;
    auto current_read3 = input.cbegin(); ++current_read3; ++current_read3;
    auto current_write = out.begin();

    for (; current_write != write_end && current_read3 != read_end;) {
        *current_write++ = *current_read1++ + *current_read2++ + *current_read3++;
    }
}


auto main() -> int {
    auto data = get_data();
    std::cout << "Part 1: \n" << count_greater(data) << '\n';
    sum_triplets(data, data);
    data[data.size() - 2] = data[data.size() - 1] = 0;
    std::cout << "Part 2: \n" << count_greater(data) << '\n';




    return 0;
}