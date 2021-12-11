#include <array>
#include <cstring>
#include <fstream>
#include <iostream>
#include <memory>
#include <string_view>
#include <utility>
#include <vector>

using byte = unsigned char;

auto read_file(std::string_view filename) -> std::unique_ptr<byte[]> {
    auto matrix = std::make_unique<byte[]>(10 * 10);
    std::ifstream input(filename.data());

    for (std::size_t i = 0; i < 10; ++i) {
        if (i > 0) input.get();  // skip the last newline
        for (std::size_t j = 0; j < 10; ++j) {
            input >> matrix[i * 10 + j];
        }
    }
    return matrix;
}

auto print_matrix(const byte *matrix) -> void {
    for (std::size_t i = 0; i < 10; ++i) {
        for (std::size_t j = 0; j < 10; ++j) {
            std::cout << matrix[i * 10 + j];
        }
        std::cout << '\n';
    }
}

auto step(byte *matrix) -> std::size_t {
    // we want to have a stack of the octopuses that we want to consider,
    static std::vector<std::size_t> to_consider;
    to_consider.clear();
    // "an octopus can only flash at most once per step"
    static bool already_flashed[100];
    std::memset(already_flashed, 0, sizeof(already_flashed));

    // first, the energy level of each octopus increases by 1.
    for (std::size_t i = 0; i < 100; ++i) {
        to_consider.push_back(i);
    }

    while (!to_consider.empty()) {
        const auto index = to_consider.back();
        to_consider.pop_back();
        if (already_flashed[index]) continue;
        // 'considering' means increasing its enery level.
        matrix[index]++;
        if (matrix[index] > '9') {
            // flash
            already_flashed[index] = true;
            // propagate the signal through all 8 adjacent
            // i = row * width + col;
            // => row = i // width
            // => col = i - row * width
            const auto row = index / 10;
            const auto col = index - row * 10;


            const auto row_offset = row * 10;

            // propagate to the row above
            if (row != 0) {
                if (col != 0) to_consider.push_back(row_offset - 10 + col - 1);
                to_consider.push_back(row_offset - 10 + col);
                if (col != 9) to_consider.push_back(row_offset - 10 + col + 1);
            }

            // propagate to the row it's in.
            {
                if (col != 0) to_consider.push_back(row_offset + col - 1);
                if (col != 9) to_consider.push_back(row_offset + col + 1);
            }

            // propagate to the row below
            if (row != 9) {
                if (col != 0) to_consider.push_back(row_offset + 10 + col - 1);
                to_consider.push_back(row_offset + 10 + col);
                if (col != 9) to_consider.push_back(row_offset + 10 + col + 1);
            }
        }
    }
    
    // finally, any octopus that flashed during this step has its energy level
    // set to 0, as it used all of its energy to flash.
    std::size_t flashed_count = 0;
    for (std::size_t i = 0; i < 100; ++i) {
        if (already_flashed[i]) {
            matrix[i] = '0';
            flashed_count++;
        } 

    }
    return flashed_count;
}


auto main(int argc, char *const argv[]) -> int {
    if (argc != 2) {
        std::fprintf(stderr, "usage: %s <file>\n", *argv);
        std::exit(0);
    }


    auto matrix =
        read_file(std::string_view(argv[1], std::strlen(argv[1])));


    // part 1
    // std::size_t total_flashes = 0;

    // for (std::size_t i = 0; i < 100; ++i) {
    //     total_flashes += step(matrix.get());
    // }

    // std::cout << "total flashes: " << total_flashes << '\n';

    for (std::size_t i = 0; true; ++i) {
        if (step(matrix.get()) == 100) {
            std::cout << "simultaneous flash at: " << (i + 1) << '\n';
            break;
        }
    }


    return 0;
}