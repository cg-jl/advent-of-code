from typing import Iterable, List


def count_times_greater(iter):
    last = None
    count = 0
    for current in iter:
        if last is not None and last < current:
            count += 1
        last = current
    return count


def make_three_windows(items):
    for i in range(len(items) - 2):
        yield items[i], items[i + 1], items[i + 2]


def part1(contents):
    return count_times_greater(contents)


def part2(contents):
    return count_times_greater(map(sum, make_three_windows(contents)))


if __name__ == "__main__":
    with open("inputs/day1.txt") as file:
        contents = [*map(int, file)]

    print("Part 1: ")
    print(part1(contents))
    print("Part 2: ")
    print(part2(contents))
