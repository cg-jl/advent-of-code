import io
from typing import Iterable, Set
from collections import namedtuple
from functools import reduce

# binary search problem
# 10 bits, MSB
# bits 0-6 indicate row in binary.
# bits 7-9 indicate column (0-7)
# seat_id = row * 8 + col

Seat = int


def parse_bin(string: str, what_is_one: str):
    return reduce(lambda x, y: (x << 1) | (y == what_is_one), string, 0)


def parse_line(line: str) -> Seat:
    return parse_bin(line[:7], "B") << 3 | parse_bin(line[7:10], "R")


def parse_input(text: io.TextIOBase) -> Iterable[Seat]:
    for line in text:
        yield parse_line(line.strip())


def solution(text: io.TextIOBase) -> int:
    return max(parse_input(text))


def main():
    from os.path import realpath
    from pathlib import Path

    p = Path(realpath(__file__)).parent.parent / "inputs/5.txt"
    with open(p) as fh:
        print(solution(fh))


if __name__ == "__main__":
    main()