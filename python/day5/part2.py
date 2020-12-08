import io
from part1 import parse_input
from typing import List


def find_missing(l: List[int]) -> int:
    for i in range(len(l) - 1):
        j = i + 1
        if l[j] - l[i] != 1:
            return l[i] + 1
    return -1  # failed


def solution(text: io.TextIOBase) -> int:
    all_ids = sorted([*parse_input(text)])
    return find_missing(all_ids)


def main():
    from os.path import realpath
    from pathlib import Path

    p = Path(realpath(__file__)).parent.parent / "inputs/5.txt"
    with open(p) as fh:
        print(solution(fh))


if __name__ == "__main__":
    main()