import io
from typing import Iterable, List


def parse_tree(text: io.TextIOBase) -> Iterable[List[bool]]:
    for line in text:
        if not line:
            continue

        yield [x == "#" for x in line.strip()]


def check_tree(x: int, line: List[bool]) -> bool:
    return line[x % len(line)]


def solution(text: io.TextIOBase) -> int:
    return sum(
        map(lambda t: check_tree(t[0] * 3, t[1]), enumerate(parse_tree(text)))
    )


if __name__ == "__main__":
    from os.path import realpath
    from pathlib import Path

    p = Path(realpath(__file__)).parent.parent / "inputs/3.txt"
    with open(p) as fh:
        print(solution(fh))