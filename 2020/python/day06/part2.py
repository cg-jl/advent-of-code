import io
import string
from collections import defaultdict
from typing import Iterable


def count(it: Iterable) -> int:
    c = 0
    for t in it:
        c += 1
    return c


def get_count(group: str) -> int:
    ppl = group.split("\n")
    d = defaultdict(lambda: 0)
    for c in filter(lambda x: x in string.ascii_lowercase, group):
        d[c] += 1

    return count(filter(lambda count: count == len(ppl), d.values()))


def get_groups(text: io.TextIOBase) -> Iterable[str]:
    return filter(len, map(str.strip, text.read().split("\n\n")))


def solution(text: io.TextIOBase) -> int:
    return sum(map(get_count, get_groups(text)))


if __name__ == "__main__":
    from os.path import realpath
    from pathlib import Path

    p = Path(realpath(__file__)).parent.parent / "inputs/6.txt"
    with open(p) as fh:
        print(solution(fh))
