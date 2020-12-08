import io
import re
from typing import Iterable
from functools import partial


def parse_group(group: str) -> dict:
    values = filter(len, re.split(r"\s+", group.strip()))
    return {k: v for k, v in map(partial(str.split, sep=":", maxsplit=2), values)}


def get_groups(text: io.TextIOBase) -> Iterable[dict]:
    current_group = ""
    for line in text:
        if not line.strip():
            if current_group:
                yield parse_group(current_group)
            current_group = ""
            continue

        current_group += line

    if current_group:
        yield parse_group(current_group)


def check_group(group: dict) -> bool:
    return all(k in group for k in ("ecl", "eyr", "hcl", "hgt", "byr", "iyr", "pid"))

def solution(text: io.TextIOBase) -> int:
    return sum(map(check_group, get_groups(text)))


if __name__ == "__main__":
    from os.path import realpath
    from pathlib import Path

    p = Path(realpath(__file__)).parent.parent / "inputs/4.txt"
    with open(p) as fh:
        print(solution(fh))