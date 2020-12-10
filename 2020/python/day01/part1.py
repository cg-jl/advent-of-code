#!/usr/bin/env python
from os import path
from pathlib import Path
import io

def solution(file: io.TextIOWrapper) -> int:
    got = []
    for line in file:
        n = int(line.strip())
        for x in got:
            if x + n == 2020:
                return x * n

        got.append(n)

    return -1
                

if __name__ == "__main__":
    p = Path(path.realpath(__file__)).parent.parent
    with open(p / "inputs/1.txt") as fh:
        print(solution(fh))