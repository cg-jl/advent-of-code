#!/usr/bin/env python
from os import path
from pathlib import Path
import io
from typing import List, Union

def solution(input: io.TextIOBase) -> int:
    got: List[int] = []
    for line in input:
        n = int(line.strip())
        for x in got:
            for y in got:
                if x + y + n == 2020:
                    return x * y * n

        got.append(n)

    return -1

if __name__ == "__main__":
    p = Path(path.realpath(__file__)).parent.parent
    with open(p / 'inputs/1.txt') as fh:
        print(solution(fh))