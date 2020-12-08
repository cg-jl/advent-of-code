#!/usr/bin/env python
from os import path
from pathlib import Path
import io
from dataclasses import dataclass
import re
from typing import Tuple
from collections import namedtuple

def compose(f1, f2):
    def fn(*args, **kwargs):
        return f1(f2(*args, **kwargs))

    return fn

    


Policy = namedtuple("Policy", ["i1", "i2", "s1", "s2"])


def parse_policy(line: str) -> Policy:
    line = line.strip()
    i1, i2, s1, _, s2 = re.split(r"[: -]", line)
    return Policy(int(i1), int(i2), s1, s2)


def check_policy(policy: Policy) -> bool:
    return policy.i1 <= policy.s2.count(policy.s1) <= policy.i2


def solution(file_input: io.TextIOBase) -> int:
    return sum(map(compose(check_policy, parse_policy), (line.strip() for line in file_input)))


if __name__ == "__main__":
    p = Path(path.realpath(__file__)).parent.parent / "inputs/2.txt"
    with open(p) as fh:
        print(solution(fh))