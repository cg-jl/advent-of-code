#!/usr/bin/env python
from os.path import realpath
from pathlib import Path
from part1 import parse_policy, Policy, compose
import io

def check_policy(policy: Policy) -> bool:
    return (policy.i1 <= len(policy.s2) and policy.s2[policy.i1 - 1] == policy.s1) + \
           (policy.i2 <= len(policy.s2) and policy.s2[policy.i2 - 1] == policy.s1) \
               == 1

def solution(file_input: io.TextIOBase) -> int:
    return sum(map(compose(check_policy, parse_policy),
    (line.strip() for line in file_input)))


if __name__ == "__main__":
    with open(Path(realpath(__file__)).parent.parent / 'inputs/2.txt') as fh:
        print(solution(fh))