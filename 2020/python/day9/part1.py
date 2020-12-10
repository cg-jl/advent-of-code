from typing import Iterable
import itertools
from collections import deque

"""
example: 
35
20
15
25
47
40 -> must be a sum of previous 5
62
55
65
95
102
117
150
182
127
219
299
277
309
576
"""

def parse_numbers(in_text: str) -> Iterable[int]:
    for line in in_text.splitlines():
        line = line.strip()
        if not line:
            continue

        yield int(line)
    
def solution(numbers: Iterable[int], preamble_size: int) -> int:
    preamble = deque(itertools.islice(numbers, preamble_size))

    for n in numbers:
        combs = itertools.combinations(preamble, 2)
        if not any(sum(comb) == n for comb in combs):
            return n
        # rotate preamble
        preamble.popleft()
        preamble.append(n)



test_case = """
35
20
15
25
47
40
62
55
65
95
102
117
150
182
127
219
299
277
309
576
    """

def test():
    assert solution(parse_numbers(test_case), 5) == 127

if __name__ == "__main__":
    with open('inputs/9.txt') as fh:
        print(solution(parse_numbers(fh.read()), 25))