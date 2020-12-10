#!/usr/bin/python3.9
from typing import Iterable
from collections import namedtuple, deque


def parse_input(text: str) -> set[int]:
    s: set[int] = set()
    for line in text.splitlines():
        line = line.strip()
        if not line:
            continue

        s.add(int(line))

    return s


Graph = dict[int, deque]

def make_graph(numbers: set[int]) -> Graph:
    g: Graph = {}
    g[0] = deque(i for i in range(1, 3 + 1) if i in numbers)
    for n in numbers:
        g[n] = deque(i for i in range(n + 1, n + 3 + 1) if i in numbers)

    g[n] = deque({n + 3})
    return g


def count_jolt_differences(graph: Graph) -> tuple[int, int, int]:
    current = 0
    counts = [0, 0, 0]
    while current in graph:
        next_item = graph[current].popleft()
        counts[next_item - current - 1] += 1
        current = next_item

    return tuple(counts)


def solution(text: str) -> int:
    graph = make_graph(parse_input(text))
    count1, _, count3 = count_jolt_differences(graph)
    return count1 * count3


test_input = """
28
33
18
42
31
14
46
20
48
47
24
23
49
45
19
38
39
11
1
32
25
35
8
17
7
9
4
2
34
10
3
"""

def test():
    assert solution(test_input) == 22 * 10


if __name__ == "__main__":
    with open('inputs/10.txt') as fh:
        print(solution(fh.read()))