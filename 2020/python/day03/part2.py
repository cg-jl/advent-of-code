import io
from part1 import parse_tree, List, check_tree
from functools import partial, reduce


def mul(iterable):
    return reduce(lambda a, b: a * b, iterable)

def count_slopes(slope_width: int, slope_height: int, tree: List[List[int]]) -> int:
    return sum(
        map(
            lambda t: check_tree(t[0] * slope_width, tree[t[1]]),
            enumerate(range(0, len(tree), slope_height)),
        )
    )


def solution(text: io.TextIOBase) -> int:
    tree = [x for x in parse_tree(text)]

    check = partial(count_slopes, tree=tree)
    return mul(map(lambda t: check(*t), ((1, 1), (3, 1), (5, 1), (7, 1), (1, 2))))


if __name__ == "__main__":
    from os.path import realpath
    from pathlib import Path

    p = Path(realpath(__file__)).parent.parent / "inputs/3.txt"
    with open(p) as fh:
        print(solution(fh))