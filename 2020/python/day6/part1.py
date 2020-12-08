import io
import re
import string


def get_count(group: str) -> int:
    return len(set(filter(lambda x: x in string.ascii_lowercase, group)))


def solution(text: str) -> int:
    return sum(map(get_count, filter(len, text.split("\n\n"))))


if __name__ == "__main__":
    from os.path import realpath
    from pathlib import Path

    p = Path(realpath(__file__)).parent.parent / "inputs/6.txt"
    with open(p) as fh:
        print(solution(fh.read()))
