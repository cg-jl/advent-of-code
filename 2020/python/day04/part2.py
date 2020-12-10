import io
from part1 import parse_group, get_groups, check_group as check_group_part1
from functools import partial
import string

def check_year(year: str, min_threshold: int, max_threshold: int) -> bool:
    return len(year) == 4 and min_threshold <= int(year) <= max_threshold

def check_hair_color(color: str) -> bool:
    # colors that start with '#' are always right
    return len(color) == 7 and color[0] == '#' and all(x in string.hexdigits for x in color[1:])


def check_pid(pid: str) -> bool:
    return len(pid) == 9 and all(x in string.digits for x in pid)

def check_height(height: str) -> bool:
    measure = height[-2:]
    return measure in { 'cm', 'in'} and {
        'cm': lambda x: 150 <= x <= 193,
        'in': lambda x: 59  <= x <=  76,
    }[measure](int(height[:-2]))


def check_eye_color(color: str) -> bool:
    return color in { 'amb', 'blu', 'brn', 'gry', 'grn', 'hzl', 'oth'}


def check_keyvalue(key: str, value: str) -> bool:
    parse_dict = {
        'byr': partial(check_year, min_threshold=1920, max_threshold=2002),
        'iyr': partial(check_year, min_threshold=2010, max_threshold=2020),
        'eyr': partial(check_year, min_threshold=2020, max_threshold=2030),
        'hgt': check_height,
        'hcl': check_hair_color,
        'pid': check_pid,
        'ecl': check_eye_color,
        'cid': lambda x: True
    }
    return parse_dict[key](value)

def check_group(group: dict) -> bool:
    return check_group_part1(group) and all(map(lambda x: check_keyvalue(*x), group.items()))

def solution(text: io.TextIOBase) -> int:
    return sum(map(check_group, get_groups(text)))


if __name__ == "__main__":
    from os.path import realpath
    from pathlib import Path
    p = Path(realpath(__file__)).parent.parent / 'inputs/4.txt'
    with open(p) as fh:
        print(solution(fh))