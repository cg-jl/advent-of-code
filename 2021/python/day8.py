def get_digit(one: set[int], four: set[int], unknown: set[int]) -> int:
    match len(unknown):
        case 2:
            return 1
        case 3:
            return 7
        case 4:
            return 4
        case 7:
            return 8
        case 5:  # candidates: 2, 3, 5
            # 3 is the only one that has both segments in common with 1
            if len(unknown.intersection(one)) == 2:
                return 3
            # 2 is the only one that has two segments in common with 4
            elif len(unknown.intersection(four)) == 2:
                return 2
            # 5 coincides with 3 in 1 and with 2 on 4 so left to elimination
            else:
                return 5

        case 6:  # candidates: 6, 9, 0
            # 6 is the only one that has just 1 segment in common with 1; rest has 2
            if len(unknown.intersection(one)) == 1:
                return 6
            # 9 is the only one that contains four, the rest has 3 in common with it.
            elif len(unknown.intersection(four)) == 4:
                return 9
            # 0 is like 5, it is not "the only one" in any of the upper cases,  so it is left to elimination
            else:
                return 0


def decode(one: set[int], four: set[int], target: list[set[int]]) -> int:
    result = 0
    for unknown in target:
        digit = get_digit(one, four, unknown)
        result = result * 10 + digit

    return result


def decode_line(codes: list[set[int]], target: list[set[int]]) -> int:
    one, four = find_one_and_four(codes)
    return decode(one, four, target)


def parse_line(l: str):
    l, _, r = l.partition(" | ")
    return [*map(set, l.split())], [*map(set, r.split())]


def find_one_and_four(l: list[set[int]]) -> tuple[set[int], set[int]]:
    one = [*filter(lambda x: len(x) == 2, l)][0]
    four = [*filter(lambda x: len(x) == 4, l)][0]
    return one, four


with open("inputs/day8.txt") as file:
    lines = [*map(parse_line, file)]

    part_one_count = 0
    part_two_sum = 0
    for codes, target in lines:
        for value in target:
            if len(value) in [2, 3, 4, 7]:
                part_one_count += 1

        part_two_sum += decode_line(codes, target)

    print(f"Part 1: {part_one_count}")
    print(f"Part 2: {part_two_sum}")
