
from part1 import solution as find_first_odd, parse_numbers, test_case
from typing import Iterable
import itertools

# O(n)
def solution(numbers: Iterable[int], preamble_size: int) -> int:
    # O(n)
    parsed: list[int] = list(numbers)

    # remove odd one out
    # O(n)
    odd_one_out: int = find_first_odd(iter(parsed), preamble_size)

    # if a + b == n then a < n && b < n
    # O(n)
    res: list[int] = [x for x in parsed if x < odd_one_out]


    # O(n)
    while len(res) >= 2:
        seq_len = 2
        # O(preamble_size) at most -> constant time
        # as preamble_size is just a parameter bc test case's
        # preamble is 5 and not 25
        seq_min = float('inf')
        seq_max = -1
        seq_sum = 0
        while seq_len <= preamble_size: 
            seq_sum += res[seq_len]
            if seq_sum == odd_one_out:
                return seq_min + seq_max

            if seq_sum > odd_one_out:
                break # if sum is longer, then there's no point in adding
                      # more stuff to it

            # do what min() and max() do
            # so I don't iterate mora times
            # than needed
            if res[seq_len] < seq_min:
                seq_min = res[seq_len]

            if res[seq_len] > seq_max:
                seq_max = res[seq_len]

            seq_len += 1

        res = res[1:]

    return -1

def test():
    sol = solution(parse_numbers(test_case), 5)
    assert sol == 62, f"Expected 62, got {sol}"


if __name__ == "__main__":
    with open('inputs/9.txt') as fh:
        print(solution(parse_numbers(fh.read()), 25))