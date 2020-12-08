from part1 import parse_line, Seat
from test_utils import assert_eq


assert_eq(parse_line("FBFBBFFRLR"), 0b0101100101)
assert_eq(parse_line("BFFFBBFRRR"), 0b1000110111)
assert_eq(parse_line("FFFBBBFRRR"), 0b0001110111)
assert_eq(parse_line("BBFFBBFRLL"), 0b1100110100)
