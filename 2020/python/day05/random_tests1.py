import argparse
from part1 import parse_line
from test_utils import create_seat, assert_eq
from argparse import ArgumentParser
from random import randint


def main():
    parser = ArgumentParser()
    parser.add_argument(
        "-n",
        "--number-of-tests",
        type=int,
        help="Number of tests to run, default 100",
        default=100,
    )
    args = parser.parse_args()

    for _ in range(args.number_of_tests):
        n = randint(0, 0b1111111111)
        seat = create_seat(n)
        assert_eq(parse_line(seat), n)


if __name__ == "__main__":
    main()