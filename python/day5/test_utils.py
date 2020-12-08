from functools import reduce


def create_seat(n: int) -> str:
    upper = n >> 3
    lower = n & 7

    u = reduce(
        lambda a, b: ("B" if b else "F") + a,
        map(lambda x: (upper >> x) & 1, range(7)),
        "",
    )

    l = reduce(
        lambda a, b: a + ("R" if b else "L"),
        map(lambda x: (lower >> x) & 1, range(3)),
        "",
    )

    return u + l


def assert_eq(a, b):
    try:
        assert a == b
    except AssertionError:
        print("Expected {}, found {}".format(b, a))
        exit(1)