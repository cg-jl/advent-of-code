# /usr/bin/python3.9
def parse_input(inp: str) -> tuple[int, list[int]]:
    k, vs = inp.split("\n")
    k = int(k)
    vs = [int(x) for x in vs.split(",") if x != "x"]
    return k, vs


def solution(inp: str) -> int:
    n, ns = parse_input(inp)
    f = min(n % x for x in ns)
    g = [x for x in ns if n % x == f][0]
    s = (f + 1) * g - n
    print(f"{f, g, s = }")
    return s * g


if __name__ == "__main__":
    with open("/home/gsus/.config/aoc_helper/2020/13.in") as fh:
        print(solution(fh.read()))
