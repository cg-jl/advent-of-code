DIRS = {
    "down" : (1, 0),
    "forward" : (0, 1),
    "up": (-1, 0)
}

def parse_line(line: str):
    dir, n = line.split()
    dx, dy = DIRS[dir]
    n = int(n)
    return n * dx, n * dy

def sim_part2(iter):
    x, y, aim = 0, 0, 0
    for action, X in iter:
        if action == 0: # then it was a `forward`
            x += X
            y += X * aim
        else:
            aim += action
    return x * y


def vector_add(iter):
    x, y = 0, 0
    for dx, dy in iter:
        x += dx
        y += dy
    return x, y

def part1(input):
    x, y = vector_add(map(parse_line, input))
    return x * y

if __name__ == "__main__":
    with open("inputs/day2.txt") as input:
        input = [*input]
        print(f"Part 1: {part1(input)}")
        print(f"Part 2: {sim_part2(map(parse_line, input))}")
