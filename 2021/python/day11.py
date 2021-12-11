def read_file(filename):
    contents = []
    with open(filename, "r") as handle:
        for line in handle:
            contents += map(int, line.strip())

    return contents


def step(matrix):
    flashed = [False] * 100

    # we start by increasing all of the octopuses' energy levels
    # ("first, all of the octopuses increase their energy levels by 1")
    to_increase = [*range(0, 100)]

    while to_increase:
        index = to_increase.pop()
        matrix[index] += 1

        # "an octopus can only flash at most once per step"
        if flashed[index]:
            continue

        if matrix[index] > 9:
            flashed[index] = True
            # propagate through its adjacent octopuses
            # i = row * 10 + col
            # => row = i // 10
            # => col = i % 10
            row = index // 10
            col = index % 10
            row_offset = row * 10

            # top row
            if row != 0:
                if col != 0:
                    to_increase.append(row_offset - 10 + col - 1)
                to_increase.append(row_offset - 10 + col)
                if col != 9:
                    to_increase.append(row_offset - 10 + col + 1)

            # middle row
            if col != 0:
                to_increase.append(row_offset + col - 1)
            if col != 9:
                to_increase.append(row_offset + col + 1)

            # bottom row
            if row != 9:
                if col != 0:
                    to_increase.append(row_offset + 10 + col - 1)
                to_increase.append(row_offset + 10 + col)
                if col != 9:
                    to_increase.append(row_offset + 10 + col + 1)

    # put the ones that flashed to 0
    # and count #flashed
    flashed_count = 0

    for i, has_flashed in enumerate(flashed):
        if has_flashed:
            matrix[i] = 0
            flashed_count += 1

    return flashed_count


if __name__ == "__main__":
    import sys

    if len(sys.argv) != 2:
        print(f"usage: {sys.argv[0]} <file>", file=sys.stderr)
        exit(1)

    contents = read_file(sys.argv[1])
    i = 0
    total_flashed = 0
    while i < 100:
        total_flashed += step(contents)
        i += 1

    print(f"{total_flashed} octopuses flashed in 100 steps")

    while step(contents) != 100:
        i += 1

    print(f"all flashed after {i + 1} steps")
