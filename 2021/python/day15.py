import sys
from collections import defaultdict
import heapq

def read_input(file):
    with open(file) as handle:
        contents = []
        for line in handle:
            contents.append([int(x) for x in line.strip()])

    return contents

def lowest_risk_path(maze):
    # maze represents the risk of entering
    # we want to minimize the function sum(path)
    end = (len(maze) - 1, len(maze) - 1)
    visited = set()
    h = []
    dist = defaultdict(lambda: sys.maxsize)
    dist[0, 0 ] = 0
    heapq.heappush(h, (0, (0, 0)))

    prev = {}


    while h:
        _, u = heapq.heappop(h)
        y, x = u
        visited.add(u)
        for dy, dx in ((-1, 0), (0, -1), (1, 0), (0, 1)):
            ny = y + dy
            nx = x + dx
            n = ny, nx
            if ny < 0 or nx < 0 or ny == len(maze) or nx == len(maze) or n in visited:
                continue

            alt = dist[u] + maze[n]
            if alt < dist[n]:
                heapq.heappush(h, (alt, n))
                dist[n] = alt
                prev[n] = u

    return dist[end]


# reconstruct path in reverse
def reconstruct_path(came_from, current):
    yield current
    while current in came_from:
        current = came_from[current]
        yield current

def print_maze(maze):
    for i in range(len(maze)):
        if i > 0: print()
        for j in range(len(maze)):
            print(maze[i, j], end='')
    # for i, row in enumerate(maze):
    #     if i > 0: print()
    #     for j, value in enumerate(row):
    #         if j > 0: print(end=' ')
    #         if (i, j) in path:
    #             print(f"\x1b[38;5;255m{value}", end='')
    #         else:
    #             print(f"\x1b[38;5;237m{value}", end='')
    # print("\x1b[m")

class Maze:
    def __init__(self, maze):
        self.maze = maze

    def __len__(self):
        return len(self.maze)

    def __getitem__(self, pos):
        y, x = pos
        return self.maze[y][x]


class AugmentedMaze(Maze):
    def __init__(self, maze):
        super(AugmentedMaze, self).__init__(maze)

    # different operator[] to expand the size without needing more memory
    def __getitem__(self, pos):
        y, x = pos
        # first, we want to know the block position
        block_y = y // len(self.maze)
        block_x = x // len(self.maze)
        # now we want the coordinates inside the block
        inside_y = y % len(self.maze)
        inside_x = x % len(self.maze)

        # this is the value we want to convert
        maze_value = self.maze[inside_y][inside_x]

        # the idea is to add both shifts to the value, subtract 1, mod by 9 and add 1
        # (the adds and substracts are to convert first a range from 1-9 to 0-8 so I can mod 9 correctly and then convert
        # it back to 1-9 range)
        return ((maze_value + block_y + block_x - 1) % 9) + 1





    def __len__(self):
        return len(self.maze) * 5 # five times larger





if __name__ == "__main__":
    inp = read_input("inputs/day15.txt")
    shortest = lowest_risk_path(Maze(inp))
    print(f"Part 1: {shortest}")
    #$print_maze(AugmentedMaze(inp))
    shortest2 = lowest_risk_path(AugmentedMaze(inp))
    print(f"Part 2: {shortest2}")
