from collections import deque

def read_line(line):
    return [int(i) for i in line.strip()]

NEIGHBOR_DIRECTIONS = {(-1, -1), (-1, 0), (-1, 1),
                           (0, -1),           (0, 1),
                           (1, -1), (1, 0), (1, 1)}


with open("inputs/day9.txt") as file:
    contents = [*map(read_line, file)]

def low_points(contents):
    for i in range(len(contents)):
        for j in range(len(contents[i])):
            neighbor_count = 8
            lt_count = 0
    
            for di, dj in NEIGHBOR_DIRECTIONS:
                ii = i + di
                jj = j + dj
                if ii < 0 or ii == len(contents) or jj < 0 or jj == len(contents[i]):
                    neighbor_count -= 1
                    continue
                
                lt_count += contents[ii][jj] > contents[i][j]
            
    
            if lt_count == neighbor_count:
                yield contents[i][j], i, j
    

def exists(contents, i, j):
    return i >= 0 and i != len(contents) and j >= 0 and j != len(contents[i])


def basins(contents):
    visited = set()
    for _, i, j in low_points(contents):
        # start a flood fill to know the size
        size = 0
        queue = [(i, j)]
        while queue:
            i, j = queue.pop()
            if contents[i][j] == 9 or (i, j) in visited:
                continue
            visited.add((i, j))
            queue += set((i + di, j + dj) for di, dj in {(-1, 0), (1, 0), (0, -1), (0, 1)} if exists(contents, i + di, j + dj))
            size += 1

        yield size

# # Part 1
# print(sum(map(lambda t: t[0] + 1, low_points(contents))))

if __name__ == "__main__":
    sizes = [*basins(contents)]
    sizes.sort()
    a, b, c = sizes.pop(), sizes.pop(), sizes.pop()
    print(a * b * c)
