#!/usr/bin/env python3
import itertools
import numpy as np


with open('input.test') as handle:
    lines = handle.readlines()

lines =  np.matrix([*map(lambda l: list(map(lambda x: ord(x) - 48,l.strip())), lines)], dtype=np.uint8)


def propagate_shadows(diri, dirk, mat, counts):
    blocked_bys = np.zeros(shape=np.shape(mat), dtype=np.uint8)
    ri = [*range(1, len(mat) - 1)]
    rk = [*ri]
    if diri < 0: ri.reverse()
    if dirk < 0: rk.reverse()
    for i in ri:
        lasti = i - diri
        for k in rk:
            lastk = k - dirk
            this = mat[i,k]
            other = max(blocked_bys[lasti,lastk], mat[lasti,lastk])
            if other >= this:
                blocked_bys[i,k] = other
                counts[i,k] += 1

# since all the numbers are 0-9, we can setup an array of 0-9 and only
# count +1 for the trees that are bigger than the current max. We then index
# the array for the current value
def count_seen(dirk, mat):
    scores = np.zeros(shape=(len(mat),len(mat)),dtype=np.uint8)
    for i in range(len(mat)):
        k = 0 if dirk > 0 else len(mat) - 1
        local_counts = np.zeros(shape=(10,), dtype=np.uint8)
        while k < len(mat) and k >= 0:
            # register the count up to the tree
            scores[i,k] = local_counts[mat[i,k]]
            local_counts += 1
            # all the counts that are less or equal than this value get reset
            # since they're blocked by this tree.
            local_counts[:mat[i,k] + 1] = 1
            k += dirk
    return scores





# blocked_bys = np.zeros(shape=(len(lines), len(lines)), dtype=np.uint8)
# counts = np.zeros(shape=(len(lines), len(lines)), dtype=np.uint8)

# part 1
# up
# propagate_shadows(-1, 0, lines, counts)
# # down
# propagate_shadows(1, 0, lines, counts)
# # left
# propagate_shadows(0, -1, lines, counts)
# # right
# propagate_shadows(0, 1, lines, counts)

flipped = lines.T
# part 2
# left
scores = count_seen(1, lines)
# right
scores2 = count_seen(-1, lines)
# up
scores3 = count_seen(1, flipped).T
# down
scores4 = count_seen(-1, flipped).T

result = scores * scores2 * scores3 * scores4

print(np.max(result))
