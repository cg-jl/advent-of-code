from typing import Iterable
import itertools
def parse_mask(line: str) -> int:
    _, _, mask_input = line.rpartition(' ')
    m, f = 0, 0
    for i in mask_input:
        m <<= 1
        f <<= 1
        if i == 'X':
            f |= 1
        elif i == '1':
            m |= 1


    return m, f


def parse_assign(line: str) -> tuple[int, int]:
    line = line[4:]
    ki = line.index(']')
    k = int(line[:ki])
    v = int(line[ki + 4:])
    return k, v


"""
truth table
===================================================
v m X || out || 
---------------
0 0 0 ||  0  ||  -> m wins
0 1 0 ||  1  ||  -> m wins
0 0 1 ||  0  || -> doesnt matter if m was 0, f is 1
0 1 1 ||  0  || -> doesnt matter if m was 1, f is 1
1 0 0 ||  0  || -> m wins (f is 0)
1 1 0 ||  0  || -> m wins (f is 0)
1 0 1 ||  1  || -> f wins -> x wins
1 1 1 ||  1  || -> f wins -> x wins


if X[i] then v[i] else m[i]


a b || a -> b
-------------
0 0 ||   1
0 1 ||   1
1 0 ||   0
1 1 ||   1

if a then b = (NOT a) OR (a AND b)
a -> b = !a | (a & b)

!X[i] -> m[i]
X[i] -> v[i]

X[i] | (!X[i] & m[i])
!X[i] | (X[i] & v[i])

(X[i] | (!X[i] & m[i])) & (!X[i] | (X[i] & v[i]))

(X | (~X & m)) & (~X | (X & v))

first expression can be calculated when getting the mask, and
we can save ~X and X as well for later
"""

def get_first_expr(xs: int, mask:int) -> int:
    return xs | (~xs & mask)

def apply_mask(first_expr: int, xs: int, not_xs: int, value: int) -> int:
    return first_expr & (not_xs | (xs & value))

def part1(text: str) -> int:
    xs, not_xs, first_expr = 0, 0, 0
    
    # 36-bit address space -> hashmap; rust will be [0 ; 2^36]
    mem = {}
    for line in filter(len, map(str.strip, text.split('\n'))):
        if line[1] == 'a':
            mask, xs = parse_mask(line)
            not_xs = ~xs
            first_expr = xs | (not_xs & mask)
        else:
            k, v = parse_assign(line)
            mem[k] = apply_mask(first_expr, xs, not_xs, v)

    return sum(mem.values())


def count_1s_bin(b: int) -> int:
    cnt = 0
    idxs = []
    i = 0
    while b > 0:
        cnt += b & 1
        if b & 1:
            idxs.append(i)

        i += 1
        b >>= 1

    assert cnt == len(idxs), "count_1s_bin: assertion error"

    return idxs, len(idxs)
        




def part2(text: str) -> int:
    mask, xs, xs_count = 0, 0, 0
    xs_idxs = []
    mem = {}
    for line in filter(len, map(str.strip, text.split('\n'))):
        if line[1] == 'a':
            mask, xs = parse_mask(line)
            xs_idxs, xs_count = count_1s_bin(xs)
            

        else:
            k, v = parse_assign(line)

            # override k with what is in mask if not in X
            k &= xs | ~mask # ~(~xs & mask) = ~~(~~xs | ~mask) = xs | ~mask
            k |= ~xs & mask


            # i need to generate all the xs where the 'X' are either 1 or 0
            gen = itertools.product((0, 1), repeat=xs_count)
            for p in gen:
                # copy it
                k0 = k
                # set value
                for i, idx in enumerate(xs_idxs):
                    # override bit with Xs
                    k0 &= ~(1 << idx)
                    k0 |= p[i] << idx


                # now I have k0 as the key
                mem[k0] = v

    return sum(mem.values())


assert part1("""
mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
mem[8] = 11
mem[7] = 101
mem[8] = 0
""") == 165


assert part2("""
mask = 000000000000000000000000000000X1001X
mem[42] = 100
mask = 00000000000000000000000000000000X0XX
mem[26] = 1
""") == 208


with open('inputs/14.txt') as fh:
    print(part2(fh.read()))
