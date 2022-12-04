import sys


def full_overlap(a, b, c, d):
    return (a <= c and b >= d) or (c <= a and d >= b)


def in_range(a, b, c):
    return a <= c and a >= b

def any_overlap(a, b, c, d):
    return in_range(a, c, d) or in_range(b, c, d) or in_range(c, a, b) or in_range(d, a, b)



def parse_ranges(line):
    [a, b] = line.split(',')
    [a, b, c,d] = a.strip().split('-') + b.strip().split('-')
    return int(a), int(b), int(c), int(d)
    

def parse_pairs(input):
    lines = (line.strip() for line in input if line.strip())
    return map(parse_ranges, lines)

pairs = list(parse_pairs(sys.stdin))

full_overlaps = sum(map(lambda t: full_overlap(*t), pairs))
any_overlaps = sum(map(lambda t: any_overlap(*t), pairs))

print(full_overlaps)
print(any_overlaps)

