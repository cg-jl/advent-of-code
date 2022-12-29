
def parse_procedure(line):
    sp = line.split(' ')
    count, from_, to = sp[1], sp[3], sp[5]

    return int(count), int(from_), int(to)


def parse_stacks(lines, count):
    stacks = [[] for _ in range(count)]

    for line in lines:

