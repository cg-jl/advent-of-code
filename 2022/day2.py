import sys


points = {
        # rock, rock: 1 for rock + 3 for draw
        ('A', 'X'): 1 + 3,
        # rock, paper: 2 for paper + 6 for win
        ('A', 'Y'): 2 + 6,
        # rock, scissors: 3 for scissors + 0 for loss
        ('A', 'Z'): 3 + 0,
        # paper, rock: 1 for rock + 0 for loss
        ('B', 'X'): 1 + 0,
        # paper, paper: 2 for paper + 3 for draw
        ('B', 'Y'): 2 + 3,
        # paper, scissors: 3 for scissors + 6 for win
        ('B', 'Z'): 3 + 6,
        # scissors, rock: 1 for rock + 6 for win
        ('C', 'X'): 1 + 6,
        # scissors, paper: 2 for paper + 0 for loss
        ('C', 'Y'): 2 + 0,
        # scissors, scissors: 3 for scissors + 3 for win
        ('C', 'Z'): 3 + 3,
        }

points2 = {
        # rock, needs loss -> scissors, 3 + 0
        ('A', 'X'): 3 + 0,
        # rock, needs draw -> rock, 1 + 3
        ('A', 'Y'): 1 + 3,
        # rock, needs  win -> paper, 2 + 6
        ('A', 'Z'): 2 + 6,
        # paper, needs loss -> rock, 1 + 0
        ('B', 'X'): 1 + 0,
        # paper, needs draw -> paper, 2 + 3
        ('B', 'Y'): 2 + 3,
        # paper, needs win -> scissors, 3 + 6
        ('B', 'Z'): 3 + 6,
        # scissors, needs loss -> paper, 2 + 0
        ('C', 'X'): 2 + 0,
        # scissors, needs draw -> scissors, 3 + 3
        ('C', 'Y'): 3 + 3,
        # scissors, needs win -> rock, 1 + 6
        ('C', 'Z'): 1 + 6,
        }


def parse(line):
    [a, b] = line.split(' ')
    return a, b

lines = [parse(line.strip()) for line in sys.stdin if line.strip()]

score = sum(points[line] for line in lines)
score2 = sum(points2[line] for line in lines)


print(score)
print(score2)
