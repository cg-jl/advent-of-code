import sys

def parse_calories(input):
    return [sum( int(cal) for cal in s.split('\n') ) for s in input.strip().split('\n\n')]



calories = sorted(parse_calories(sys.stdin.read()))
calories.reverse()



print(calories[0])
print(sum(calories[:3]))
