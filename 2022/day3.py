import sys

priorities = {}
for letter in range(ord('a'), ord('z') + 1):
    priorities[letter] = letter - ord('a') + 1
for letter in range(ord('A'), ord('Z') + 1):
    priorities[letter] = letter - ord('A') + 27


line = 'vJrwpWtwJgWrhcsFMMfFFhFp'


def get_shared_item(line):
    mid = len(line)//2
    a, b = line[:mid], line[mid:]
    common = set(a) & set(b)
    return common.pop()




rucksacks = [line.strip() for line in sys.stdin if line.strip()]

badges = [(set(rucksacks[i]) & set(rucksacks[i + 1]) & set(rucksacks[i +
                                                                     2])).pop()
          for i in range(0, len(rucksacks), 3)]


priority_sum = sum(priorities[ord(get_shared_item(rs))] for rs in rucksacks)
sum2 = sum(priorities[ord(b)] for b in badges)

print(priority_sum)
print(sum2)
