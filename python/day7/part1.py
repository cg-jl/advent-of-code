import re
from pprint import pprint

# color: (\w+\s\w+)
# number: \d+
# list: {number} {color} bags?[,list]
# description: {color} bags contain ({list} | no other bags).


from dataclasses import dataclass
from functools import reduce, cached_property
import operator
from typing import Callable

color_table = {}


def apply_arguments(f):
    def g(args):
        return f(*args)

    return g


@dataclass
class Bag:
    id: int
    color: str
    containers: dict["Bag", int]
    contains: dict["Bag", int]

    @cached_property
    def how_many_inside(self):
        return sum(
            map(
                apply_arguments(lambda bag, count: count + count * bag.how_many_inside),
                self.contains.items(),
            )
        )

    def num_containers(self, already_visited: set["Bag"] = set()):
        num = 0
        for container in self.containers.keys():
            if container in already_visited:
                continue
            num += container.num_containers(already_visited) + 1
            already_visited.add(container)
        return num

    # methods so I can get a bag into a dict
    def __hash__(self) -> int:
        return hash(self.color)

    def __eq__(self, other):
        return self.color == other


def get_id(color: str, bags: list[Bag], color_table: dict[str, int]) -> int:
    if color not in color_table:
        next_id = len(bags)
        color_table[color] = next_id
        bags.append(Bag(next_id, color, {}, {}))

    return color_table[color]


def compose(a, b) -> Callable[[any], any]:
    def c(*args, **kwargs):
        return a(b(*args, **kwargs))

    return c


def parse_line(
    line: str, bags: list[Bag], color_table: dict[str, int]
) -> tuple[str, int]:
    line = line.strip()
    index_contain = line.index("contain")
    color = " ".join(line[:index_contain].split(" ")[:2])
    bag_id = get_id(color, bags, color_table)
    current_bag = bags[bag_id]

    other_bags = map(
        compose(
            lambda vs: (
                0 if vs[0] == "no" else int(vs[0]),
                " ".join(vs[1:3] if vs[0] != "no" else ""),
            ),
            lambda descr: descr.split(" "),
        ),
        line[index_contain + 8 : -1].split(", "),
    )

    # for each bag, I need to know who contains that bag.
    for quantity, other_color in other_bags:
        if quantity == 0:
            continue
        b_id = get_id(other_color, bags, color_table)
        # two way mapping for part 2
        bags[b_id].containers[current_bag] = quantity
        current_bag.contains[bags[b_id]] = quantity

    return (current_bag, bag_id)


# gimme golden bag and all other bags
def parse_input(input: str) -> Bag:
    bags: list[Bag] = []
    color_table: dict[str, int] = {}
    for line in input.split("\n"):
        if not line.strip():
            continue
        parse_line(line.strip(), bags, color_table)

    return bags[color_table["shiny gold"]]


# shiny_gold = parse_input(
#     """
# light red bags contain 1 bright white bag, 2 muted yellow bags.
# dark orange bags contain 3 bright white bags, 4 muted yellow bags.
# bright white bags contain 1 shiny gold bag.
# muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
# shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
# dark olive bags contain 3 faded blue bags, 4 dotted black bags.
# vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
# faded blue bags contain no other bags.
# dotted black bags contain no other bags.
# """
# )
# print shiny_gold.num_containers()

if __name__ == "__main__":
    import aoc_helper
    from pathlib import Path
    import os

    aoc_helper.DATA_DIR = repr(
        Path(os.path.realpath(__file__)).parent.parent / "inputs"
    )
    inp = aoc_helper.fetch(7, 2020)
    print(parse_input(inp).num_containers())