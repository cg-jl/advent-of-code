from collections import deque
from functools import cache
from os import PathLike


class Rule:
    def __init__(self, source: PathLike) -> None:
        self.fgraph: dict[str, dict[str, int]] = {}
        self.bgraph: dict[str, dict[str, int]] = {}

        for line in source.split("\n"):
            line = line.strip()
            if not line:
                continue
            self.add_rule(line)

    def add_rule(self, rule: str):
        def parse(edge: str) -> tuple[str, int]:
            count, _, bag = edge.partition(" ")
            bag, _, _ = bag.rpartition(" ")
            return bag, int(count)

        bag, _, edges = rule[:-1].partition(" bags contain ")
        if edges != "no other bags":
            for edge in edges.split(", "):
                k, v = parse(edge)
                self.bgraph.setdefault(k, {})[bag] = self.fgraph.setdefault(bag, {})[
                    k
                ] = v

        else:
            self.fgraph[bag] = {}

    def find_containers_for(self, bag: str) -> set[str]:
        visited: set[str] = set()
        queue = deque({bag})
        container: set[str] = set()
        while queue:
            if (c := queue.popleft()) in visited:
                continue

            p = self.bgraph.get(c, {})
            print(f"{c} -> {p}, { container = }, { queue = }, { visited = }")
            container |= set(p)
            queue.extend(p)
            visited.add(c)

        return container

    @cache
    def weight(self, bag: str) -> int:
        return sum(
            count * (1 + self.weight(other))
            for other, count in self.fgraph[bag].items()
        )


assert Rule(
    """
light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.
"""
).find_containers_for("shiny gold") == {
    "bright white",
    "muted yellow",
    "dark orange",
    "light red",
}