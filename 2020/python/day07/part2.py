import aoc_helper
from part1 import parse_input, Bag

if __name__ == "__main__":
    #     print(
    #         parse_input(
    #             """
    # shiny gold bags contain 2 dark red bags.
    # dark red bags contain 2 dark orange bags.
    # dark orange bags contain 2 dark yellow bags.
    # dark yellow bags contain 2 dark green bags.
    # dark green bags contain 2 dark blue bags.
    # dark blue bags contain 2 dark violet bags.
    # dark violet bags contain no other bags.
    #     """
    #         ).how_many_inside
    #     )

    import aoc_helper

    data = aoc_helper.fetch(7, 2020)
    aoc_helper.submit(7, 2, parse_input(data).how_many_inside, 2020)
