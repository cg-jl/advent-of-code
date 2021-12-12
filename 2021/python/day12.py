from typing import Optional
import time


def build_graph(lines: list[str]) -> tuple[dict[str, list[str]], set[str]]:
    d = dict()
    lower = set()
    for line in lines:
        f, _, t = line.strip().partition("-")
        if f not in d:
            d[f] = []
        if t not in d:
            d[t] = []
        d[f].append(t)
        d[t].append(f)
        if f.islower():
            lower.add(f)
        if t.islower():
            lower.add(t)

    return d, lower


def all_paths(graph: dict[str, list[str]], allowed_twice: Optional[int] = None):
    queue = [(0, "start")]
    path = []
    visited = dict()

    while queue:
        depth, node = queue.pop()
        while len(path) > depth:
            node_ = path.pop()
            if node_ in visited:
                if visited[node_] == 1:
                    del visited[node_]
                else:
                    visited[node_] -= 1




        path.append(node)

        if node == "end":
            yield path
            path.pop()
            continue

        if node in visited and (visited[node] == 2 if node == allowed_twice else True):
            path.pop()
            continue
        


        if node in visited and not (node == allowed_twice and visited[node] != 2):
            path.pop()
            continue


        if node in graph:
            for connection in graph[node]:
                queue.append((depth + 1, connection))
        else:
            path.pop()

        if node.islower():
            if not node in visited:
                visited[node] = 0
            visited[node] += 1

def make_hasher(graph: dict[str, list[str]]):
    # give "points" to each value of the path, and join them in a way that is not reproducible (at least for the number of paths that I produce)
    h_scores = {k:i for i, k in enumerate(graph.keys())}
    def hash(path: list[str]) -> int:
        s = 0
        for item in path:
            score = h_scores[item]
            # using 43 because it's prime and it's higher than the usual 1,2,3,5,7
            # no real math here, just tested and it works out
            s = s * 43 + score
        return s

    return hash


def count(it):
    c = 0
    for _ in it:
        c += 1
    return c

with open("inputs/day12.txt") as file:
    graph, small_caves = build_graph(file)
    hash_path = make_hasher(graph)
    small_caves.remove("start")
    small_caves.remove("end")

    print(f"Part 1: {count(all_paths(graph))} paths")

    # ~0.66s for part 2
    # NOTE: this is doing a lot of unneeded work.
    start = time.time()
    s = set()
    for exception in small_caves:
        for x in all_paths(graph, exception):
            s.add(hash_path(x))

    end = time.time()

    print(f"Part 2: {len(s)} paths")
    print(f"In {end - start} time")
