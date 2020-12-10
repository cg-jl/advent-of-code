from part1 import make_graph, parse_input, test_input


def solution(text: str) -> int:
    graph = make_graph(parse_input(text))
    # cache[x] is the same as calling f(x)
    # as everything needs the last input to 
    # calculate its output. Therefore I will start
    # with the last item
    cache = {}
    keys = list(graph.keys())[::-1]
    cache[keys[0] + 3] = 1
    for num in keys:
        cache[num] = sum(cache[x] for x in graph[num])

    return cache[0] # last operation done is the 0 one

def test():
    assert solution(test_input) == 19208

if __name__ == "__main__":
    with open('inputs/10.txt') as fh:
        print(solution(fh.read()))