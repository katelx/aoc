from itertools import cycle, accumulate


def parse():
    return map(int, open("input"))


print(sum(parse()))

prev = set()

print(next(x for x in accumulate(cycle(parse())) if x in prev or prev.add(x)))
