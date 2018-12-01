from itertools import cycle, dropwhile


def parse():
    return map(int, open("input"))


print(sum(parse()))


class Dup:
    def __init__(self):
        self.cur = 0
        self.prev = set()

    def __call__(self, x):
        self.cur += x
        found = self.cur in self.prev
        self.prev.add(self.cur)
        return not found


dup = Dup()
next(dropwhile(dup, cycle(parse())))

print(dup.cur)
