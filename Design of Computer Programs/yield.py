def ints(start, end=None):
    i = start
    while start <= end or end is None:
        yield i
        i+=1

def allints():
    yield 0
    for i in ints(0):
        yield i
        yield -i
