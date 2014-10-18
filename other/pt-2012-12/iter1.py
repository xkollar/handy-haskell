import itertools

instSize = 3

def isOk(x,y,a):
    try:
        n = a[x][y]
        return len(set(filter(lambda m: m < n,concat([concat(a[x:x+1])[y-1:y-1+1]
        ,concat(a[x:x+1])[y+1:y+1+1]
        ,concat(a[x-1:x-1+1])[y:y+1]
        ,concat(a[x+1:x+1+1])[y:y+1]
        ])))) + 1 == n
    except IndexError:
        return True

def isOkAll(a):
    n = len(a)
    for i in range(n):
        for j in range(n):
            if not isOk(i,j,a):
                return False
    return True

start = map(lambda x: list(x),[[1] * instanceSize] * instanceSize)

for n in itertools.combinations(itertools.product(range(6), repeat=2),9):
    for (x,y) in itertools.product(range(6), repeat=2)
