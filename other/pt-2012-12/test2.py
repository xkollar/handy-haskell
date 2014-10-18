import random

instanceSize = 6

start = map(lambda x: list(x),[[1] * instanceSize] * instanceSize)
chose = range(instanceSize)

def concat(s):
    return reduce(lambda x,y: x+y,s,[])

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

def canInc(x,y,a):
    if a[x][y] == 5:
        return False
    a[x][y] = a[x][y] + 1
    ret = isOk(x,y,a) & isOk(x,y-1,a) & isOk(x,y+1,a) & isOk(x-1,y,a) & isOk(x+1,y,a)
    a[x][y] = a[x][y] - 1
    return ret

def value(a):
    return sum(map(sum,a))

def f():
    print canInc(0,0,start)

def mypp(a):
    print "\n".join(map(str,a))

def canAny(a):
    for i in range(instanceSize):
        for j in range(instanceSize):
            if canInc(i,j,a):
                return True
    return False

def isOkAll(a):
    n = len(a)
    ret = True
    for i in range(n):
        for j in range(n):
            if not isOk(i,j,a):
                print i,j
                ret = False
    return ret

curMax = 92

v = 0
alst =  map(lambda x: list(x),start)

try :
    while True:
        for n in range(500):
            # x = random.choice(chose)
            x = random.randrange(instanceSize)
            # y = random.choice(chose)
            y = random.randrange(instanceSize)
            if canInc(x,y,alst):
                alst[x][y] = alst[x][y]+1
                v = v+1
        if canAny(alst):
            print 'Cont...',
        else:
            if v >= curMax:
                print "\n%d:" % v
                mypp(alst)
                curMax = v
            v = instanceSize**2
            alst =  map(lambda x: list(x),start)
except:
    pass

# example = [[2,3,4,2,3,1],[1,1,1,4,5,1],[2,4,5,3,2,4],[4,3,2,5,1,3],[1,2,1,4,3,2],[3,2,2,2,4,1]]
# example = [[3,2,1,3,4,1],[1,4,3,2,2,4],[2,5,3,2,1,3],[3,1,1,3,5,1],[4,2,1,3,4,2],[1,4,3,2,1,3]]
# print isOkAll(example)
print

# 89:
# [3, 1, 4, 2, 4, 1]
# [2, 4, 3, 1, 3, 2]
# [1, 5, 2, 1, 3, 4]
# [2, 3, 4, 5, 2, 1]
# [3, 1, 1, 3, 4, 3]
# [1, 2, 3, 2, 1, 2]

# 90:
# [[1,3,2,3,1,3],[2,5,1,5,2,2],[3,4,1,4,3,1],[1,2,1,2,3,4],[4,3,4,5,1,2],[2,1,2,3,1,3]]
# [[3,1,2,4,3,1],[2,3,4,1,2,3],[1,4,2,3,1,4],[2,5,1,5,4,2],[4,3,1,2,3,1],[1,2,3,1,4,2]]

# 92:
# [1, 3, 2, 1, 2, 3]
# [1, 5, 4, 3, 4, 1]
# [3, 2, 1, 2, 5, 3]
# [1, 5, 3, 4, 1, 2]
# [3, 4, 2, 1, 3, 4]
# [2, 1, 3, 4, 2, 1]
#
# 92:
# [2, 1, 2, 3, 4, 1]
# [3, 4, 3, 1, 2, 3]
# [1, 2, 5, 4, 3, 1]
# [3, 5, 1, 2, 5, 1]
# [2, 4, 1, 3, 4, 2]
# [1, 3, 2, 4, 1, 3]

