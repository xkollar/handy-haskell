import random

level = 6
a = range(1,2**level)

print "Running for 1..%d (level %d)" % (len(a),level)

def check(a):
    l = len(a)
    b = [False] * (l-1)
    for i in range(l/2):
        root = a[i]
        lv = a[2*i+1]
        rv = a[2*i+2]
        # print abs(root-lv), abs(root-rv)
        b[abs(root-lv)-1] = True;
        b[abs(root-rv)-1] = True;
    return all(b)


# print check([7,1,4,5,6,2,3])
# print check([1,2,3,4,5,6,7])

cnt=1
while True:
    random.shuffle(a)
    if check(a):
        print "%010d: %s" % (cnt,a)
        exit(0)
    cnt = cnt + 1

