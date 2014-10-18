import random

# level = 4
# a = range(1,2**level)
#
# print "Running for 1..%d (level %d)" % (len(a),level)

def check(a):
    l = len(a)
    b = [False] * (l-1)
    for i in range(l/2):
        root = a[i]
        lv = a[2*i+1]
        rv = a[2*i+2]
        b[abs(root-lv)-1] = True;
        b[abs(root-rv)-1] = True;
    return all(b)

print check([11,22,63,20,21,1,2,23,27,25,29,56,60,58,62,13,18,12,14,16,19,15,17,5,7,3,6,8,10,4,9,50,59,39,47,43,57,53,61,52,54,41,49,34,55,37,45,40,48,30,51,36,44,31,33,24,32,28,42,38,46,26,35])

# while not check(a):
#     random.shuffle(a)
#
# print a
# exit(0)

