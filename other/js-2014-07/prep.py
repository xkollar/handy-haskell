import random

a = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,42,44,45,46,48,49,50,51,52,54,55,56,57,58,60,62,63,64,65,66,68,69,70,72,74,75,76,77,78,80,81,84,85,87,88,90,91,92,93,95,96,98,99,100]

# print len(a)

random.shuffle(a)
# a= [1] + a
print "[%s]" % reduce(lambda x,y: str(x) + ','  + str(y), a)