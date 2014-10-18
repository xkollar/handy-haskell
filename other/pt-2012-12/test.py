#!/usr/bin/env python

a = [[1,2,1],[4,3,4],[2,1,2]]
b = [[1,2,3],[4,5,6],[7,8,9]]

def subcheck(x,a):
    return len(set(filter(lambda y: y<x,a))) + 1 == x

def check(a):

