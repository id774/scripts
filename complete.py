#!/usr/bin/env python
# - * - coding: utf-8 - * -

"""
>>> import complete
>>> complete.get_divisor(6)
[1, 2, 3]
>>> complete.get_divisor(7)
[1]
>>> complete.get_divisor(100)
[1, 2, 4, 5, 10, 20, 25, 50]
>>> complete.get_divisor(200)
[1, 2, 4, 5, 8, 10, 20, 25, 40, 50, 100]
>>> complete.get_divisor(44)
[1, 2, 4, 11, 22]
>>> complete.is_complete(6)
True
>>> complete.is_complete(28)
True
>>> complete.is_complete(4)
False
>>> complete.is_complete(496)
True
>>> complete.is_complete(7)
False
"""
try: xrange
except: xrange = range

def get_divisor(number):
    l = []
    for cnt in range(1,number):
        if number % (cnt) == 0:
            l.append(cnt)
    return l

def is_complete(number):
    multi = 0
    for num in get_divisor(number):
        multi += num
    if multi == number: return True;
    else: return False;

if __name__ == '__main__':
    #import doctest
    #doctest.testmod()
    for cnt in xrange(1,1001):
        if is_complete(cnt):
            print(cnt)

