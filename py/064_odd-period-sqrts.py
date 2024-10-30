from math import isqrt

k = 0
for i in range(1,10001):
    if isqrt(i)*isqrt(i) != i:
        k += 1
print(k)

periods = [int(line.split(' ')[1]) for line in open('p064_list.txt').read().split('\n')][:k]
print(len([p for p in periods if p % 2 == 1]))