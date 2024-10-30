from math import isqrt

L = []
for i in range(1,1001):
    if isqrt(i)*isqrt(i) != i:
        L.append(i)

slns = [int(line.split(' ')[1]) for line in open('p066_list.txt').read().split('\n')][:len(L)]
print(max(slns))
print(L[635])