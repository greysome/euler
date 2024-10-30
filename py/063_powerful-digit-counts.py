# We are counting the number of solutions n,k to
#
# floor(log10(n^k))+1 = floor(k*log10(n))+1 = k.
#
# Note that 1 <= n < 10, and so we count the number of solutions
# k >= 1 for each n.

from math import floor, log10
sols = 0
for n in range(1,10):
    k = 1
    while floor(k*log10(n))+1 == k:
        print(f'{k}*log10({n})+1 = {k}')
        k += 1
    sols += k-1
print(sols)