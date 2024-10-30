# Number of subrectangles in large mxn rectangle is
# mn(m+1)(n+1) / 4 = A(A+m+n+1) / 4.
# We want to find m,n such that this is ~2 million; thus
# A ~ sqrt(8 million), or at least bounded above by that.

from math import sqrt, floor

bestm = 0
bestn = 0
best = 8000000

def f(m,n):
    return m*n*(m+1)*(n+1)//4

k = floor(sqrt(8000000))
for A in range(1,k):
    for m in range(1,A):
        if A % m == 0:
            n = A//m
            test = abs(f(m,n) - 2000000)
            if test < best:
                print(m, n, f(m,n), test)
                best = test
                bestm = m
                bestn = n

print(bestm, bestn, best, bestm*bestn)