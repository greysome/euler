# Let b be number of blue discs and T be the total. We can transform
# the equation 2b(b-1) = T(T-1) into 2(b-1/2)^2 - (T-1/2)^2 = 1/4,
# and this reduces to solving the Pell equation
# 2p^2 - q^2 = 1, where b=(p+1)/2 and T=(q+1)/2.

from math import isqrt

# First few solutions q to the Pell equation q^2-2p^2 = -1, from OEIS
qqs = [1,49,1681,57121,1940449,65918161,2239277041,76069501249,2584123765441,87784138523761,2982076586042449,101302819786919521,3441313796169221281,116903366249966604049,3971273138702695316401,134906383349641674153601,4582845760749114225906049,155681849482120242006652081]

for qq in qqs:
    q = isqrt(qq)
    total = (q+1)//2
    if total > 10**12:
        p = isqrt((qq+1)//2)
        blue = (p+1)//2
        print(blue)
        assert(total*(total-1) == 2*blue*(blue-1))
        break