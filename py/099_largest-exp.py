lines = open('0099_base_exp.txt').readlines()
pairs = [line.split(',') for line in lines]
pairs = [(int(a),int(b)) for a,b in pairs]

maxlog = 0
maxline = None

from math import log
for idx,(base,exp) in enumerate(pairs):
    curlog = exp*log(base)
    if curlog > maxlog:
        maxlog = curlog
        maxline = idx+1

print(maxlog, maxline)