def letterval(c):
    if c == 'I': return 1
    elif c == 'V': return 5
    elif c == 'X': return 10
    elif c == 'L': return 50
    elif c == 'C': return 100
    elif c == 'D': return 500
    elif c == 'M': return 1000
    print(f'unknown letter {c}')
    assert False

def subtractive(c1, c2):
    return f'{c1}{c2}' in 'IV IX XL XC CD CM'.split(' ')

def unitlen(n):
    if n == 0: return 0
    elif n in (1,5): return 1
    elif n in (2,4,6,9): return 2
    elif n in (3,7): return 3
    elif n == 8: return 4

def optimallen(val):
    thousands = val // 1000
    val -= thousands * 1000
    hundreds = val // 100
    val -= hundreds * 100
    tens = val // 10
    val -= tens * 10
    ones = val
    return thousands + unitlen(hundreds) + unitlen(tens) + unitlen(ones)

def getval(romanform):
    val = 0
    idx = 0
    while idx < len(romanform):
        c = romanform[idx]
        if idx < len(romanform)-1:
            nextc = romanform[idx+1]
            if subtractive(c, nextc):
                val += letterval(nextc) - letterval(c)
                idx += 2
            else:
                val += letterval(c)
                idx += 1
        else:
            val += letterval(c)
            idx += 1

    return val

romanforms = [line.strip('\n') for line in open('0089_roman.txt', 'r').readlines()]
total = 0
for f in romanforms:
    print(f, len(f), getval(f))
    diff = len(f) - optimallen(getval(f))
    assert diff >= 0
    total += diff
print(total)