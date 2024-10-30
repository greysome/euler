L = [list(map(int, line.split(' '))) for line in open('p067_triangle.txt').read().split('\n')]

L_new = []
L_new.append([L[0][0]])
for i in range(1,len(L)):
    row = []
    for j in range(i+1):
        x = L[i][j]
        if j == 0:
            row.append(L_new[i-1][j] + x)
        elif j == i:
            row.append(L_new[i-1][j-1] + x)
        else:
            row.append(max(L_new[i-1][j-1] + x, L_new[i-1][j] + x))
    L_new.append(row)
print(max(L_new[-1]))