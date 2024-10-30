from pwn import *
ords = [int(x) for x in open('p059_cipher.txt').read().split(',')]
chars = 'abcdefghijklmnopqrstuvwxyz'
# key = e x p
'''
for c in chars:
    for d in chars:
        for e in chars:
          decrypt_attempt = str(xor(ords, (c,d,e)))
          if decrypt_attempt.count(' ') < 40:
              continue
          print(decrypt_attempt)
'''

ords = xor(ords, b'exp')
print(sum(list(ords)))