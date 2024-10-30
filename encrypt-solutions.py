#!/usr/bin/python

from pathlib import Path
import subprocess

solutions = [s.strip() for s in open('solutions-past-100.txt', 'r').readlines()]

def find_solution_file(num):
    all_sources = [f for f in Path('./c').iterdir()] + \
        [f for f in Path('./lisp').iterdir()] + \
        [f for f in Path('./py').iterdir()]
    res = [f for f in all_sources
            if f.is_file() \
            and '.' in f.name \
            and f.name.startswith(str(num)) \
            and not f.name.endswith('.gpg')]
    already_encrypted = len([f for f in all_sources
                             if f.is_file() \
                             and f.name.startswith(str(num)) \
                             and f.name.endswith('.gpg')]) >= 1
    return res, already_encrypted

def encrypt(f, passphrase):
    cmd = f'gpg -c --passphrase {passphrase} --batch --yes {f.resolve()}'
    result = subprocess.run(cmd, shell=True, capture_output=True, text=True)
    assert result.returncode == 0, f'failed to encrypt {f.name} with passphrase {passphrase}'
    print(f'encrypted {f.resolve()}')

for idx, s in enumerate(solutions):
    num = idx + 101
    fs, already_encrypted = find_solution_file(str(num))
    if len(fs) == 0:
        print(f'no solution yet: {num}')
        continue

    f = fs[0]
    if len(fs) > 1:
        print(f'multiple solutions for {num}')
        print(f'using: {f.resolve()}')
        
    if already_encrypted:
        print(f'already encrypted: {f.resolve()}')
        continue

    encrypt(f, s)