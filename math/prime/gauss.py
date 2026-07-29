import prime_lib
import sys

def main(n, mod, print_all=False):
    for x in range(n):
        for y in range(x+1, n):
            p = 5*y**2 - x**2
            if prime_lib.is_prime(p):
                print(f"({x: >3}, {y: >3}): {p: >6}, mod {mod} {p%mod}")
            else:
                if print_all:
                    print(f"({x: >3}, {y: >3}): {p: >6}, mod {mod} {p%mod} - not prime")
    # for x in range(n):
    #     for y in range(x+1, n):
    #         for z in range(y+1, n):
    #             p = x**3 + y**3 + z**3
    #             if prime_lib.is_prime(p):
    #                 print(f"({x: >3}, {y: >3}, {z: >3}): {p: >6}, mod {mod} {p%mod}")

def by_prime_list(n, mod, print_all=False):
    primes = prime_lib.generate_primes(n ** 2)

    tmods = {}
    fmods = {}

    for i in range(mod):
        tmods[i] = 0
        fmods[i] = 0

    for p in primes:
        eq = False

        for x in range(n*2):
            for y in range(n*2):
                if x**2 - 14*y**2 == p:
                    print(f"({x: >3}, {y: >3}): {p: >6}, mod {mod} {p%mod}")
                    eq = True
                    tmods[p % mod] += 1

        if not eq and print_all:
            print(f"not found   {p: >6}, mod {mod} {p%mod}")
            fmods[p % mod] += 1

    print(f"true  mods:")
    for k, v in tmods.items():
        if v > 0:
            print(f"  {k: >3}: {v: >3}")
    print(f"false mods:")
    for k, v in fmods.items():
        if v > 0:
            print(f"  {k: >3}: {v: >3}")

if __name__ == '__main__':
    if len(sys.argv) < 3:
        print("Usage: gauss.py <n> <mod> [-p]")
        print("  <n>   : upper limit for x and y")
        print("  <mod> : modulus for checking")
        print("  -p    : optional flag to print all results, including non-primes")
        sys.exit(1)
    
    n = int(sys.argv[1])
    mod = int(sys.argv[2])
    print_all = '-p' in sys.argv

    by_prime_list(n, mod, print_all)