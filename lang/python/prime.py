import math

def product(n):
    p = 1
    for i in range(1, n):
        if math.gcd(i, n) == 1:
            p = (p * i) % n

    return p if p == 1 else p - n

if __name__ == "__main__":
    for i in range(0, 10):
        for j in range(0, 10):
            print(str(product(i * j + 1)).rjust(3), end = "")
        print()