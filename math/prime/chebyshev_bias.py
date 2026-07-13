import matplotlib.pyplot as plt
import sys

def is_prime(n):
    """Check if a number is prime."""
    if n <= 1:
        return False
    if n <= 3:
        return True
    if n % 2 == 0 or n % 3 == 0:
        return False
    i = 5
    while i * i <= n:
        if n % i == 0 or n % (i + 2) == 0:
            return False
        i += 6
    return True

def check(n):
    """Run the Chebyshev bias check for numbers up to n."""
    count_1_mod_4 = 0
    count_3_mod_4 = 0

    for i in range(1, n + 1):
        if is_prime(i):
            if i % 4 == 1:
                count_1_mod_4 += 1
            elif i % 4 == 3:
                count_3_mod_4 += 1

    return count_1_mod_4, count_3_mod_4

def ton(n):
    x = [i * 100 for i in range(1, n + 1)]
    y1 = []
    y2 = []
    for i in x:
        c1, c2 = check(i)
        y1.append(c1)
        y2.append(c2)
    plt.plot(x, y1, label='Primes ≡ 1 (mod 4)')
    plt.plot(x, y2, label='Primes ≡ 3 (mod 4)')
    plt.xlabel('Number')
    plt.ylabel('Count')
    plt.title('Chebyshev Bias')
    plt.legend()
    plt.show()

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: python chebyshev_bias.py <n>")
        sys.exit(1)

    n = int(sys.argv[1])
    ton(n)