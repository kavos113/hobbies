import matplotlib.pyplot as plt
import sys
import gen_prime

def ton(n):
    x = [i * 100 for i in range(1, n + 1)]
    
    mod1 = [0 for _ in range(0, n)]
    mod3 = [0 for _ in range(0, n)]
    primes = gen_prime.generate_primes(n * 100)
    for p in primes:
        if p % 4 == 1:
            mod1[p // 100] += 1
        elif p % 4 == 3:
            mod3[p // 100] += 1

    y1 = [0 for _ in range(0, n)]
    y2 = [0 for _ in range(0, n)]
    y3 = [0 for _ in range(0, n)]
    y1[0] = mod1[0]
    y2[0] = mod3[0]
    y3[0] = (y1[0] + y2[0]) / 2
    for i in range(1, n):
        y1[i] = y1[i - 1] + mod1[i]
        y2[i] = y2[i - 1] + mod3[i]
        y3[i] = (y1[i] + y2[i]) / 2
    
    plt.plot(x, y1, label='1 mod 4')
    plt.plot(x, y2, label='3 mod 4')
    plt.xlabel('Number')
    plt.ylabel('Count')
    plt.title('number of primes (mod 4)')
    plt.legend()
    plt.savefig("out/mod4.png")
    plt.close()

    y1_ = [0 for _ in range(0, n)]
    y2_ = [0 for _ in range(0, n)]
    for i in range(0, n):
        y1_[i] = y1[i] - y3[i]
        y2_[i] = y2[i] - y3[i]
        if y1_[i] == y2_[i]:
            print(f"same: {i * 100}")

    plt.plot(x, y1_, label='1 mod 4')
    plt.plot(x, y2_, label='3 mod 4')
    plt.xlabel('Number')
    plt.ylabel('Difference')
    plt.title('number of primes (mod 4) | difference from average')
    plt.legend()
    plt.savefig("out/mod4_diff.png")
    plt.close()

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: python chebyshev_bias.py <n>")
        sys.exit(1)

    n = int(sys.argv[1])
    ton(n)