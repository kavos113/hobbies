import matplotlib.pyplot as plt
import sys
import gen_prime
import numpy as np

def ton(mod, n):
    x = [i * 100 for i in range(1, n + 1)]
    
    mods = np.zeros((mod - 1, n), dtype=np.int64)
    primes = gen_prime.generate_primes(n * 100)
    for p in primes:
        mods[(p % mod) - 1, p // 100] += 1

    # 意味あるところだけ抜く
    _y = mods.cumsum(axis=1)
    y = _y[_y.max(axis=1) > 10]
    ave = y.mean(axis=0)

    print(y[:, :100])
    print(ave[:100])
    
    for i in range(0, y.shape[0]):
        plt.plot(x, y[i], label=f"{i + 1} mod {mod}")
    plt.xlabel('Number')
    plt.ylabel('Count')
    plt.title(f'number of primes (mod {mod})')
    plt.legend()
    plt.savefig(f"out/mod{mod}.png")
    plt.close()

    y_ = y - np.tile(ave, (y.shape[0], 1))

    for i in range(0, y_.shape[0]):
        plt.plot(x, y_[i], label=f"{i + 1} mod {mod}")
    plt.xlabel('Number')
    plt.ylabel('Difference')
    plt.title(f'number of primes (mod {mod}) | difference from average')
    plt.legend()
    plt.savefig(f"out/mod{mod}_diff.png")
    plt.close()

if __name__ == "__main__":
    if len(sys.argv) != 3:
        print("Usage: python chebyshev_bias.py <mod> <n/100>")
        sys.exit(1)

    mod = int(sys.argv[1])
    n = int(sys.argv[2])
    ton(mod, n)