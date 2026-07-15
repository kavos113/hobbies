import sys

def generate_primes(n):
    nums = [True for _ in range(0, n)]
    nums[0] = False
    nums[1] = False

    primes = []

    for i in range(2, n):
        if not nums[i]:
            continue

        primes.append(i)
        
        for j in range(i * 2, n, i):
            nums[j] = False

    return primes

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("print primes by N")
        print("Usage: python3 gen_prime.py <N>")
        sys.exit(1)

    n = int(sys.argv[1])
    primes = generate_primes(n)

    print(primes)