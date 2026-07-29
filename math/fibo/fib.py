import sys

def fib(n):
    """Return the nth Fibonacci number."""
    if n < 0:
        raise ValueError("Input should be a non-negative integer.")
    elif n == 0:
        return 0
    elif n == 1:
        return 1
    else:
        a, b = 0, 1
        for _ in range(2, n + 1):
            a, b = b, a + b
        return b

def fib_arr(n):
    """Return an array of the first n Fibonacci numbers."""
    if n < 0:
        raise ValueError("Input should be a non-negative integer.")
    elif n == 0:
        return []
    elif n == 1:
        return [0]
    else:
        arr = [0, 1]
        for _ in range(2, n):
            arr.append(arr[-1] + arr[-2])
        return arr
    
if __name__ == "__main__":
    if len(sys.argv) != 3:
        print("Usage: python fib.py <n> <mod>")
        sys.exit(1)

    n = int(sys.argv[1])
    mod = int(sys.argv[2])

    arr = fib_arr(n)

    arr_mod = [x % mod for x in arr]
    for i in range(n):
        print(f"Fibonacci({i}) = {arr_mod[i]} (mod {mod})")