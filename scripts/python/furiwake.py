import sys
import random

def furiwake(total, parts):
    dist = {-1: -1}

    for i in range(parts):
        for j in range(total // parts):
            target = -1
            while target in dist:
                target = random.randint(0, total - 1)
            dist[target] = i
    
    result = [[] for _ in range(parts)]
    for key, value in dist.items():
        if key != -1:
            result[value].append(key)

    return result

if __name__ == "__main__":
    if len(sys.argv) != 3:
        print("Usage: python furiwake.py <total> <parts>")
        sys.exit(1)

    total = int(sys.argv[1])
    parts = int(sys.argv[2])

    distribution = furiwake(total, parts)
    
    for i, part in enumerate(distribution):
        print(f"Part {i}: {part}")