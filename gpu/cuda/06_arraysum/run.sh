if [ -z "$1" ]; then
  echo "Usage: $0 <file_name>"
  exit 1
fi

ITERATIONS=10

nvcc -o $1 $1.cu
./$1 $ITERATIONS 32
./$1 $ITERATIONS 64
./$1 $ITERATIONS 256
./$1 $ITERATIONS 512
./$1 $ITERATIONS 1024