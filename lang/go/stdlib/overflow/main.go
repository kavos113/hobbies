package main

import (
	"fmt"
	"math"
)

func main() {
	var a int32
	a = math.MaxInt32 - 1

	fmt.Printf("maxint32 - 1: %d\n", a)

	a = a + 2

	fmt.Printf("maxInt32 + 1:  %d\n", a)
}
