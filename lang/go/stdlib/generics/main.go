package main

func Add[T int | float64](a, b T) T {
	return a + b
}

func main() {
	println(Add(1, 2))     // Output: 3
	println(Add(1.5, 2.5)) // Output: 4.0
}
