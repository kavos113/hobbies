package main

import "fmt"

func main() {
	for i := 0; i < 10; i++ {
		switch i {
		case 0:
			fmt.Println("zero")

		case 1:
		case 2:
			fmt.Println("one or two")

		case 3:
		case 4:
		case 5:
			fmt.Println("three to five")

		case 6:
		default:
			fmt.Println("other")
		}
	}
}