package main

import "fmt"

func main() {
	var nilarr []int
	nilarr = nil

	fmt.Printf("len = %d\n", len(nilarr))
	fmt.Printf("arr = %+v\n", nilarr)

	fmt.Println(nil == []int{})
	fmt.Println(nil == nilarr)
}
