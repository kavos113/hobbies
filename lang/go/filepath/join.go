package main

import (
	"fmt"
	"path/filepath"
)

func main() {
	dir, err := filepath.Abs("aaa")
	if err != nil {
		fmt.Println("Error:", err)
		return
	}
	fmt.Println("Absolute path:", dir)

	fmt.Printf("abs path + . %v\n", filepath.Join(dir, "."))
}
