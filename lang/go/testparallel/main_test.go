package main

import (
	"fmt"
	"testing"
	"time"
)

func TestFirst(t *testing.T) {
	fmt.Println("test first started")
	time.Sleep(2 * time.Second)
	fmt.Println("test first ended")
}

func TestSecond(t *testing.T) {
	fmt.Println("test second started")
	time.Sleep(1 * time.Second)
	fmt.Println("test second ended")
}