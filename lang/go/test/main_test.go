package main

import (
	"fmt"
	"testing"
)

func TestMain(m *testing.M) {
	fmt.Println("Setting up tests...")
	m.Run()
	fmt.Println("Tearing down tests...")
}

func TestAdd(t *testing.T) {
	result := Add(2, 3)
	if result != 5 {
		t.Errorf("Expected 5, got %d", result)
	}
}

func TestSubtract(t *testing.T) {
	result := Subtract(5, 3)
	if result != 2 {
		t.Errorf("Expected 2, got %d", result)
	}
}