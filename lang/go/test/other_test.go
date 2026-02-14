package main

import "testing"

func TestSomethingElse(t *testing.T) {
	result := SomethingElse()
	expected := "This is something else."
	if result != expected {
		t.Errorf("Expected '%s', got '%s'", expected, result)
	}
}
