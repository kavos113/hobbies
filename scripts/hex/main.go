package main

import (
	"fmt"
	"os"
)

func usage() {
	fmt.Println("Usage: hex <output-format> <input-format> <input>")
	fmt.Println("  Formats: hex, dec, bin")
}

func main() {
	if len(os.Args) < 4 {
		usage()
	}

	format := os.Args[1]
	inputFormat := os.Args[2]
	input := os.Args[3]

	var value int64
	var err error

	switch inputFormat {
	case "hex":
		_, err = fmt.Sscanf(input, "%x", &value)
	case "dec":
		_, err = fmt.Sscanf(input, "%d", &value)
	case "bin":
		_, err = fmt.Sscanf(input, "%b", &value)
	default:
		fmt.Printf("Unknown input format: %s\n", inputFormat)
		return
	}

	if err != nil {
		fmt.Printf("Error parsing input: %v\n", err)
		return
	}

	switch format {
	case "hex":
		fmt.Printf("%x\n", value)
	case "dec":
		fmt.Printf("%d\n", value)
	case "bin":
		fmt.Printf("%b\n", value)
	default:
		fmt.Printf("Unknown output format: %s\n", format)
	}
}
