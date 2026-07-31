package main

import (
	"bytes"
	"cmp"
	"encoding/json"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"slices"
)

type entry struct {
	Name string `json:"name"`
	Line int    `json:"line"`
}

func main() {
	if len(os.Args) < 3 {
		fmt.Println("Usage: cloc <dir> <outfile>")
		fmt.Println("    dir: required git repository")
		os.Exit(1)
	}

	dir := os.Args[1]
	files, err := listFiles(dir)
	if err != nil {
		panic(err)
	}

	lines := make(map[string]int)

	r := NewRules()

	fmt.Printf("Total %d files\n", len(files))

	for _, f := range files {
		lang := r.DetectLanguage(f)
		line, err := countLine(f)
		if err != nil {
			fmt.Printf("[ERROR]: %+v", err)
			continue
		}

		if lang == "" {
			continue
		}

		_, ok := lines[lang]
		if !ok {
			lines[lang] = line
			continue
		}
		lines[lang] += line
	}

	entries := make([]entry, 0, len(lines))

	fmt.Println("\n---------------- Lines ---------------")
	for lang, line := range lines {
		fmt.Printf("%s: %d lines\n", lang, line)
		entries = append(entries, entry{lang, line})
	}

	slices.SortFunc(entries, func(a, b entry) int {
		return cmp.Compare(a.Line, b.Line)
	})

	outfile := os.Args[2]
	data, err := json.MarshalIndent(entries, "", "  ")
	if err != nil {
		panic(err)
	}
	err = os.WriteFile(outfile, data, 0644)
	if err != nil {
		panic(err)
	}
}

func countLine(path string) (int, error) {
	file, err := os.Open(path)
	if err != nil {
		return 0, err
	}
	defer file.Close()

	buf := make([]byte, 32*1024)
	count := 0
	sep := []byte{'\n'}

	for {
		c, err := file.Read(buf)
		count += bytes.Count(buf[:c], sep)

		if err == io.EOF {
			return count, nil
		} else if err != nil {
			return 0, err
		}
	}
}

func listFiles(dir string) ([]string, error) {
	cmd := exec.Command("git", "-C", dir, "ls-files", "-z")

	out, err := cmd.Output()
	if err != nil {
		return nil, err
	}

	rawFiles := bytes.Split(out, []byte{0})
	files := make([]string, 0, len(rawFiles))
	for _, f := range rawFiles {
		if len(f) == 0 {
			continue
		}
		path := string(f)

		osRelPath := filepath.FromSlash(path)
		absPath, err := filepath.Abs(filepath.Join(dir, osRelPath))
		if err != nil {
			continue
		}

		files = append(files, absPath)
	}

	return files, nil
}
