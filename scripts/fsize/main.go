package main

import (
	"fmt"
	"io/fs"
	"log"
	"os"
	"path/filepath"
	"sort"
	"sync"
)

func main() {
	if len(os.Args) < 2 {
		log.Fatal("Usage: fsize <folder_path> [d]")
	}
	dirPath, err := filepath.Abs(os.Args[1])
	if err != nil {
		log.Fatalf("無効なパスです: %v", err)
	}

	if len(os.Args) == 3 && os.Args[2] == "d" {
		entries, err := os.ReadDir(dirPath)
		if err != nil {
			log.Fatalf("ディレクトリの読み込みに失敗しました: %v", err)
		}

		type dirSize struct {
			name string
			size int64
			dirCount int64
			fileCount int64
		}
		dirs := make([]dirSize, 0, len(entries))
		for i, entry := range entries {
			size, fileCount, dirCount, err := getDirSizeSync(filepath.Join(dirPath, entry.Name()))
			if err != nil {
				log.Printf("サイズの取得に失敗しました: %v", err)
				continue
			}
			dirs = append(dirs, dirSize{entry.Name(), size, dirCount, fileCount})
			fmt.Printf("Processed %d/%d\r", i+1, len(entries))
		}
		sort.Slice(dirs, func(i, j int) bool {
			return dirs[i].size > dirs[j].size
		})
		fmt.Println("size      files    dirs     name")
		for _, d := range dirs {
			fmt.Printf("%9s %8d %8d %s\n", formatSize(d.size), d.fileCount, d.dirCount, d.name)
		}
	} else {
		size, fileCount, dirCount, err := getDirSizeSync(dirPath)
		if err != nil {
			log.Fatalf("サイズの取得に失敗しました: %v", err)
		}

		fmt.Printf("Size of '%s': %s\n", dirPath, formatSize(size))
		fmt.Printf("Files: %d\n", fileCount)
		fmt.Printf("Directories: %d\n", dirCount)
	}
}

func getDirSizeSync(path string) (int64, int64, int64, error) {
	var size int64
	var fileCount int64
	var dirCount int64

	_ = filepath.WalkDir(path, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() {
			dirCount++
			return nil
		}
		info, err := d.Info()
		if err != nil {
			return err
		}
		size += info.Size()
		fileCount++
		return nil
	})
	return size, fileCount, dirCount, nil
}

func getDirSize(path string) (int64, error) {
	var wg sync.WaitGroup
	sizeChan := make(chan int64, 10)

	wg.Add(1)
	go walkDir(path, &wg, sizeChan)

	go func() {
		wg.Wait()
		close(sizeChan)
	}()

	var size int64
	for s := range sizeChan {
		size += s
	}

	return size, nil
}

func walkDir(path string, wg *sync.WaitGroup, sizeChan chan<- int64) {
	defer wg.Done()

	entries, err := os.ReadDir(path)
	if err != nil {
		return
	}

	for _, entry := range entries {
		if entry.IsDir() {
			wg.Add(1)
			go walkDir(filepath.Join(path, entry.Name()), wg, sizeChan)
		} else {
			info, err := entry.Info()
			if err != nil {
				log.Printf("ファイル情報の取得に失敗しました: %v", err)
				continue
			}
			sizeChan <- info.Size()
		}
	}
}

func formatSize(size int64) string {
	const (
		B  = 1
		KB = B * 1024
		MB = KB * 1024
		GB = MB * 1024
	)

	switch {
	case size >= GB:
		return fmt.Sprintf("%.2f GB", float64(size)/float64(GB))
	case size >= MB:
		return fmt.Sprintf("%.2f MB", float64(size)/float64(MB))
	case size >= KB:
		return fmt.Sprintf("%.2f KB", float64(size)/float64(KB))
	default:
		return fmt.Sprintf("%d B", size)
	}
}
