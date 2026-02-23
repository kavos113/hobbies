package main

import (
	"archive/tar"
	"io"
	"io/fs"
	"os"
	"path/filepath"
)

func archiveDirectory(w *tar.Writer, dir string) error {
	return filepath.Walk(dir, func(path string, info fs.FileInfo, err error) error {
		if err != nil {
			return err
		}

		if info.IsDir() {
			return nil
		}

		relPath, err := filepath.Rel(dir, path)
		if err != nil {
			return err	
		}

		header := &tar.Header{
			Name:    relPath,
			Size:    info.Size(),
			Mode:    int64(info.Mode()),
			ModTime: info.ModTime(),
		}

		if err := w.WriteHeader(header); err != nil {
			return err
		}

		file, err := os.Open(path)
		if err != nil {
			return err
		}
		defer file.Close()

		if _, err := io.Copy(w, file); err != nil {
			return err
		}

		return nil
	})
}

func archiveDirectoryFS(w *tar.Writer, dir string) error {
	return w.AddFS(os.DirFS(dir))
}

func main() {
	if len(os.Args) < 2 {
		println("Usage: tar <directory>")
		return
	}

	target := os.Args[1]
	println("Creating tar archive for directory:", target)

	f, err := os.Create(target + ".tar")
	if err != nil {
		panic(err)
	}
	defer f.Close()

	tw := tar.NewWriter(f)
	defer tw.Close()

	if err := archiveDirectoryFS(tw, target); err != nil {
		panic(err)
	}

	println("Tar archive created successfully:", target+".tar")
}
