package main

import (
	"fmt"
	"image"
	"io"
	"os"
	"time"

	"github.com/klippa-app/go-pdfium"
	"github.com/klippa-app/go-pdfium/requests"
	"github.com/klippa-app/go-pdfium/webassembly"
	"github.com/makiuchi-d/gozxing"
	"github.com/makiuchi-d/gozxing/qrcode"
)

type PdfReader struct {
	pool pdfium.Pool
}

func NewPdfReader() (*PdfReader, error) {
	pool, err := webassembly.Init(webassembly.Config{
		MaxTotal: 1,
		Stdout:   io.Discard,
		Stderr:   io.Discard,
	})
	if err != nil {
		return nil, fmt.Errorf("failed to create pdf pool: %w", err)
	}

	return &PdfReader{
		pool: pool,
	}, nil
}

func (r *PdfReader) Close() error {
	return r.pool.Close()
}

func (r *PdfReader) ReadQrCodes(data []byte) ([]string, error) {
	if len(data) == 0 {
		return nil, fmt.Errorf("invalid pdf")
	}

	instance, err := r.pool.GetInstance(30 * time.Second)
	if err != nil {
		return nil, fmt.Errorf("failed to get instance: %w", err)
	}
	defer instance.Close()

	doc, err := instance.OpenDocument(&requests.OpenDocument{
		File: &data,
	})
	if err != nil {
		return nil, fmt.Errorf("failed to open pdf: %w", err)
	}
	defer instance.FPDF_CloseDocument(&requests.FPDF_CloseDocument{
		Document: doc.Document,
	})

	pageCount, err := instance.FPDF_GetPageCount(&requests.FPDF_GetPageCount{
		Document: doc.Document,
	})
	if err != nil {
		return nil, fmt.Errorf("failed to get page count: %w", err)
	}
	fmt.Printf("page: %d\n", pageCount.PageCount)

	for i := range pageCount.PageCount {
		pageRender, err := instance.RenderPageInDPI(&requests.RenderPageInDPI{
			DPI: 200,
			Page: requests.Page{
				ByIndex: &requests.PageByIndex{
					Document: doc.Document,
					Index:    i,
				},
			},
		})
		if err != nil {
			return nil, fmt.Errorf("failed to render pdf: %w", err)
		}

		code, err := readQr(pageRender.Result.Image)
		if err != nil {
			return nil, fmt.Errorf("failed to read qr: %w", err)
		}

		fmt.Printf("qr: %s\n", code)

		pageRender.Cleanup()
	}

	return nil, nil
}

func readQr(img image.Image) (string, error) {
	bmp, err := gozxing.NewBinaryBitmapFromImage(img)
	if err != nil {
		return "", fmt.Errorf("failed to create bitmap: %w", err)
	}

	qrReader := qrcode.NewQRCodeReader()
	result, err := qrReader.Decode(bmp, nil)
	if err != nil {
		return "", fmt.Errorf("failed to decode qr: %w", err)
	}

	return result.GetText(), nil
}

func main() {
	if len(os.Args) < 2 {
		fmt.Println("Usage: qr <in pdf file>")
		os.Exit(1)
	}

	pdffile := os.Args[1]

	data, err := os.ReadFile(pdffile)
	if err != nil {
		panic(err)
	}

	r, err := NewPdfReader()
	if err != nil {
		panic(err)
	}

	_, err = r.ReadQrCodes(data)
	if err != nil {
		panic(err)
	}
}
