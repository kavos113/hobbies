package main

import (
	"fmt"
	"sync"
	"time"
)

func sleepOne(wg *sync.WaitGroup, ch chan struct{}) {
	defer wg.Done()
	fmt.Println("Starting sleepOne")
	time.Sleep(1 * time.Second)
	fmt.Println("Slept for 1 second")
	ch <- struct{}{}
}

// depends on sleepOne
func sleepTwo(wg *sync.WaitGroup, ch chan struct{}) {
	defer wg.Done()
	<-ch
	fmt.Println("Starting sleepTwo")
	time.Sleep(2 * time.Second)
	fmt.Println("Slept for 2 seconds")
}

func main() {
	var wg sync.WaitGroup
	wg.Add(2)
	ch := make(chan struct{})
	go sleepOne(&wg, ch)
	go sleepTwo(&wg, ch)

	wg.Wait()
	fmt.Println("All goroutines finished")
}
