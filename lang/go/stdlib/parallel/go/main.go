package main

import (
	"fmt"
	"sync"
	"time"
)

func sleepOne(wg *sync.WaitGroup) {
	defer wg.Done()
	time.Sleep(1 * time.Second)
	fmt.Println("Slept for 1 second")
}

func sleepTwo(wg *sync.WaitGroup) {
	defer wg.Done()
	time.Sleep(2 * time.Second)
	fmt.Println("Slept for 2 seconds")
}

func sleepThree(wg *sync.WaitGroup) {
	defer wg.Done()
	time.Sleep(3 * time.Second)
	fmt.Println("Slept for 3 seconds")
}

func main() {
	var wg sync.WaitGroup
	wg.Add(3)
	go sleepOne(&wg)
	go sleepTwo(&wg)
	go sleepThree(&wg)

	wg.Wait()
	fmt.Println("All goroutines finished")
}
