package main

import (
	"bufio"
	"fmt"
	_ "io"
	_ "io/ioutil"
	"math/rand"
	"os"
	"time"
)

func main() {
	// fmt.Println("vim-go")
	var file string = "/usr/include/jokes.txt"

	// Load file to count total lines
	dataFile, err := os.Open(file)
	check(err)
	defer dataFile.Close()
	scanner := bufio.NewScanner(dataFile)

	// Count lines
	lineCount := 0
	for scanner.Scan() {
		lineCount++
	}
	dataFile.Close()

	// Choose random line in jokes file to write to stdout
	r1 := rand.New(rand.NewSource(time.Now().UnixNano()))
	selectedLine := r1.Intn(lineCount)

	// Reload the file
	dataFile, err = os.Open(file)
	check(err)
	defer dataFile.Close()
	scanner = bufio.NewScanner(dataFile)

	// Select line in file at which to start
	for i := 0; i < selectedLine; i++ {
		scanner.Scan()
	}

	// Check if selected line is empty.
	// Cycle to next available non-empty line if it is
	if scanner.Text() == "" {
		for scanner.Text() == "" {
			scanner.Scan()
		}
	}

	// Cycle one more entry just to guarantee it prints the full joke
	for scanner.Text() != "" {
		scanner.Scan()
	}
	scanner.Scan()

	// Print joke OMEGALUL
	var text string
	for {
		text = scanner.Text()
		if text == "" {
			break
		} else {
			fmt.Printf(text + "\n")
		}
		scanner.Scan()
	}
}

func check(e error) {
	if e != nil {
		panic(e)
	}
}
