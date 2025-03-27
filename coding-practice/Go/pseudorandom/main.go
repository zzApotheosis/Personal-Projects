package main

import (
	"bufio"
	"crypto/md5"
	"encoding/binary"
	"fmt"
	"github.com/atotto/clipboard"
	"io"
	"math/rand"
	"os"
	"strconv"
)

func main() {
	// Program intro
	explain()

	// Create variables
	scanner := bufio.NewScanner(os.Stdin)

	// Fetch input
	// Seed
	fmt.Printf("Enter seed: ")
	scanner.Scan()
	input := scanner.Text()
	if input == "" {
		fmt.Println("No text entered. Exiting program.")
		return
	}

	// Offset
	fmt.Printf("Offset: ")
	scanner.Scan()
	offset := scanner.Text()

	// Combining "Seed" and Offset
	dataIn := input + offset

	// Sequence length
	fmt.Printf("Sequence length: ")
	scanner.Scan()
	length, err := strconv.ParseInt(scanner.Text(), 10, 64)
	if err != nil {
		panic(err)
		return
	}

	// Calculate MD5
	h := md5.New()
	io.WriteString(h, dataIn)
	var seed uint64 = binary.BigEndian.Uint64(h.Sum(nil))
	setSeed(seed)
	result := randomNumSequence(int(length))
	fmt.Println("Output:", result)

	// Write to clipboard
	clipboard.WriteAll(result)
	fmt.Printf("\nThe output has been copied to the clipboard.")

	// Wait for keyboard enter before program exit
	fmt.Printf("\n\nPress enter to exit program.")
	scanner.Scan()
	return
}

func explain() {
	fmt.Printf("This program generates a pseudorandom sequence of numbers" +
		" (of variable length) based on a given seed.\n\n")
}

func setSeed(in uint64) {
	rand.Seed(int64(in))
}

func randomInt(min, max int) int {
	return min + rand.Intn(max-min)
}

func randomString(l int) string {
	bytes := make([]byte, l)
	for i := 0; i < l; i++ {
		// 32 to 126 for typical ASCII characters
		// 48 to 57 for numerical ASCII characters 0-9
		bytes[i] = byte(randomInt(48, 57))
	}
	return string(bytes)
}

func randomNumSequence(length int) string {
	return randomString(length)
}
