/*
 * The original program can be found here:
 * https://gist.github.com/manishtpatel/8222606
 */

package main

import (
	"bufio"
	"fmt"
	"jenningsUtil"
	"os"
	"strings"

	"github.com/atotto/clipboard" // For Linux/Unix-like systems, either xclip or xsel must be installed
)

func main() {
	// Clear terminal
	util.TerminalClear()

	// Explain
	explain()

	// Create variables
	scanner := bufio.NewScanner(os.Stdin)
	var decision, result string

	// Determine encryption or decryption
	fmt.Print("\n\n[E]ncrypt or [D]ecrypt? ")
	scanner.Scan()
	decision = strings.ToLower(scanner.Text())
	if decision != "e" && decision != "d" {
		fmt.Println("Invalid decision.")
		return
	}

	// Get output
	result = process(decision)

	// Output data
	output(result)

	// Wait for keyboard enter before program exit
	fmt.Print("\n\nPress enter to exit program.")
	scanner.Scan()
	return
}

func explain() {
	fmt.Print("This program encrypts or decrypts text messages.")
	fmt.Print("\nThe cryptological application in this program is adequately ")
	fmt.Print("strong, but with all security, there is a risk in using it.")
	fmt.Print("\n\nUse at your own risk.")
}

func process(d string) string {
	// Create variables
	var input, key string
	scanner := bufio.NewScanner(os.Stdin)

	// Fetch input
	fmt.Print("\nEnter input: ")
	scanner.Scan()
	input = scanner.Text()

	// Fetch key
	// Check for existence of key.txt
	if _, err := os.Stat("./key.txt"); os.IsNotExist(err) {
		os.Create("./key.txt") // Create it if it does not exist
		fmt.Println("\"./key.txt\" created. Put your desired key in the first line of that file.")
		os.Exit(0)
	}

	// Open key.txt
	f, err := os.Open("./key.txt")
	util.Check(err)
	defer f.Close()

	// Begin scanning key.txt
	keyScanner := bufio.NewScanner(f)
	keyScanner.Scan()
	key = keyScanner.Text()
	util.Check(keyScanner.Err())

	// Convert key to 256-bit key via zzKeygen
	util.SetSeedAsString(key)
	key = util.OneLineKeygen("1111", 32, 0)
	// Key can be 16 bytes (128-bit) or 32 bytes (256-bit)
	// key := []byte("An example of a 32-bit key!!!!!!")

	// Branch based on decision
	if d == "e" { // If decision == encrypt
		return util.StrEncrypt([]byte(key), input) // encrypt value to base64
	} else if d == "d" { // If decision == decrypt
		return util.StrDecrypt([]byte(key), input) // encrypt base64 crypto to original value
	}

	// Just in case the decision is invalid, return empty string
	return ""
}

func output(in string) {
	fmt.Print("Output: " + in)
	clipboard.WriteAll(in) // Copy to system clipboard
	fmt.Print("\n\nThe output has been copied to the clipboard.")
}
