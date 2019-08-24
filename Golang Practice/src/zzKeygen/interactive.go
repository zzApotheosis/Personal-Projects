/*
 * Created by Steven Jennings (zzApotheosis) on 15 August, 2019.
 */

package main

import (
  "fmt"
  "bufio"
  "os"
  "strconv"
  "jenningsUtil"
	"github.com/atotto/clipboard" // For Linux/Unix-like systems, either xclip or xsel must be installed
)

func interactive() {
	// Explain program
	explain()

	// Create variables
	scanner := bufio.NewScanner(os.Stdin)
	var input, output, offset, fullseed, charset string
	var length, shift int

	// Fetch input
	// "Seed"
	fmt.Print("Enter seed: ")
	scanner.Scan()
	input = scanner.Text()

	// Exit program if no text was entered
	if input == "" {
		fmt.Println("No text entered. Exiting program.")
		return
	}

	// Offset
	fmt.Print("Offset: ")
	scanner.Scan()
	offset = scanner.Text()

	// Combining "Seed" and Offset
	fullseed = input + offset

	// Sequence length
	fmt.Print("Sequence length: ")
	scanner.Scan()
	temp, err := strconv.ParseInt(scanner.Text(), 10, 64)
	length = int(temp)
	if err != nil {
		fmt.Println("Invalid sequence length.")
		return
	}
	if length <= 0 { // Exit if sequence length is invalid
		fmt.Println("Sequence length cannot be non-positive.")
		return
	}

	// Shift result (Must be non-negative integer)
	fmt.Print("Shift: ")
	scanner.Scan()
	temp, err = strconv.ParseInt(scanner.Text(), 10, 64)
	shift = int(temp)
	if err != nil {
		fmt.Println("Invalid shift value.")
		return
	}
	if shift < 0 {
		fmt.Println("Shift value cannot be negative.")
		return
	}

	// Character Set (Symbols, lowercase letters, uppercase letters, numbers)
	fmt.Print("\nEnter character set as a 4-bit binary number, where each bit ")
	fmt.Print("represents a boolean value for lowercase letters, ")
	fmt.Print("uppercase letters, numbers, and symbols in that order. ")
	fmt.Print("A value of 0000 will exit the program.")
	fmt.Print("\n\nEnter desired character set: ")
	scanner.Scan()
	charset = scanner.Text()

	// Process data; Generate key
	util.SetSeedAsString(fullseed)
	output = util.RandomString(length, charset, shift)
	fmt.Print("\nOutput: ")
	fmt.Print(output)
	clipboard.WriteAll(output) // Copy to system clipboard
	fmt.Print("\nThe output has been copied to the clipboard.")

	// Wait for keyboard enter before program exit
	fmt.Print("\n\nPress enter to exit program.")
	scanner.Scan()
}

func explain() {
	fmt.Print("This program generates a relatively secure set of pseudorandom")
	fmt.Print(" characters for use in cryptographical applications.\n\n")
	fmt.Print("WARNING: The level of security and cryptography generated by")
	fmt.Print(" this application is not as powerful as it could be.\n")
	fmt.Print("This program is intended to be used to scramble data in such a")
	fmt.Print(" way that can be replicated later.\n\n")
	fmt.Print("Use at your own risk.\n\n")
}
