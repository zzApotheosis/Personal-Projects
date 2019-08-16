/*
 * Created by Steven Jennings (zzApotheosis) at an unknown time.
 */

package main

import (
	"fmt"
	"os"
)

func main() {
	// Determine whether or not to use this program as an interactive session or as a one-line use
	switch len(os.Args) {
	case 1:
		interactive()
	case 6:
		oneLineExecute(os.Args[1:])
	default:
		warn()
	}
	os.Exit(0)
}

func warn() {
	fmt.Println("Usage: zzKeygen <seed> [offset] <sequence length> <shift> <character set>")
	fmt.Println("<seed> is the desired seed given as a string. This will be concatenated with the offset to form the full seed.")
	fmt.Println("<offset> is an additional component of the seed to further randomize the pseudorandom algorithm. This argument must exist, however, so if no offset is desired, enter \"\" as the argument.")
	fmt.Println("<sequence length> is the desired length of the output string.")
	fmt.Println("<shift> is the desired number of times to \"shift\" the desired output in the positive direction only.")
	fmt.Println("<character set> is a 4-bit binary number representing lowercase letters, uppercase letters, numbers, and symbols in that order.")
}
