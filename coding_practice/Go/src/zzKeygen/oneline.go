/*
 * Created by Steven Jennings (zzApotheosis) on 15 August, 2019.
 */

package main

import (
	_ "fmt"
	"jenningsUtil"
	"os"
	"strconv"
)

// oneLineExecute This does not copy to system clipboard because
// it is designed to be used in shell scripting on Linux.
func oneLineExecute(args []string) {
	switch len(args) {
	case 3:
		threeArgs(args)
	case 5:
		fiveArgs(args)
	default:
		os.Stdout.Write([]byte("waow"))
	}
}

func threeArgs(a []string) {
	// Define variables
	var charset string
	var length, shift int
	var err error

	// Parse arguments
	util.SetSeedAsSysTime()
	length, err = strconv.Atoi(a[0])
	util.Check(err)
	shift, err = strconv.Atoi(a[1])
	util.Check(err)
	charset = a[2]

	// Process data, print to stdout for use in Linux piping. God, Linus Torvalds is big daddy.
	os.Stdout.Write([]byte(util.RandomString(length, charset, shift)))
}

func fiveArgs(a []string) {
	// Define variables
	var fullseed, charset string
	var length, shift int
	var err error

	// Parse arguments
	fullseed = a[0] + a[1]
	length, err = strconv.Atoi(a[2])
	util.Check(err)
	shift, err = strconv.Atoi(a[3])
	util.Check(err)
	charset = a[4]

	// Process data, print to stdout for use in Linux piping. God, Linus Torvalds is big daddy.
	util.SetSeedAsString(fullseed)
	os.Stdout.Write([]byte(util.RandomString(length, charset, shift)))
}
