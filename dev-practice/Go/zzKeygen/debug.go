/*
 * Created by Steven Jennings (zzApotheosis) on 24 August 2019.
 *
 * I just learned that using "go fmt" in a working directory will format all of the go
 * source files in that directory. No more using "gofmt -w <whatever>"!
 */

package main

import (
	"fmt"
)

func printAll(n *dataset) {
	fmt.Println("Input:", n.input)
	fmt.Println("Output:", n.output)
	fmt.Println("Fullseed:", n.fullseed)
	fmt.Println("Offset:", n.offset)
	fmt.Println("Charset:", n.charset)
	fmt.Println("Shift:", n.shift)
	fmt.Println("Sequence length:", n.length)
}
