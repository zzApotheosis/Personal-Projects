package main

import (
	"fmt"
)

func main() {
	var myVar int = 5
	fmt.Println("myVar =", myVar)
	fmt.Println("myVar Pointer =", &myVar)
	fmt.Println("paramWithoutPointer:", paramWithoutPointer(myVar))
	fmt.Println("paramWithPointer:", paramWithPointer(&myVar))
	fmt.Println("returnWithPointer:", returnWithPointer(&myVar))
	fmt.Println("getPointersPointer:", getPointersPointer(&myVar))
}

func paramWithoutPointer(in int) int {
	return in
}

func paramWithPointer(in *int) int {
	return *in
}

func returnWithPointer(in *int) *int {
	return in
}

/*
 * This one's interesting. It returns a pointer to a pointer to an int.
 */
func getPointersPointer(in *int) **int {
	return &in
}
