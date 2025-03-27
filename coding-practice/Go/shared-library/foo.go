package main

import "C"

//export Add
func Add(a int, b int) int {
	return a + b
}

//export Greet
func Greet(name *C.char) *C.char {
	goName := C.GoString(name)
	greeting := "Hello, " + goName + "!"
	return C.CString(greeting)
}

// Required for build mode
func main() {}

