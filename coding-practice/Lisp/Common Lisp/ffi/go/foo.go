package main

import (
	"C";
	//"fmt";
)

//export Add
func Add(a int, b int) int {
	return a + b
}

//export Greet
func Greet(name *C.char) *C.char {
	goName := C.GoString(name)
	greeting := "Hello, " + goName + "!"
	//fmt.Println(greeting)
	return C.CString(greeting)
}

// Required for build mode
func main() {}

