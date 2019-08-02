/*
 * Packages are found in $GOPATH because Go encourages programmers to organize
 * all packages in the $GOPATH directory, which is an OS environment variable.
 * However, it is possible to import local packages.
 *
 * Also, the term "package" in Go is a bit different from other programming
 * languages. A package in Go is simply a folder of .go source files. All of
 * the source files in a package must have the same package designation at the
 * top of their files. I'm still not sure how source files in the same folder
 * as the main source file are in the same "scope" as the main source file
 * because I can't get the main source file to use other source files in the
 * same directory.
 *
 * https://golang.org/doc/code.html#GOPATH
 *
 * Thanks to the lovely people of Reddit (https://bit.ly/2JtBmaR), I finally
 * understood how packages work in Go. It turns out, the source files in the
 * same package as the main file will be in the same scope as the main file
 * if and when the whole main package is built. Local imports (such as "./util")
 * are supported in development with commands like "go run main.go" but are not
 * supported in the fully built executable. That's why it's important to
 * properly set up the $GOPATH environment variable.
 *
 * Also, it's important to understand how the src/pkg/bin folders work in a Go
 * workspace.
 *
 * https://golang.org/doc/code.html#Workspaces
 */

package main

import (
	"fmt"
	"understandingPackages/util"
)

func main() {
	fmt.Println("CHECKPOINT1")
	fmt.Println(util.Add(2, 3)) // 5
	fmt.Println("CHECKPOINT2")
	fmt.Println(util.OtherAdd(3, 4)) // 7
	fmt.Println("CHECKPOINT3")
	Custom()
	fmt.Println("CHECKPOINT4")
}
