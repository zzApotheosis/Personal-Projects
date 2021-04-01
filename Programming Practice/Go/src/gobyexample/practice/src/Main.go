// Tesing stuff
package main

import (
    "fmt"
    "./testpackage"
)

func main() {
    fmt.Println("Hello world!")

    var a, b int = 2, 4
    var c int

    var ap, bp, cp *int = &a, &b, &c

    for i := 0; i < 5; i++ {
        testMethod(ap, bp, cp)
        fmt.Println(c)
        fmt.Println(a, b, "\n")
    }

    // return // End program

    var temp other.Pencil = other.Pencil{}
    temp.Init()
    fmt.Println(temp)
    temp.SetGraphite(10)
    fmt.Println(temp)
    temp.IncGraphite(5)
    fmt.Println(temp)
    temp.Init()
    fmt.Println(temp)

    test := other.Pencil{}
    test.SetGraphite(1337)
    test.SetVersion("LEET")
    fmt.Println(test)
}

func testMethod(a, b, c *int) {
    *c = *a + *b
    *a++
    *b++
}
