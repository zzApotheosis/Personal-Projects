/*
 * Well, it turns out that genuinely nested functions actually don't exist in
 * Go. That's actually for the better; I'd rather not have to worry about
 * complications like that.
 *
 * However, it is still possible to assign a function declaration to a variable
 * as shown below. That's about as nested as a function can get.
 */

package main

import (
    "fmt"
    "runtime"
)



func main() {
    nestedFunction := func() {
        fmt.Println("LUL")
    }

    nestedFunction()
    nestedFunction()
    nestedFunction()

    fmt.Println(runtime.GOOS)
}
