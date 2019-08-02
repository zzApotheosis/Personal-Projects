package main

import (
    "fmt"
    "jenningsUtil"
    "io/ioutil"
    "time"
)

func main() {
    // Clear terminal
	util.TerminalClear()

    // Explain program
	explain()

    // Create variables
    max := 5

    for i := 1; i <= max; i++ {
        err := ioutil.WriteFile("test.txt", []byte(fmt.Sprintf("testlul %d", i)), 0666)
        util.Check(err)
        if i != max {
            time.Sleep(1000 * time.Millisecond)
        }
    }
}

func explain() {
    fmt.Println("lul")
}
