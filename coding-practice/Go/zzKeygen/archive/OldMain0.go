/*
 * Created by Steven Jennings (zzApotheosis) on 05 June 2018.
 *
 *******************************************************************************
 *
 * This is just a quick program to hone my Golang knowledge and general
 * programming skills. This program averages an indefinite set of numbers.
 */

package main

import (
    "fmt"
    "os/exec"
    "os"
    "container/list"
    "strings"
    "strconv"
)

func main() {
    // Clear Windows PowerShell
    cmd := exec.Command("cmd", "/c", "cls")
    cmd.Stdout = os.Stdout
    if err := cmd.Run(); err != nil {
        fmt.Println(err)
        return
    }

    // Create variables
    l := list.New() // List
    var input string // User input
    var val float64 // Value parsed from input
    var err error // Possible errors
    var avg, total float64 // Variables for mean calculations

    // Collect input
    for {
        fmt.Print("Input: ")
        fmt.Scanln(&input)

        if strings.ToLower(input) == "exit" {
            break
        }

        val, err = strconv.ParseFloat(input, 64)

        if err != nil {
            fmt.Println(err)
            continue
        }

        l.PushBack(val)
    }

    // Print list
    i := 0
    for e := l.Front(); e != nil; e = e.Next() {
        fmt.Print(i)
        fmt.Print(": ")
        fmt.Print(e.Value)
        fmt.Print("\n")
        i++
    }

    // Calculate average
    for e := l.Front(); e != nil; e = e.Next() {
        total += e.Value.(float64)
    }

    if l.Len() != 0 {
        avg = total / float64(l.Len())
    } else {
        avg = 0.0
    }

    // Print average
    fmt.Println("Average:", avg)
}


/*
func main() {
    cmd := exec.Command("cmd", "/c", "cls")
    cmd.Stdout = os.Stdout
    if err := cmd.Run(); err != nil {
        fmt.Println(err)
        return
    }

    temp := Test{"Initial", 1}

    fmt.Println(temp)

    temp.intfield = 50
    temp.stringfield = "Steven"
    fmt.Println(temp)
}
*/

type Test struct {
    stringfield string
    intfield int
}
