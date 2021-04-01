// Go's _structs_ are typed collections of fields.
// They're useful for grouping data together to form
// records.

package main

import (
    "fmt"
)

// This `person` struct type has `name` and `age` fields.
type person struct {
    name string
    age  int
}

func main() {

    // This syntax creates a new struct.
    fmt.Println(person{"Bob", 20})

    // You can name the fields when initializing a struct.
    fmt.Println(person{name: "Alice", age: 30})

    // Omitted fields will be zero-valued.
    fmt.Println(person{name: "Fred"})

    // An `&` prefix yields a pointer to the struct.
    fmt.Println(&person{name: "Ann", age: 40})

    // Access struct fields with a dot.
    s := person{name: "Sean", age: 50}
    fmt.Println(s.name)

    // You can also use dots with struct pointers - the
    // pointers are automatically dereferenced.
    sp := &s
    fmt.Println(sp.age)

    // Structs are mutable.
    sp.age = 51
    fmt.Println(sp.age)

    // CUSTOM STUFF BELOW THIS COMMENT

    // Using a function created for the person struct
    s.testFunction()

    // Instantiating an empty struct
    temp := person{}

    // Filling the empty struct with stuff, in two ways. The first way:
    temp.age = 1337
    temp.name = "Leet"

    fmt.Println(temp)

    // The second way:
    temp = person{"Karen Gillan", 30}

    fmt.Println(temp)
    fmt.Println("Karen Gillan is absolutely gorgeous!")

    // Editing the struct again
    temp.name = "Steven"
    temp.age = 23

    // Using a function created for the person struct
    temp.testFunction()
}

func (test person) testFunction() {
    fmt.Println(fmt.Sprintf("Test function for person struct. Person's name is %s. Age is %d", test.name, test.age))
}
