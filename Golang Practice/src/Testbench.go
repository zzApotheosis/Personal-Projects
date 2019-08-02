/*
 * A very important concept to understand about Go is the $GOPATH environment
 * variable. I have adjusted my $GOPATH to include the Golang Practice
 * directory, so I can import packages beginning at "Golang Practice/src".
 */

package main

import (
	_ "bufio"
        "fmt"
	_ "jenningsUtil"
       	_ "os"
	"time"
)

func main() {
	fmt.Println("This is a test! Hello world! :D")

	// Gonna play with variables and shit
	var temp1 string = "Yuhhh boi muh waifu is 2B from NieR:Automata and I luv dat schuhweeeeet Mass Effect, Stellaris, and general Sci-Fi crap :drake_LUL:"

	fmt.Println(temp1)
	fmt.Println("YEEEEH BOIII")

	for i := 0; i < 10; i++ {
		fmt.Println(i)
		time.Sleep(1 * time.Second)
	}

	var temp2 int = 10
	fmt.Println(temp2)

	temp3 := "LUL"
	fmt.Println(temp3)

	temp4 := 1337 / 69
	fmt.Println(temp4)

	temp5 := float64(1337) / 69
	fmt.Println(temp5)

	temp6 := 1337.0 / 69
	fmt.Println(temp6)
}

/*
func main() {
	// Check for existence of key.txt
	if _, err := os.Stat("./key.txt"); os.IsNotExist(err) {
		os.Create("./key.txt")
	}

	f, err := os.Open("./key.txt")
	util.Check(err)
	defer f.Close()

	scanner := bufio.NewScanner(f)
	i := 0
	for scanner.Scan() {
		fmt.Println(scanner.Text())
		i++
	}
	fmt.Println(i)

	util.Check(scanner.Err())
}
*/
