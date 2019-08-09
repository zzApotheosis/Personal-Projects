/*
 * I'm developing this program to autospam random crap on
 * Town of Salem LUL
 */

package main

import (
	"fmt"
	_ "github.com/atotto/clipboard"
	"github.com/micmonay/keybd_event"
	"os"
	"log"
	"bufio"
	util "jenningsUtil"
	_ "reflect"
	"io"
	"time"
)

const (
	limit int = 117
)

func main() {
	kb, err := keybd_event.NewKeyBonding()
	util.Check(err)
	time.Sleep(3 * time.Second) // Give user time to switch applications, this also gives /dev/uinput time to prepare for writing
	// KBString(&kb, "|")
	TOSautopaster(&kb, "./input.txt")
}

func TOSautopaster(k *keybd_event.KeyBonding, sourceFilePath string) {
	// Check if file exists
	if !util.FileExists(sourceFilePath) {
		panic(fmt.Sprintf("%s FILE DOES NOT EXIST!", sourceFilePath))
	}

	// Set variables


	// Read source file
	sourceFile, err := os.Open(sourceFilePath); defer sourceFile.Close(); util.Check(err)
	r := bufio.NewReader(sourceFile)

	// Read rune by rune
	for {
		for i := 0; i < limit; i++ {
			if c, _, err := r.ReadRune(); err != nil {
				if err == io.EOF {
					break
				} else {
					log.Fatal(err)
				}
			} else {
				// fmt.Printf("%q [%d]\n", string(c), sz)
				writeKBEvent(k, c)
				if util.GetOS() == "linux" {
					time.Sleep(10 * time.Millisecond)
				}
			}
		}

		// BETTER GET THIS BEAUTY IN THE CHAT!!!!!!!!!!!!!!!!!!!
		pressEnter(k)

		// Delay to prevent spam detection
		time.Sleep(3 * time.Second)

		// Break if there's no more in the input file
		if r.Buffered() == 0 {
			break
		}
	}
}

func KBString(k *keybd_event.KeyBonding, data string) {
	for _, e := range data {
		writeKBEvent(k, e)
		if util.GetOS() == "linux" {
			// I guess /dev/uinput needs time between each write to function properly
			time.Sleep(10 * time.Millisecond)
		}
	}
}

func writeKBEvent(k *keybd_event.KeyBonding, x rune) {
	doShift, key := getKey(x)
	k.HasSHIFT(doShift)
	k.SetKeys(key)
	util.Check(k.Launching())
}

func pressEnter(k *keybd_event.KeyBonding) {
	k.HasSHIFT(false)
	k.SetKeys(keybd_event.VK_ENTER)
	util.Check(k.Launching())
}

func testEvent(x []byte) {
	kb, err := keybd_event.NewKeyBonding()
	util.Check(err)

	for index, element := range x {
		switch element {
		case 1:
			kb.SetKeys(keybd_event.VK_A)
		case 2:
			kb.SetKeys(keybd_event.VK_B)
		case 3:
			kb.SetKeys(keybd_event.VK_X)
		default:
			fmt.Printf("YEET\n")
		}
		fmt.Printf("Iteration: %d\n", index)
		fmt.Println("Element: ", element)
	}

	kb.HasSHIFT(false)

	util.Check(kb.Launching())
}
