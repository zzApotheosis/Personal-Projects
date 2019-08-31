/*
 * I'm developing this program to autospam random crap on
 * Town of Salem LUL
 *
 * TODO: Implement a new approach that uses clipboard to copy and paste,
 * instead of trying to "copy/paste" through the keyboard events.
 */

package main

import (
	"bufio"
	"fmt"
	_ "github.com/atotto/clipboard"
	"github.com/micmonay/keybd_event"
	"io"
	util "jenningsUtil"
	"log"
	"os"
	"strconv"
	_ "reflect"
	"time"
)

type config struct {
	sourceFilePath string
	msdelay int
	keystrokelimit int
}

const (
	limit int = 117
)

func main() {
	explain()
	kb, err := keybd_event.NewKeyBonding()
	conf := config{"./input.txt", 2500, 130} // Default values
	getInput(&conf)
	util.Check(err)
	s := bufio.NewScanner(os.Stdin)
	fmt.Println("PRESS ENTER TO YEET!!!")
	s.Scan()
	time.Sleep(3 * time.Second) // Give user time to switch applications, this also gives /dev/uinput time to prepare for writing
	// KBString(&kb, "|")
	TOSautopaster(&kb, &conf)
}

func getInput(v *config) {
	// Variables, structs
	s := bufio.NewScanner(os.Stdin); var err error

	fmt.Printf("Input delay between entries in milliseconds [%d]: ", v.msdelay); s.Scan()
	if s.Text() != "" {
		v.msdelay, err = strconv.Atoi(s.Text()); util.Check(err)
	}

	fmt.Printf("Input the number of characters to include in each entry [%d]: ", v.keystrokelimit); s.Scan()
	if s.Text() != "" {
		v.keystrokelimit, err = strconv.Atoi(s.Text()); util.Check(err)
	}
}

func TOSautopaster(k *keybd_event.KeyBonding, conf *config) {
	// Check if file exists
	if !util.FileExists(conf.sourceFilePath) {
		panic(fmt.Sprintf("%s FILE DOES NOT EXIST!", conf.sourceFilePath))
	}

	// Set variables

	// Read source file
	sourceFile, err := os.Open(conf.sourceFilePath)
	defer sourceFile.Close()
	util.Check(err)
	r := bufio.NewReader(sourceFile)

	// Begin the epicness; I've already pissed people off by reciting the Constitution on Town of Salem
	for {
		for i := 0; i < conf.keystrokelimit; i++ {
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
		time.Sleep(time.Duration(conf.msdelay) * time.Millisecond)

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
