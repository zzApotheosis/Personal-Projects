package main

import (
	"fmt"
	_ "github.com/atotto/clipboard"
	"github.com/micmonay/keybd_event"
	util "jenningsUtil"
	"time"
	"reflect"
)

func main() {
	kb, err := keybd_event.NewKeyBonding(); util.Check(err)
	fmt.Println(reflect.TypeOf(kb).String())

	// Important pause for Linux systems
	if util.GetOS() == "linux" {
		time.Sleep(2 * time.Second)
	}

	// kb.SetKeys(keybd_event.VK_A, keybd_event.VK_B, keybd_event.VK_X)

	// kb.HasSHIFT(true)

	// err = kb.Launching()
	// util.Check(err)

	// testEvent([]byte{1, 2, 3, 4})

	execKBEvent(&kb, 'A')
}

func execKBEvent(k *keybd_event.KeyBonding, x rune) {
	kb, err := keybd_event.NewKeyBonding(); util.Check(err)
	if util.GetOS() == "linux" {
		time.Sleep(2 * time.Second)
	}
	kb.SetKeys(getKey(x))
	kb.HasSHIFT(true)
	util.Check(kb.Launching())
}

func getKey(x rune) int {
	switch x {
	case 'a':
		return keybd_event.VK_A
	case 'A':
		return keybd_event.VK_A
	}
	return -1
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
