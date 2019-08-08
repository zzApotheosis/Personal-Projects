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

	KBString(&kb, "Test,yeet")
	KBString(&kb, "Test, yeet")
	KBString(&kb, "Test, yeet? WTF?")
}

func SlowKBString(k *keybd_event.KeyBonding, data string) {

}

func KBString(k *keybd_event.KeyBonding, data string) {
	for _, e := range data {
		writeKBEvent(k, e)
	}
}

func writeKBEvent(k *keybd_event.KeyBonding, x rune) {
	doShift, key := getKey(x)
	k.HasSHIFT(doShift)
	k.SetKeys(key)
	util.Check(k.Launching())
}

func getKey(x rune) (bool, int) {
	fmt.Println(x)
	switch x {
	case 'a':
		return false, keybd_event.VK_A
	case 'A':
		return true, keybd_event.VK_A
	case 'b':
		return false, keybd_event.VK_B
	case 'B':
		return true, keybd_event.VK_B
	case 'c':
		return false, keybd_event.VK_C
	case 'C':
		return true, keybd_event.VK_C
	case 'd':
		return false, keybd_event.VK_D
	case 'D':
		return true, keybd_event.VK_D
	case 'e':
		return false, keybd_event.VK_E
	case 'E':
		return true, keybd_event.VK_E
	case 'f':
		return false, keybd_event.VK_F
	case 'F':
		return true, keybd_event.VK_F
	case 'g':
		return false, keybd_event.VK_G
	case 'G':
		return true, keybd_event.VK_G
	case 'h':
		return false, keybd_event.VK_H
	case 'H':
		return true, keybd_event.VK_H
	case 'i':
		return false, keybd_event.VK_I
	case 'I':
		return true, keybd_event.VK_I
	case 'j':
		return false, keybd_event.VK_J
	case 'J':
		return true, keybd_event.VK_J
	case 'k':
		return false, keybd_event.VK_K
	case 'K':
		return true, keybd_event.VK_K
	case 'l':
		return false, keybd_event.VK_L
	case 'L':
		return true, keybd_event.VK_L
	case 'm':
		return false, keybd_event.VK_M
	case 'M':
		return true, keybd_event.VK_M
	case 'n':
		return false, keybd_event.VK_N
	case 'N':
		return true, keybd_event.VK_N
	case 'o':
		return false, keybd_event.VK_O
	case 'O':
		return true, keybd_event.VK_O
	case 'p':
		return false, keybd_event.VK_P
	case 'P':
		return true, keybd_event.VK_P
	case 'q':
		return false, keybd_event.VK_Q
	case 'Q':
		return true, keybd_event.VK_Q
	case 'r':
		return false, keybd_event.VK_R
	case 'R':
		return true, keybd_event.VK_R
	case 's':
		return false, keybd_event.VK_S
	case 'S':
		return true, keybd_event.VK_S
	case 't':
		return false, keybd_event.VK_T
	case 'T':
		return true, keybd_event.VK_T
	case 'u':
		return false, keybd_event.VK_U
	case 'U':
		return true, keybd_event.VK_U
	case 'v':
		return false, keybd_event.VK_V
	case 'V':
		return true, keybd_event.VK_V
	case 'w':
		return false, keybd_event.VK_W
	case 'W':
		return true, keybd_event.VK_W
	case 'x':
		return false, keybd_event.VK_X
	case 'X':
		return true, keybd_event.VK_X
	case 'y':
		return false, keybd_event.VK_Y
	case 'Y':
		return true, keybd_event.VK_Y
	case 'z':
		return false, keybd_event.VK_Z
	case 'Z':
		return true, keybd_event.VK_Z
	case '1':
		return false, keybd_event.VK_1
	case '2':
		return false, keybd_event.VK_2
	case '3':
		return false, keybd_event.VK_3
	case '4':
		return false, keybd_event.VK_4
	case '5':
		return false, keybd_event.VK_5
	case '6':
		return false, keybd_event.VK_6
	case '7':
		return false, keybd_event.VK_7
	case '8':
		return false, keybd_event.VK_8
	case '9':
		return false, keybd_event.VK_9
	case '0':
		return false, keybd_event.VK_0
	case ' ':
		return false, keybd_event.VK_SPACE
	case '?':
		return true, keybd_event.VK_SP11
	case '!':
		return true, keybd_event.VK_1
	case ',':
		return false, keybd_event.VK_SP9
	case '.':
		return false, keybd_event.VK_SP10
	default:
		fmt.Println("WARNING: INVALID RUNE!")
	}
	return false, -1
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
