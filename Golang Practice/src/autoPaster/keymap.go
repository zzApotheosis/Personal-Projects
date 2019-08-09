package main

import (
  "fmt"
  "github.com/micmonay/keybd_event"
)

func getKey(x rune) (bool, int) {
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
	case '`':
		return false, keybd_event.VK_SP1
	case '~':
		return true, keybd_event.VK_SP1
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
	case '!':
		return true, keybd_event.VK_1
	case '@':
		return true, keybd_event.VK_2
	case '#':
		return true, keybd_event.VK_3
	case '$':
		return true, keybd_event.VK_4
	case '%':
		return true, keybd_event.VK_5
	case '^':
		return true, keybd_event.VK_6
	case '&':
		return true, keybd_event.VK_7
	case '*':
		return true, keybd_event.VK_8
	case '(':
		return true, keybd_event.VK_9
	case ')':
		return true, keybd_event.VK_0
	case '-':
		return false, keybd_event.VK_SP2
	case '_':
		return true, keybd_event.VK_SP2
	case ' ':
		return false, keybd_event.VK_SPACE
	case '[':
		return false, keybd_event.VK_SP4
	case '{':
		return true, keybd_event.VK_SP4
	case ']':
		return false, keybd_event.VK_SP5
	case '}':
		return true, keybd_event.VK_SP5
	case '\\':
		return false, keybd_event.VK_SP8
	case '|':
		return true, keybd_event.VK_SP8
	case ';':
		return false, keybd_event.VK_SP6
	case ':':
		return true, keybd_event.VK_SP6
	case '\'':
		return false, keybd_event.VK_SP7
	case '"':
		return true, keybd_event.VK_SP7
	case ',':
		return false, keybd_event.VK_SP9
	case '<':
		return true, keybd_event.VK_SP9
	case '.':
		return false, keybd_event.VK_SP10
	case '>':
		return true, keybd_event.VK_SP10
	case '/':
		return false, keybd_event.VK_SP11
	case '?':
		return true, keybd_event.VK_SP11
	case '	':
		return false, keybd_event.VK_TAB
	case '\n':
    return false, keybd_event.VK_ENTER
	default:
		fmt.Println("WARNING: INVALID RUNE!")
	}
	return false, -1
}
