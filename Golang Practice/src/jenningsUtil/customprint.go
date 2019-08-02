package util

import (
	"fmt"
	"strings"
	"time"
)

func Println(a ...interface{}) (n int, err error) {
	var hour, min, sec int = time.Now().Clock()
	return fmt.Println(prefixTimeFormat(hour, min, sec)+" <] ", strings.Trim(fmt.Sprint(a), "[]"))
}

func Print(a ...interface{}) (n int, err error) {
	var hour, min, sec int = time.Now().Clock()
	return fmt.Print(prefixTimeFormat(hour, min, sec)+" <] ", strings.Trim(fmt.Sprint(a), "[]"))
}

// TODO
func Printf(a ...interface{}) (n int, err error) {
	var hour, min, sec int = time.Now().Clock()
	return fmt.Printf(prefixTimeFormat(hour, min, sec)+" <] ", a)
}

func prefixTimeFormat(hr, mn, sc int) (str string) {
	return fmt.Sprintf("%d:%d:%d", hr, mn, sc)
}
