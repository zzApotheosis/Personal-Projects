package util

import (
	"errors"
	"fmt"
	"os"
	"os/exec"
	"runtime"
	_ "syscall"
)

func GetOS() string {
	return runtime.GOOS
}

func Command(in string) (status int, err error) {
	switch GetOS() {
	//	case "windows":
	//		cmd := exec.Command("cmd", "/c", in)
	//		cmd.SysProcAttr = &syscall.SysProcAttr{HideWindow: true}
	//		cmd.Stdout = os.Stdout
	//		if err := cmd.Run(); err != nil {
	//			return -1, err
	//		}
	case "linux":
		cmd := exec.Command(in)
		cmd.Stdout = os.Stdout
		if err := cmd.Run(); err != nil {
			return -1, err
		}
	default:
		fmt.Println("Unsupported OS")
		return -1, errors.New("Test")
	}

	return 0, nil
}
