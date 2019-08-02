package util

import (
	"fmt"
	"os"
	"os/exec"
)

func TerminalClear() {
	// Clear terminal
	switch GetOS() {
	case "windows":
		winClear()
	case "linux":
		linClear()
	default:
		fmt.Println("Unsupported OS.")
		return
	}
}

func winClear() {
	// Clear Windows PowerShell
	cmd := exec.Command("cmd", "/c", "cls")
	cmd.Stdout = os.Stdout
	if err := cmd.Run(); err != nil {
		fmt.Println(err)
		return
	}
}

func linClear() {
	// Clear Linux terminal
	cmd := exec.Command("clear")
	cmd.Stdout = os.Stdout
	if err := cmd.Run(); err != nil {
		fmt.Println(err)
		return
	}
}
