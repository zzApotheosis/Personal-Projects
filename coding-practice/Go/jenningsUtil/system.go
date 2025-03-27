package util

import (
	"runtime"
	"os"
)

// GetOS yeet
func GetOS() string {
	return runtime.GOOS
}

// Checks if a file exists, and also ensures it is not a directory
func FileExists(path string) bool {
	info, err := os.Stat(path)
	if os.IsNotExist(err) {
		return false
	}
	return !info.IsDir()
}
