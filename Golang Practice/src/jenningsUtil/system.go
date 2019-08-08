package util

import (
	"runtime"
)

// GetOS yeet
func GetOS() string {
	return runtime.GOOS
}
