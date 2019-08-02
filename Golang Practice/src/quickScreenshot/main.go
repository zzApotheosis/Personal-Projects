/*
 * This program started from the following URL:
 * https://github.com/kbinani/screenshot
 *
 * Use "go get github.com/kbinani/screenshot" to fetch the required packages to
 * run this program.
 *
 * I changed it around a bit to suit my needs.
 *
 * I also learned that in order to hide the Windows cmd.exe window that launches
 * by default, the program must be built with this flag:
 * go build -ldflags "-H windowsgui"
 *
 * I'm guessing the -H means "hide".
 */

package main

import (
	"fmt"
	"image/png"
	_ "io/ioutil"
	"os"
	"time"

	"github.com/kbinani/screenshot"
)

func main() {
	// Check to see if output directory exists. If not, create it
	// var outDirectory string = "./Quick Screenshot/" // Commented out because IDE complained about it
	outDirectory := "./Quick Screenshot/"
	if _, err := os.Stat(outDirectory); os.IsNotExist(err) {
		os.Mkdir(outDirectory, os.ModePerm)
	}

	// Change to Quick Screenshot directory
	err := os.Chdir(outDirectory)
	if err != nil {
		panic(err)
	}

	// Get current time and date
	t := time.Now()
	ty := t.Year()
	tm := int(t.Month())
	td := t.Day()
	th := t.Hour()
	tmin := t.Minute()
	ts := t.Second()

	// Fetch screenshots
	n := screenshot.NumActiveDisplays()
	var filenames = make([]string, n)
	for i := 0; i < n; i++ {
		bounds := screenshot.GetDisplayBounds(i)

		img, err := screenshot.CaptureRect(bounds)
		if err != nil {
			panic(err)
		}

		fileName := fmt.Sprintf("%d,%02d,%02d-%02d,%02d,%02d-%d.png", ty, tm, td, th, tmin, ts, i)
		filenames[i] = fileName
		file, _ := os.Create(fileName)
		defer file.Close()
		png.Encode(file, img)
	}

	// // Check for existence of GIMP directory. If not, exit program
	// if _, err := os.Stat("C:/Program Files/GIMP 2/"); os.IsNotExist(err) {
	// 	return
	// }
	//
	// // // Get files in GIMP/bin directory
	// // files, err := ioutil.ReadDir("C:/Program Files/GIMP 2/bin/")
	// // if err != nil {
	// // 	fmt.Println(err)
	// // 	return
	// // }
	// // var f string
	// // for i := 0; i < len(files); i++ {
	// // 	f = files[i].Name()
	// // 	if f[:6] == "gimp-2" {
	// // 		break
	// // 	}
	// // }
	//
	// // Create command string
	// var command string
	//
	// // Define GIMP's command
	// gimpcmd := "gimp-2.10"
	//
	// // Append GIMP executable to command
	// command += gimpcmd + " "
	//
	// // Accumulate filenames
	// for i := 0; i < n; i++ {
	// 	command += filenames[i]
	// 	if i != n-1 {
	// 		command += " "
	// 	}
	// }
	//
	// // Launch GIMP
	// util.Command(command)
}
