/*
 * Created by Steven Jennings on 2020 July 29.
 */

package main

import (
	"flag"
	"fmt"
	"gopkg.in/gomail.v2"
	"io/ioutil"
	util "jenningsUtil"
	_ "log"
	_ "net/smtp"
	"os"
)

func main() {
	var password string
	d
	sendGomail(data)
}

func getPassword(file string) string {
	// Define method variables
	var data []byte
	var err error

	// Check if file exists
	if !fileExists(file) {
		fmt.Fprintf(os.Stderr, "File does not exist: %s\n", file)
		return ""
	}

	// Read file
	data, err = ioutil.ReadFile(file)
	util.Check(err)

	// Done
	return string(data)
}

func sendGomail(in map[string]string) {
	// Define method variables
	var from, password, to, subject, messageType, message, attachment string
	var keyExists bool

	// Check for critical data; populate
	if from, keyExists = in["username"]; !keyExists {
		panic("Cannot send email with no username!")
	}
	if password, keyExists = in["password"]; !keyExists {
		panic("Cannot send email with no password!")
	}
	if to, keyExists = in["target"]; !keyExists {
		panic("Cannot send email with no destination address!")
	}
	if subject, keyExists = in["subject"]; !keyExists {
		subject = "No Subject"
	}
	if messageType, keyExists = in["messagetype"]; !keyExists {
		messageType = "text/plain"
	}
	if message, keyExists = in["message"]; !keyExists {
		message = ""
	}
	if attachment, keyExists = in["attachment"]; !keyExists {
		attachment = ""
	}

	// Begin building gomail
	m := gomail.NewMessage()
	m.SetHeader("From", from)
	m.SetHeader("To", to)
	m.SetHeader("Subject", subject)
	m.SetBody(messageType, message)
	if fileExists(attachment) {
		m.Attach(attachment)
	}

	// Create dialer
	d := gomail.NewDialer("smtp.gmail.com", 587, from, password)

	// Send gomail!
	if err := d.DialAndSend(m); err != nil {
		panic(err)
	}
}

func fileExists(filename string) bool {
	info, err := os.Stat(filename)
	if os.IsNotExist(err) {
		return false
	}
	return !info.IsDir()
}
