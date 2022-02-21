/*
 * Created by Steven Jennings on 2020 February 09.
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
	// Define method variables
	var data map[string]string
	var arg_target, arg_subject, arg_message, arg_attachment string

	// Fetch arguments
	flag.StringVar(&arg_target, "target", "zzApotheosis@gmail.com", "The target email address")
	flag.StringVar(&arg_subject, "subject", "Subject", "The Subject of the email")
	flag.StringVar(&arg_message, "message", "", "The body of the email")
	flag.StringVar(&arg_attachment, "attachment", "", "The path to a file to attach")
	flag.Parse()

	// Populate data
	data = make(map[string]string)
	data["username"] = "zzApotheosis@gmail.com"
	data["password"] = getPassword("/root/zzApotheosis_gmail_app_password.txt")
	data["target"] = arg_target
	data["subject"] = arg_subject
	data["messageType"] = "text/plain"
	data["message"] = arg_message
	data["attachment"] = arg_attachment

	// Send message
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
