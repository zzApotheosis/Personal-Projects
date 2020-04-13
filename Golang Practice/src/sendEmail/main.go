/*
 * Created by Steven Jennings on 2020 February 09.
 */

package main

import (
	"flag"
	"fmt"
	"gopkg.in/gomail.v2"
	util "jenningsUtil"
	"log"
	"net/smtp"
	"os"
	"io/ioutil"
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

/*
 * Sample email with standard Go libraries
 */
func sendEmail(in map[string]string) {
	// Define method variables
	var from, pass, to, subject, message, attachment string
	var keyExists bool
	var payload string
	var err error

	// Determine data
	from, keyExists = in["username"]
	if !keyExists {
		panic("Cannot send email with no username!")
	}
	pass, keyExists = in["password"]
	if !keyExists {
		panic("Cannot send email with no password!")
	}
	to, keyExists = in["target"]
	if !keyExists {
		panic("Cannot send email with no destination address!")
	}
	subject, keyExists = in["subject"]
	if !keyExists {
		subject = "No Subject"
	}
	message, keyExists = in["message"]
	if !keyExists {
		return // There's no message, so no reason to send anything
		if !keyExists {
		}
		_ = attachment // Right now, I don't need attachments. TODO: implement later

		// Construct email payload
		payload =
			"From: " + from + "\n" +
				"To: " + to + "\n" +
				"Subject: " + subject + "\n\n" +
				message

		// Send email over SMTP
		err = smtp.SendMail("smtp.gmail.com:587",
			smtp.PlainAuth("", from, pass, "smtp.gmail.com"),
			from, []string{to}, []byte(payload))

		// Check for errors
		if err != nil {
			log.Printf("SMTP Error: %s", err)
		}

		// Done
		log.Print("Email sent to " + to)
	}
}

func gomailSample() {
	m := gomail.NewMessage()
	m.SetHeader("From", "zzApotheosis@gmail.com")
	m.SetHeader("To", "zzApotheosis@gmail.com")
	// m.SetAddressHeader("Cc", "dan@example.com", "Dan")
	m.SetHeader("Subject", "Hello!")
	// m.SetBody("text/html", "Hello <b>Bob</b> and <i>Cora</i>!")
	m.SetBody("text/plain", "Big ligma")
	m.Attach("/home/zzapotheosis/Desktop/test_attachment.txt")

	d := gomail.NewDialer("smtp.gmail.com", 587, "zzApotheosis@gmail.com", "jkovzowhjzkousin")

	// Send the email to Bob, Cora and Dan.
	if err := d.DialAndSend(m); err != nil {
		panic(err)
	}
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
