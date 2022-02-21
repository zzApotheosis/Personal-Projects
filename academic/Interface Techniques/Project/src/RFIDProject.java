/*
 * Created by Steven Jennings on 19 April 2017.
 * Copyright 2017 Steven Jennings. All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *     http://www.apache.org/licenses/LICENSE-2.0
 * 
 * (See j8header-3b.png for pin layout)
 * Pins denoted by asterisk (*) are not included in standard SPI implementation.
 * Pin layout for Raspberry Pi 3 - Model B:
 * SDA   <->  Pin 24 (GPIO 10)
 * SCK   <->  Pin 23 (GPIO 14)
 * MOSI  <->  Pin 19 (GPIO 12)
 * MISO  <->  Pin 21 (GPIO 13)
 * IRQ*  <->  Pin 16 (GPIO 04)
 * GND   <->  Pin 9
 * RST*  <->  Pin 22 (GPIO 06)
 * 3.3V  <->  Pin 1
 * 
 * Custom pins:
 * out0  <->  Pin 11 (GPIO 00)
 * 
 * UIDs used in this project:
 * Blue card (Authorized key): A58B956AD1
 * White Card: BA5DF10610
 * White Card: AA16F20648
 * 
 * To run this program, export it as a runnable jar (on Eclipse). Make sure to include any referenced libraries. Then run this command:
 * sudo java -jar <exportedfile>.jar
 * (For Unix, Linux, etc. systems. Obviously should be a Raspberry Pi compatible OS like Raspbian.)
 * 
 * Due to some bugs in Pi4J/wiringPi, you may need to include this parameter when launching the program:
 * -Dpi4j.linking=dynamic
 * to make the full command:
 * sudo java -Dpi4j.linking=dynamic -jar <exportedfile>.jar
 * 
 * Alternatively, you may use the built-in tools that Pi4J has to offer. Try compiling the main Java class with:
 * pi4j -c <classname>.java
 * and run it with:
 * sudo pi4j <classname>
 * 
 * A large portion of the code for this project is based on code from GitHub user LiangYuen's repository:
 * https://github.com/LiangYuen/Pi4j-RC522
 * At this time, 29 April 2017, the source code is unlicensed.
 */

import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.Scanner;

import com.pi4j.io.gpio.GpioController;
import com.pi4j.io.gpio.GpioFactory;
import com.pi4j.io.gpio.GpioPinDigitalInput;
import com.pi4j.io.gpio.GpioPinDigitalOutput;
import com.pi4j.io.gpio.PinPullResistance;
import com.pi4j.io.gpio.PinState;
import com.pi4j.io.gpio.RaspiPin;
import com.pi4j.util.Console;

public class RFIDProject {
	// Instantiate Console
	private static final Console console = new Console();

	// Create message to run on separate thread.
	private static volatile String message = "";

	public static void main(String[] args) throws InterruptedException, IOException {
		// Instantiate project objects
		RFID rfid = null;
		final MyEmail mail = new MyEmail();
		final GpioController gpio = GpioFactory.getInstance();
		MFRC522 values = new MFRC522();
		Scanner sc = new Scanner(System.in);

		// Set GPIO pins
		final GpioPinDigitalOutput out0 = gpio.provisionDigitalOutputPin(RaspiPin.GPIO_00, "LED", PinState.LOW); // Motorized door lock
		final GpioPinDigitalOutput RST = gpio.provisionDigitalOutputPin(RaspiPin.GPIO_06, "Reset", PinState.HIGH); // Reset idles high
		final GpioPinDigitalInput IRQ = gpio.provisionDigitalInputPin(RaspiPin.GPIO_04, "Interrupt", PinPullResistance.PULL_DOWN); // Interrupts suck on this thing
		out0.setShutdownOptions(true, PinState.LOW);
		RST.setShutdownOptions(true, PinState.HIGH);
		IRQ.setShutdownOptions(true, PinState.LOW);

		// Hardware reset happens on RST low. Normal operations on RST high.
		RST.low();
		Thread.sleep(250);
		RST.high();
		Thread.sleep(250);
		rfid = new RFID(); // Instantiate RFID() after the hardware reset

		// Define important values
		int back_bits[] = new int[1];
		String UID;
		byte tagid[] = new byte[5];
		int i, status;
		byte blockaddress = 8; // Any value between 0-63, or just use sector/block combination
		byte sector = 15, block = 2;

		// Define keys. Default is all 0xFF
		byte[] keyA = new byte[] { (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF };
		byte[] keyB = new byte[] { (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF };

		// Print program title/header
		console.title("<-- Interface Techniques Project -->", "Program to interface with RFID reader");
		console.promptForExit(); // Run until Ctrl-C

		// Print welcome message
		welcome();

		// Miscellaneous values
		int unauth = 0;
		String[] UIDHistory = new String[50];
		String[] timestamps = new String[UIDHistory.length];
		boolean cardPresent = false;
		int state = 0;
		for (int j = 0; j < UIDHistory.length; j++) {
			UIDHistory[j] = "00000000"; // Initialize all indexes to blank UIDs
			timestamps[j] = "TIME"; // Initialize all timestamps to "TIME"
		}
		byte[] data = new byte[16];

		// Main loop
		while (console.isRunning()) {
			status = values.MI_ERR; // Initialize status to MI_ERR (which is actually just 2; MI_OK is 0)

			// Wait until card is detected
			while (status != values.MI_OK) {
				status = rfid.Select_MiFareOne(tagid);
				Thread.sleep(50); // Polling at 20Hz
			}

			if (status == values.MI_OK) {
				cardPresent = true;
				UID = rfid.bytesToHex(tagid);

				// Cycle UID History
				for (int j = UIDHistory.length - 1; j >= 0; j--) {
					if (j != 0) {
						UIDHistory[j] = UIDHistory[j - 1];
						timestamps[j] = timestamps[j - 1];
					} else {
						UIDHistory[j] = UID;
						timestamps[j] = getTimeStamp();
					}
				}

				// Check for authorized card
				if (check(UID)) {
					customPrint("Welcome home!");

					if (unauth >= 3) {
						customPrint("Number of unauthorized attempts prior: " + unauth);
					}
					System.out.println(); // Extra println just to space out console text
					unauth = 0;

					// Unlock door
					new Thread(new Runnable() {
						public void run() {
							out0.high();
							try {
								Thread.sleep(5000); // 5 seconds
							} catch (InterruptedException e) {
								e.printStackTrace();
							}
							out0.low();
						}
					}).start();
				} else {
					customPrint("Unauthorized UID: " + UID);
					unauth++;

					/*
					 * If the reader detects 3 consecutive unauthorized UIDs,
					 * it will notify me by email, complete with UID history,
					 * up to the past (size of UIDHistory) UIDs.
					 * It will only notify at certain thresholds to control email spam.
					 * Notify by email at 3 and 10 unauthorized attempts.
					 * Notify by email at every 25th unauthorized attempt.
					 */
					if (unauth == 3 || unauth == 10 || unauth % 25 == 0) {
						message = "Unauthorized access attempt at RFID Reader.\n\nThere have been " + unauth + " consecutive unauthorized access attempts.\n\n<-----UID History----->\n";
						for (int j = 0; j < UIDHistory.length; j++) {
							if (j != UIDHistory.length - 1) {
								message += "Time: " + timestamps[j] + "\nUID: " + UIDHistory[j] + "\n\n";
							} else {
								message += "Time: " + timestamps[j] + "\nUID: " + UIDHistory[j]; // Set the last lines without the double carriage-return at the end
							}
						}

						/*
						 *  Run the notification send on a separate thread
						 *  for efficient operation. This allows the main
						 *  system to keep running without pausing to send
						 *  the notification(s).
						 */
						new Thread(new Runnable() {
							public void run() {
								mail.notification(message, true);
							}
						}).start();
					}

					if (unauth >= 3) { // Each further unauthorized attempt locks the system for an additional 15 seconds
						customPrint("System locked for " + (30 + unauth * 15) + " seconds.");
						Thread.sleep((30 + unauth * 15) * 1000);
					}

					customPrint("Try another card.\n");
				}

				/*
				 * Wrote some code to automatically detect when a detected card leaves the field.
				 * There's probably an easier way to do this through the RFID reader's datasheet,
				 * but I'm lazy.
				 */
				while (cardPresent) {
					if (state == 0 && status == values.MI_OK) {
						state = 1;
						status = rfid.Select_MiFareOne(tagid);
					} else if (state == 1 && status != values.MI_OK) {
						state = 0;
						status = rfid.Select_MiFareOne(tagid);
					} else {
						cardPresent = false;
						state = 0;
					}
					Thread.sleep(50); // Polling at 20Hz
				}
			} else {
				UID = "00000000";
			}
		}

		// Handle objects before system exit
		console.emptyLine();
		gpio.shutdown();
		sc.close();
	}

	private static boolean check(String in) {
		// Add more valid UIDs here if needed
		if (in.equals("A58B956AD1"))
			return true;
		return false;
	}

	private static String getTimeStamp() {
		return new SimpleDateFormat("yyyy/MM/dd - HH:mm:ss").format(new java.util.Date());
	}

	private static String getConsoleTime() {
		return new SimpleDateFormat("HH:mm:ss >>> ").format(new java.util.Date());
	}

	protected static void customPrint(String in) {
		System.out.println(getConsoleTime() + in);
	}

	private static void welcome() {
		printCopyright();
		System.out.println();
		customPrint("This program secures a lock.");
		customPrint("For demonstration purposes, this lock will secure a door.");
		customPrint("A specific NFC card is the key to the door.");
		customPrint("The program will notify the master by email (must be configured)");
		customPrint("if it detects consecutive unauthorized access attempts.");
		System.out.println();
		customPrint("Enjoy!");
		System.out.println();
	}

	private static void printCopyright() {
		System.out.println("Copyright 2017 Steven Jennings. All rights reserved.");
		System.out.println("Licensed under the Apache License, Version 2.0");
	}
}
