import java.util.Scanner;

import javax.mail.internet.MimeMessage.RecipientType;

import org.simplejavamail.email.Email;
import org.simplejavamail.mailer.Mailer;
import org.simplejavamail.mailer.config.TransportStrategy;

import com.pi4j.io.gpio.GpioController;
import com.pi4j.io.gpio.GpioFactory;
import com.pi4j.io.gpio.GpioPinDigitalInput;
import com.pi4j.io.gpio.GpioPinDigitalOutput;
import com.pi4j.io.gpio.PinPullResistance;
import com.pi4j.io.gpio.PinState;
import com.pi4j.io.gpio.RaspiPin;
import com.pi4j.util.Console;

public class Test {

	private static Password pswd = new Password();

	private static final GpioController gpio = GpioFactory.getInstance();

	private static final GpioPinDigitalInput myButton = gpio
			.provisionDigitalInputPin(RaspiPin.GPIO_00,
					PinPullResistance.PULL_DOWN);
	private static final GpioPinDigitalOutput out = gpio
			.provisionDigitalOutputPin(RaspiPin.GPIO_02, PinState.LOW);

	private static int state = 0;

	private static Scanner sc = new Scanner(System.in);

	/**
	 * @param args
	 */
	public static void main(String[] args) throws InterruptedException {
		final Console console = new Console();
		console.promptForExit();

		myButton.setShutdownOptions(true, PinState.LOW);
		out.setShutdownOptions(true, PinState.LOW);

		int i = 0;
		while (console.isRunning()) {
			if (myButton.isHigh()) {
				out.low();
				sendEmail();
			}
			i++;
			if (i < 49000) {
				out.low();
			} else if (i >= 49000 && i < 50000) {
				out.high();
			} else if (i >= 50000) {
				i = 0;
			}
		}

		gpio.shutdown();

	}

	private static void sendEmail() {
		// Set information, change as needed
		String hostUsername = "zzApotheosis@gmail.com";
		String hostPassword = pswd.getPassword0();
		String fromName = "Raspberry Pi";
		String fromAddress = "Rasp@berry.pi";
		System.out.print("Set Recipient Name: ");
		String toName0 = sc.nextLine();
		System.out.print("Set Recipient Address: ");
		String toAddress0 = sc.nextLine();
		// String toAddress0 = pswd.getAddr0(); // Sends to my phone number
		// String toName1 = "Jake";
		// String toAddress1 = "a7xstryker@gmail.com";
		System.out.print("Set Email Subject: ");
		String subject = sc.nextLine();
		System.out.print("Write Message: ");
		String body = sc.nextLine();
		System.out.println("Press the button to send email. Wait 50 LED strobes to cancel.");
		int i = 0;
		int j = 0;
		while (myButton.getState() == PinState.LOW) {
			i++;
			if (i < 4900) {
				out.low();
			} else if (i >= 4900 && i < 5000) {
				out.high();
			} else if (i >= 5000) {
				i = 0;
				j++;
			}
			if (j >= 50) {
				System.out.println("Email canceled.\n");
				return; // Cancel send.
			}
		}

		// Compose email
		Email email = new Email();
		email.setFromAddress(fromName, fromAddress);
		email.addRecipient(toName0, toAddress0, RecipientType.TO);
		// email.addRecipient(toName1, toAddress1, RecipientType.CC);
		email.setSubject(subject);
		email.setText(body);

		// Attempt send
		try {
			out.high();
			new Mailer("smtp.gmail.com", new Integer(587), hostUsername,
					hostPassword, TransportStrategy.SMTP_TLS).sendMail(email);
			System.out.println("Mail sent!\n");
			out.low();
		} catch (Exception e) {
			if (out.isHigh()) {
				out.low();
			}
			System.out.println("Mail not sent.\n");
			e.printStackTrace();
		}
	}

}
