import javax.mail.internet.MimeMessage.RecipientType;

import org.simplejavamail.email.Email;
import org.simplejavamail.mailer.Mailer;
import org.simplejavamail.mailer.config.TransportStrategy;

public class MyEmail {
	// Object to hold private password information
	private static Password pswd = new Password();

	public MyEmail() {
		// Constructor - nothing needed
	}

	public void notification(String msg, boolean sms) {
		// Set information, change as needed
		String hostUsername = "zzApotheosis@gmail.com";
		String hostPassword = pswd.getPassword0();
		String fromName = "Raspberry Pi";
		String fromAddress = "Rasp@berry.pi";
		String toName0 = "Steven";
		String toAddress0 = "zzApotheosis@gmail.com";
		String toName1 = "Jake";
		String toAddress1 = "a7xstryker@gmail.com";
		String subject = "Raspberry Pi Notification";
		String body = msg;

		// Compose email
		Email email = new Email();
		email.setFromAddress(fromName, fromAddress);
		email.addRecipient(toName0, toAddress0, RecipientType.TO);
//		email.addRecipient(toName1, toAddress1, RecipientType.CC);
		email.setSubject(subject);
		email.setText(body);

		// Instantiate mailer
		Mailer mailer = new Mailer("smtp.gmail.com", new Integer(587), hostUsername, hostPassword, TransportStrategy.SMTP_TLS);

		// Attempt email send
		try {
			mailer.sendMail(email);
			RFIDProject.customPrint("Mail sent!");
			if (!sms) { // Keeps console clean-looking
				System.out.println();
			}
		} catch (Exception e) {
			RFIDProject.customPrint("Mail failed to send.");
			if (!sms) { // Keeps console clean-looking
				System.out.println();
			}
			e.printStackTrace();
		}

		if (sms) {
			// Compose SMS notification
			Email smsMsg = new Email();
			smsMsg.setFromAddress(fromName, fromAddress);
			smsMsg.addRecipient("Steven", pswd.getAddr0(), RecipientType.TO);
			smsMsg.setSubject(subject);
			smsMsg.setText("Check your email for details.");

			// Attempt SMS send
			try {
				mailer.sendMail(smsMsg);
				RFIDProject.customPrint("SMS sent!\n");
			} catch (Exception e) {
				RFIDProject.customPrint("SMS failed to send.\n");
				e.printStackTrace();
			}
		}

	}

	// In case someone wants to be notified without including SMS boolean parameter
	public void notification(String msg) {
		notification(msg, false);
	}
}