/*
 * Created by Steven Jennings (zzApotheosis) on 22/Apr/2017.
 * 
 * This is an example implementation of the awesome API "Simple Java Mail", which utilizes the standard javax.mail library in itself.
 * 
 * By the way, let it be known that recent versions of Simple Java Mail also utilizes the open-source library slf4j, which can be found here:
 * https://www.slf4j.org/download.html
 * 
 * The two .jar files needed are the "api" and "simple" files. From what I understand, the api .jar is for the access to the slf4j library,
 * and the "simple" .jar is what the API uses for its logging functionality. Don't quote me on that though; that's just my understanding.
 * 
 * More information can be found at the home page: https://www.slf4j.org/
 */

import javax.mail.internet.MimeMessage.RecipientType;

import org.simplejavamail.email.Email;
import org.simplejavamail.mailer.Mailer;
import org.simplejavamail.mailer.config.TransportStrategy;

public class SendEmailWithSJM {

    private static Password pswd = new Password();

    public static void main(String[] args) {
        // Define parameters
        // Basic email information
        String fromName = "Person0";
        String fromAddress = "address0@what.ever";
        String toName = "Steven Jennings";
        String toAddress = "zzApotheosis@gmail.com";
        // String[] toNames = new String[3]; // To send to multiple recipients (Names)
        // String[] toAddresses = new String[3]; // To send to multiple recipients (Addresses)
        String subject = "Test subject";
        String body = "This is a test email!";
        // Mailer information
        String host = "smtp.gmail.com";
        Integer port = new Integer(587); // TLS = 587, SSL = 465 for Gmail
        String hostUsername = "zzApotheosis@gmail.com";
        String password = pswd.getPassword();
        TransportStrategy type = TransportStrategy.SMTP_TLS; // For SSL: TransportStrategy.SMTP_SSL

        // Compose email
        Email email = new Email();
        email.setFromAddress(fromName, fromAddress);
        email.addRecipient(toName, toAddress, RecipientType.TO);
        email.setSubject(subject);
        email.setText(body);

        // Attempt send
        Mailer inHouseMailer = new Mailer(host, port, hostUsername, password, type);
        try {
            inHouseMailer.sendMail(email);
            System.out.println("Mail sent!");
        } catch (Exception e) {
            System.out.println("Mail failed to send.");
            e.printStackTrace();
        }
    }
}