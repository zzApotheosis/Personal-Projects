
/*
 * Created by Steven Jennings (zzApotheosis) on 22/Apr/2017.
 * 
 * A lot of code was found by Googling this sort of thing, and ended up finding these sources:
 * https://www.mkyong.com/java/javamail-api-sending-email-via-gmail-smtp-example/
 * https://www.lifewire.com/what-are-the-gmail-smtp-settings-1170854
 * https://www.javatpoint.com/example-of-sending-email-using-java-mail-api
 */

import java.util.Properties;

import javax.mail.Authenticator;
import javax.mail.Message;
import javax.mail.MessagingException;
import javax.mail.PasswordAuthentication;
import javax.mail.Session;
import javax.mail.Transport;
import javax.mail.internet.InternetAddress;
import javax.mail.internet.MimeMessage;

public class SendMailTLS {

    private static final Password pswd = new Password();

    public static void main(String[] args) {
        // Define credentials, variables, etc.
        // Change as needed
        final String authUsername = "zzApotheosis@gmail.com";
        final String authPassword = pswd.getPassword();
        String host = "smtp.gmail.com";
        String port = "587";
        String auth = "true";
        String from = "Test@gmail.com";
        String to = "zzApotheosis@gmail.com";
        String[] toMany = { "test", "test2" };
        String subject = "Test Subject - Sending mail with Java!";
        String textBody = "Test message.\n\nIs this working?\n\nDo not reply.";

        // Make new properties instance
        Properties props = System.getProperties();
        props.put("mail.smtp.host", host);
        props.put("mail.smtp.port", port);
        props.put("mail.smtp.auth", auth);
        props.put("mail.smtp.starttls.enable", "true");

        // Authentication
        Session session = Session.getInstance(props, new Authenticator() {
            protected PasswordAuthentication getPasswordAuthentication() {
                return new PasswordAuthentication(authUsername, authPassword);
            }
        });

        // Compose and send message
        try {

            Message message = new MimeMessage(session);
            message.setFrom(new InternetAddress(from));
            message.setRecipients(Message.RecipientType.TO, InternetAddress.parse(to));
            message.setSubject(subject);
            message.setText(textBody);

            Transport.send(message);

            System.out.println("Done");

        } catch (MessagingException e) {
            throw new RuntimeException(e);
        }
    }
}