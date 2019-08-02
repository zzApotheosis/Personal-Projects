package providedCode;

import java.text.SimpleDateFormat;
import java.util.Date;
 
public class DateDifferenceExample {
 
   static Toolkit tools = new Toolkit();
 
	public static void main(String[] args) {
 
		String dateStart = "01/14/2014 22:29:58";
		String dateStop  = "01/15/2016 22:44:59";
 
		//HH converts hour in 24 hours format (0-23), day calculation
		SimpleDateFormat format = new SimpleDateFormat("MM/dd/yyyy HH:mm:ss");
 
		Date d1 = null;
		Date d2 = null;
 
		try {
			d1 = format.parse(dateStart);
			d2 = format.parse(dateStop);
 
			// Calculate the difference in milliseconds
			long diff = d2.getTime() - d1.getTime();
 
			int diffSeconds = (int) (diff / 1000 % 60);
			int diffMinutes = (int) (diff / (60 * 1000) % 60);
			int diffHours   = (int) (diff / (60 * 60 * 1000) % 24);
			int diffDays    = (int) (diff / (24 * 60 * 60 * 1000));
 
			System.out.println("The difference between " + 
               dateStart + " and " + dateStop + " is \n" +               
               diffDays + " day" + tools.giveMeAnS(diffDays) + ", " +
			      diffHours + " hour" + tools.giveMeAnS(diffHours) + ", " +
			      diffMinutes + " minute" + tools.giveMeAnS(diffMinutes) + ", and " +
			      diffSeconds + " second" + tools.giveMeAnS(diffSeconds) + ".");
 
		} catch (Exception e) {
			e.printStackTrace();
		}
 
	}
 
}