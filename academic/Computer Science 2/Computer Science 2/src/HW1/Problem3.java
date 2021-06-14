/*
 * Created by Steven Jennings (zzApotheosis) on 13 September 2016.
 *
 * This is problem 1.2.12 in the textbook.
 */

package HW1;

import java.util.Calendar;
import java.util.HashMap;
import java.util.Map;

public class Problem3 {

    private static Map<Integer,String> weekDays = new HashMap<Integer,String>();

    public static void main(String[] args) {
        setDays();

        int Year = 2016;
        int Month = 2;
        int Day = 7; //When Von Miller totally harrassed Cam Newton all day.

        Calendar cal = Calendar.getInstance();
        cal.set(Year, Month, Day);
        int dayOfWeek = cal.get(Calendar.DAY_OF_WEEK);

        System.out.println(weekDays.get(dayOfWeek));

    }

    private static void setDays() {
        weekDays.put(1, "Sunday");
        weekDays.put(2, "Monday");
        weekDays.put(3, "Tuesday");
        weekDays.put(4, "Wednesday");
        weekDays.put(5, "Thursday");
        weekDays.put(6, "Friday");
        weekDays.put(7, "Saturday");
    }
}