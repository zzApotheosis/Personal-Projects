/*
 * Created by Steven Jennings (zzApotheosis) on 13 September 2016.
 *
 * This is problem 1.2.6 in the textbook.
 */

package HW1;

import java.util.Arrays;

public class Problem2 {
    public static void main(String[] args) {
        String s = "hello world";
        String t = "worldhello ";
        char[] charArray1 = s.toCharArray();
        char[] charArray2 = t.toCharArray();
        boolean circular = false;

        if (charArray1.length != charArray2.length) {
            System.out.println("The two strings do not have matching lengths. They are not circular shifts of one another.");
        } else {
            for (int i = 0; i < charArray1.length; i++) {
                if (Arrays.equals(charArray1, charArray2)) {
                    circular = true;
                    break;
                }
                charArray2 = shift(charArray2);
            }

            if (circular) {
                System.out.println("The two strings are circular shifts of one another.");
            } else {
                System.out.println("The two strings are not circular shifts of one another.");
            }
        }

    }

    private static char[] shift(char[] a) {
        char[] newArray = new char[a.length];
        for (int i = 0; i < a.length - 1; i++) {
            newArray[i] = a[i + 1];
        }
        newArray[a.length - 1] = a[0];
//        System.out.println(newArray); //Debug info
        return newArray;
    }
}