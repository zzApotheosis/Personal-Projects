/*
 * Created by Steven Jennings (zzApotheosis) on 13 September 2016.
 *
 * This is problem 1.1.3 in the textbook.
 */

package HW1;

import java.util.Scanner;

public class Problem1 {
    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);
        int a = sc.nextInt();
        int b = sc.nextInt();
        int c = sc.nextInt();
        sc.close();

        if (a == b && b == c) {
            System.out.println("equal");
        } else {
            System.out.println("not equal");
        }
    }
}