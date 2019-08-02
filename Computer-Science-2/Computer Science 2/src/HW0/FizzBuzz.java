/*
 * Created by Steven Jennings (zzApotheosis) on 25 August 2016.
 */

package HW0;

import java.util.InputMismatchException;
import java.util.Scanner;

public class FizzBuzz {

    public static void main(String[] args) {
        message();
        int limit = getLimit();
        int[] data = fizzBuzz(limit);
        printReport(data);
    }

    private static void message() {
        System.out.println("This program will display the answers to the game \"FizzBuzz\"" +
                "\nwhere the objective is to count from 1 in intervals of 1 and say \"Fizz\" when" +
                "\na number is divisible by 3, say \"Buzz\" when a number is divisible by 5," +
                "\nand say \"FizzBuzz\" when a number is divisible by both 3 and 5.\n");
    }

    private static int getLimit() {
        System.out.println("Enter a positive integer as the limit of the game's set of answers.");

        int num = 0;
        Scanner sc = new Scanner(System.in);

        try {
            num = sc.nextInt();
            if (num < 1 || num > 1000000) { //No child is ever going to count to 1,000,000 legitimately...
                System.out.println("User input is out of bounds. Setting limit to default 100.");
                num = 100;
            }
        } catch (InputMismatchException e) {
            System.out.println("Input is not a positive integer. Closing Program.");
            System.exit(-1);
        }

        sc.close(); //The only thing I wish IntelliJ had over Eclipse was the ability to catch potential resource leaks like a forgotten scanner close.
        return num;
    }

    private static int[] fizzBuzz(int limit) { //This could have been cleaner, but at least it's original
        int[] array = new int[5];
        array[4] = limit;
        boolean flag;

        System.out.println("\nAnswers:");
        for (int i = 1; i <= limit; i++) {
            flag = false;
            if (i % 3 == 0) {
                System.out.print("Fizz");
                array[0]++; //Fizz Counter
                flag = true;
            }
            if (i % 5 == 0) {
                System.out.print("Buzz");
                if (!flag) {
                    array[1]++; //Buzz Counter
                }
                if (flag) {
                    array[2]++; //FizzBuzz Counter
                    array[0]--;
                }
            }
            if (i % 3 != 0 && i % 5 != 0) {
                System.out.print(i);
                array[3]++; //Normal Counter
            }
            System.out.println();
        }

        return array;
    }

    private static void printReport(int[] data) {
        System.out.println("\nReport");
        System.out.println("----------------------------");
        System.out.println("Normal integer count: " + data[3]);
        System.out.println("\"Fizz\" count: " + data[0]);
        System.out.println("\"Buzz\" count: " + data[1]);
        System.out.println("\"FizzBuzz\" count: " + data[2]);
        System.out.println("Total count: " + data[4]);
    }
}