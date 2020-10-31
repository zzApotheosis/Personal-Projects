/*
 * Created by Steven Jennings on 20 September 2017.
 *
 * Program #5, CS 1050, Section 3
 *
 * Assignment description:
 * This program calculates gross pay, savings, and investment
 * using methods and method calls.
 *
 * System Information:
 * Operating System: Arch Linux (amd64)
 * Official Oracle JDK Version: Version 8, Update 152 (build 1.8.0_152-b16)
 * IDE Version: IntelliJ IDEA 2017.2.5
 *
 * Extra notes:
 * This is essentially a copy of my Main_02 class, because I already
 * used methods and method calls in that assignment.
 *
 * Please note: I use the Apache License 2.0 because it is a
 * permissive free software license. I do this because all of my
 * code is publicly available on my GitHub repository for the world to see.
 * Licensing my work under the Apache License 2.0 does not explicitly
 * mean that I use any works from the Apache Software Foundation as
 * part of this program.
 *
 * Observe this excerpt from Wikipedia:
 * "[...] many non-[Apache Software Foundation] projects
 * are also using the [Apache License v2.0]."
 *
 * Vocabulary word of choice:
 * Petrichor
 * (n) The earthy scent produced when rain falls on dry soil.
 * (n) A pleasant smell that frequently accompanies the first rain after
 * a long period of warm, dry weather.
 *
 * Quote of choice:
 * "I can't complain, but sometimes I still do." - Joe Walsh
 *
 *******************************************************************************
 *
 * Copyright 2017 Steven Jennings
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package program05;

import providedCode.Toolkit;

import java.util.InputMismatchException;
import java.util.Scanner;

/**
 * The main class for program 5.
 * <p>
 * This program calculates gross pay, savings, and investment
 * using methods and method calls.
 *
 * @author Steven Jennings
 * @version 0.0.2
 * @since 20 September 2017
 */
public class StevenJennings_3_05 {

    /**
     * The main method. The whole program is executed here.
     *
     * @param args Unused.
     */
    public static void main(String[] args) {
        /*
         * This is all the data for the assignment, with each variable
         * stored in an index of this array as follows:
         *
         * [0] = Gross Pay
         * [1] = Savings Rate
         * [2] = IRA Rate
         * [3] = Savings Amount
         * [4] = IRA Amount
         * [5] = Total Saved and Invested, which is just [3] + [4]
         */
        double[] data = new double[6];

        // Explain the program, as per specifications
        explain();

        // Get user input
        data[0] = getUserInput("Enter the gross pay: ");
        data[1] = getUserInput("Enter the savings rate %: ");
        data[2] = getUserInput("Enter the IRA rate %: ");

        // Make calculations
        calculate(data);

        // Print results
        output(data);
    }

    // *************************************************************************

    /**
     * Explains the purpose of the program.
     */
    private static void explain() {
        System.out.println("This program calculates savings amounts" +
                " using methods.\n");
    }

    // *************************************************************************

    /**
     * Gets the user input.
     *
     * @param prompt The string prompt that will be printed to the user.
     * @return A double-type value for processing later.
     */
    private static double getUserInput(String prompt) {
        // Declare and/or initialize local variables
        Scanner console = null; // Scanner for user input
        double out = 0.0;       // Method output result
        boolean valid = false;  // Tracks valid input

        // Get user input
        while (!valid) {
            // Instantiate new scanner. If the loop runs more than once,
            // the JVM's garbage collection will automatically handle the
            // old scanner.
            console = new Scanner(System.in);

            // Prompt the user based on method parameter
            System.out.print(prompt);

            // Get input from the scanner
            try {
                out = console.nextDouble();
                if (out > 0) {
                    valid = true;
                    continue; // Skip the rest of the while loop
                }
            } catch (InputMismatchException e) {
                valid = false; // Not needed, but clarification is good
            }

            // If input is invalid, prompt again
            System.out.println("Please enter a value greater than 0.");
        }

        return out; // Method output
    }

    // *************************************************************************

    /**
     * Calculates the Savings Amount, IRA Investment, and Total Amount.
     * <p>
     * Note: Arrays passed into a method as a parameter can be modified
     * and the changes will be reflected back to the original array. This is
     * because an object as a method parameter is an implicit pointer back
     * to the original object. This doesn't work with primitive data types
     * in Java
     *
     * @param in The input data to process.
     */
    private static void calculate(double[] in) {
        in[3] = in[0] * (in[1] / 100.0); // Index 3 = savings amount
        in[4] = in[0] * (in[2] / 100.0); // Index 4 = ira amount
        in[5] = in[3] + in[4];           // Index 5 = total amount
    }

    // *************************************************************************

    /**
     * Outputs the data in a readable format.
     *
     * @param in The input data.
     */
    private static void output(double[] in) {
        // Use leftPad() for cleaner output.
        Toolkit toolkit = new Toolkit();

        // Print blank line for readability
        System.out.println();

        // Display formatted output
        System.out.println(
                "Gross pay:"
                        + toolkit.leftPad(in[0], 40, "$#,##0.00", "."));
        System.out.println(
                "Savings rate %:"
                        + toolkit.leftPad(in[1] / 100.0, 36, "#,##0.00%", "."));
        System.out.println(
                "Savings amount:"
                        + toolkit.leftPad(in[3], 35, "$#,##0.00", "."));
        System.out.println(
                "IRA rate %:"
                        + toolkit.leftPad(in[2] / 100.0, 40, "#,##0.00%", "."));
        System.out.println(
                "IRA investment amount:"
                        + toolkit.leftPad(in[4], 28, "$#,##0.00", "."));
        System.out.println(
                "Total of savings and IRA amounts:"
                        + toolkit.leftPad(in[5], 17, "$#,##0.00", "."));
    }

}