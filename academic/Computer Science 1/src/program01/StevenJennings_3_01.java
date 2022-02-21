/*
 * Created by Steven Jennings on 21 August 2017.
 *
 * Program #1, CS 1050, Section 3
 *
 * Assignment description:
 * This program asks the user to input two real numbers,
 * calculates the average of these numbers, and prints the results.
 *
 * System Information:
 * Operating System: Arch Linux (amd64)
 * Official Oracle JDK Version: Version 8, Update 152 (build 1.8.0_152-b16)
 * IDE Version: IntelliJ IDEA 2017.2.5
 *
 * Extra notes:
 * I wrote this from scratch. I didn't use the template provided, sorry!
 * Please note that I will keep all of my code
 * on my GitHub repository: https://github.com/zzApotheosis/Computer-Science-1
 *
 * Vocabulary word of choice:
 * Ataraxia
 * (n) The only true happiness possible for a person.
 * (n) A state of serene calmness.
 * (n) Calmness untroubled by mental or emotional disquiet.
 *
 * Quote of choice:
 * "Once in a while, you have to take a break and visit yourself."
 * - Audrey Giorgi
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

package program01;

import java.util.InputMismatchException;
import java.util.Scanner;

/**
 * The main class for program 1.
 * <p>
 * This program asks the user to input two real numbers,
 * calculates the average of these numbers, and prints the results.
 *
 * @author Steven Jennings
 * @version 0.0.0
 * @since 21 August 2017
 */
public class StevenJennings_3_01 {

    /**
     * The main method.
     *
     * @param args Unused.
     */
    public static void main(String[] args) {
        // Declare/instantiate variables
        Scanner sc = new Scanner(System.in);
        double num1, num2, result;

        // Get first number from user
        System.out.print("Enter the first number to average: ");
        try {
            num1 = sc.nextDouble();
        } catch (InputMismatchException e) {
            e.printStackTrace();
            System.out.println("That's not a number!");
            return; // Stop the program if the input is incorrect
        }

        // Get second number from user
        System.out.print("Enter the second number to average: ");
        try {
            num2 = sc.nextDouble();
        } catch (InputMismatchException e) {
            e.printStackTrace();
            System.out.println("That's not a number!");
            return; // Stop the program if the input is incorrect
        }

        // Calculate the average. This will always be proper division because
        // num1 and num2 are both double data types.
        result = (num1 + num2) / 2.0;

        // Display results
        System.out.print("Average: " + result);

        // Close the scanner
        sc.close();
    }

}