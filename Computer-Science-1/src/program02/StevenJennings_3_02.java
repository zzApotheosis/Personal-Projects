/*
 * Created by Steven Jennings on 30 August 2017.
 *
 * Program #2, CS 1050, Section 3
 *
 * Assignment description:
 * This program calculates gross pay, savings, and investment.
 *
 * System Information:
 * Operating System: Arch Linux (amd64)
 * Official Oracle JDK Version: Version 8, Update 152 (build 1.8.0_152-b16)
 * IDE Version: IntelliJ IDEA 2017.2.5
 *
 * Extra notes:
 * Written from scratch. Attempting to perfectly match
 * the assignment requirements.
 *
 * Vocabulary word of choice:
 * Empyrean
 * (n) The place in the highest heaven.
 * (adj) Belonging to or deriving from heaven.
 *
 * Quote of choice:
 * "I'd rather be a failure at something I love than a success at something
 * I hate." - George Burns
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

package program02;

import java.util.InputMismatchException;
import java.util.Scanner;

/**
 * The main class for program 2.
 * <p>
 * This program calculates gross pay, savings, and investment.
 *
 * @author Steven Jennings
 * @version 0.0.0
 * @since 30 August 2017
 */
public class StevenJennings_3_02 {

    // Declare and/or initialize class-wide variables
    private static double grossPay = 0.0;
    private static double savingsRate = 0.0; // Percentage format, I.E. Input 15.0 = 15.0%
    private static double iraRate = 0.0; // Also percentage format
    private static double savingsAmount = 0.0;
    private static double iraAmount = 0.0;

    public static void main(String[] args) {
        // Get user input
        if (getUserInput() == -1) {
            return; // Exit the program early
        }

        // Make calculations
        calculate();

        // Print results
        output();
    }

    private static int getUserInput() {
        // Instantiate scanner object
        Scanner sc = new Scanner(System.in);

        // Get Gross Pay
        System.out.print("Enter the gross pay: ");
        try {
            grossPay = sc.nextDouble();
        } catch (InputMismatchException e) {
            e.printStackTrace();
            System.out.println("Invalid input!");
            return -1;
        }

        // Get Savings Rate
        System.out.print("Enter the savings rate %: ");
        try {
            savingsRate = sc.nextDouble();
        } catch (InputMismatchException e) {
            e.printStackTrace();
            System.out.println("Invalid input!");
            return -1;
        }

        // Get IRA Rate
        System.out.print("Enter the IRA rate %: ");
        try {
            iraRate = sc.nextDouble();
        } catch (InputMismatchException e) {
            e.printStackTrace();
            System.out.println("Invalid input!");
            return -1;
        }

        return 0; // 0 for successful method call
    }

    private static int calculate() {
        savingsAmount = grossPay * (savingsRate / 100.0);
        iraAmount = grossPay * (iraRate / 100.0);
        return 0; // 0 for successful method call
    }

    private static int output() {
        System.out.println("Gross pay: " + grossPay);
        System.out.println("Savings rate %: " + savingsRate);
        System.out.println("Savings amount: " + savingsAmount);
        System.out.println("IRA rate %: " + iraRate);
        System.out.println("IRA investment amount: " + iraAmount);
        System.out.println("Total of savings and IRA amounts: " + (savingsAmount + iraAmount));
        return 0; // 0 for successful method call
    }

}