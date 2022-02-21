/*
 * Created by Steven Jennings on 20 October 2017.
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

package program11;

import providedCode.Toolkit;

import java.util.ArrayList;
import java.util.InputMismatchException;
import java.util.Scanner;

/**
 * Handles all of the data processing for the assignment.
 *
 * @author Steven Jennings
 * @since 20 October 2017
 */
public class DataHandler {

    // Class fields
    private ArrayList<Integer> data; // The program data
    private int total; // It's an int because we work with only ints
    private double average; // The average of all combined entries

    // *************************************************************************

    /**
     * Constructor initializes fields.
     */
    DataHandler() {
        this.data = new ArrayList<>();
        this.total = 0;
        this.average = 0.0;
    }

    // *************************************************************************

    /**
     * Gets the input from the user.
     */
    void getInput() {
        // Method variables and objects
        Scanner sc = new Scanner(System.in);
        int userInput; // Variable to track the user's inputted value

        /*
         * Get data until user enters 0.
         *
         * I use the true/break approach here because I want more control in
         * when I want the loop to end.
         */
        while (true) {
            // Prompt user
            System.out.print("Enter an integer (0 to end program): ");

            // Get input
            try {
                userInput = (int) sc.nextDouble();
            } catch (InputMismatchException e) {
                System.out.println("Enter an actual number!");
                sc = new Scanner(System.in); // Instantiate a clean scanner
                // How dare the user enter in gibberish
                continue; // Skip the rest of the loop, and repeat
            }

            // Exit loop if 0 is entered
            if (userInput == 0) {
                break;
            }

            // Add to data field if the input is not 0
            this.data.add(userInput);
        }

        // Finalize IO operations
        sc.close();
    }

    // *************************************************************************

    /**
     * Processes the program data acquired by the input module.
     */
    void process() {
        // Check for any entries at all; otherwise, do nothing
        if (this.data.size() > 0) {
            // Calculate total
            for (Integer value : this.data) {
                this.total += value;
            }

            // Calculate average
            this.average = (double) this.total / this.data.size();
        }
    }

    // *************************************************************************

    /**
     * Outputs the program data in a neat format.
     */
    void output() {
        // Execute output. No special code here
        System.out.print("Number of data values processed:");
        System.out.println(Toolkit.leftPad(this.data.size(), 6, "#,##0", "."));
        System.out.print("Total of all values:");
        System.out.println(Toolkit.leftPad(this.total, 18, "#,##0", "."));
        System.out.print("Average of all values:");
        System.out.println(Toolkit.leftPad(this.average, 19, "#,##0.00", "."));
    }

}