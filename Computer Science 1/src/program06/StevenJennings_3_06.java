/*
 * Created by Steven Jennings on 22 September 2017.
 *
 * Program #6, CS 1050, Section 3
 *
 * Assignment description:
 * Calculate mileage reimbursement based on the data in the following table.
 * This program uses input and output files with Java IO.
 *
 * Round trip mileage           Rate
 * ___________________________  _______________________________________________
 * less than 400 miles			18 cents per mile
 * ≥ 400, < 900 miles			$ 65.00 plus 15 cents for each mile over 400
 * ≥ 900 miles, < 1300 miles	$115.00 plus 12 cents for each mile over 900
 * ≥ 1300, < 1900 miles			$140.00 plus 10 cents for each mile over 1300
 * ≥ 1900, < 2600 miles			$165.00 plus 8 cents for each mile over 1900
 * ≥ 2600 miles                 $195.00 plus 6 cents for each mile over 2600
 *
 * System Information:
 * Operating System: Arch Linux (amd64)
 * Official Oracle JDK Version: Version 8, Update 152 (build 1.8.0_152-b16)
 * IDE Version: IntelliJ IDEA 2017.2.5
 *
 * Extra notes:
 * Written from scratch. Starting with this assignment, our class will
 * submit work directly to professor Kramer.
 *
 * I manually switched back to Java 8. Not sure why the jdk package maintainers
 * on the AUR opted to use Java 9 so early in its development.
 * For any fellow Arch users, I'm using the jdk8 package in the AUR.
 *
 * Vocabulary word of choice:
 * Apotheosis
 * (n) The perfect form or example of something. (Quintessence)
 * (n) The highest or best part of something. (Peak)
 * (n) Elevation to divine status. (Deification)
 *
 * Quote of choice:
 * "Give me a lever long enough and a fulcrum strong enough, and single-
 * handedly I will move the world." - Archimedes
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

package program06;

import providedCode.Toolkit;

import java.io.*;
import java.text.DecimalFormat;
import java.util.Scanner;

/**
 * The main class for program 6.
 * <p>
 * Calculates mileage reimbursement based on the data in the following table.
 * This program uses input and output files with Java IO.
 * <p>
 * The specification explicitly states to avoid using external classes
 * to hold data, so everything required to satisfy this assignment's
 * requirements is in this class.
 *
 * @author Steven Jennings
 * @version 0.0.0
 * @since 22 September 2017
 */
public class StevenJennings_3_06 {

    /**
     * The main method.
     *
     * @param args Unused.
     * @throws IOException For any IO-related exceptions. This makes
     *                     the code much cleaner because I don't have
     *                     to write try/catch blocks everywhere.
     */
    public static void main(String[] args) throws IOException {
        // Declare or initialize program objects and variables
        int dataCount;                           // Number of data pieces
        double miles;                            // Mileage
        double reimburse;                        // Reimbursement amount
        double reimburseTotal = 0.0;             // Reimbursement total
        int validCount = 0;                      // Counter for non-negatives
        StringBuilder out = new StringBuilder(); // Output table

        // Prepare IO-related objects and variables
        final String INPUT_FILENAME = "resources/Main_06_Input.txt";
        final String OUTPUT_FILENAME = "resources/Main_06_Output.txt";
        File inFile = new File(INPUT_FILENAME); // Input file
        Scanner sc = new Scanner(inFile);       // Input file scanner

        // BEGIN PROCESSING DATA

        // The first number of the input file
        dataCount = sc.nextInt();

        // Append output table header (4 spaces for column divider)
        out.append(getHeader()); // Gets header from method

        // Process data
        while (sc.hasNext()) {
            // Get the next piece of data to process
            miles = sc.nextDouble();

            // Determine the reimbursement based on the mileage
            if (miles < 0.0) { // Invalid mileage
                reimburse = 0.0;
            } else { // Valid mileage
                // Increment valid data count
                validCount++;

                // Begin valid mileages - Note: This is a nested if statement
                if (miles == 0.0) {                    // Mileage = 0
                    reimburse = 0.0;

                } else if (miles < 400.0) {            // 0 < Mileage < 400
                    reimburse = 0.0
                            + 0.18 * (miles);

                } else if (miles < 900) {              // 400 <= Mileage < 900
                    reimburse = 65.0
                            + 0.16 * (miles - 400.0);

                } else if (miles < 1300.0) {           // 900 <= Mileage < 1300
                    reimburse = 115.0
                            + 0.12 * (miles - 900.0);

                } else if (miles < 1900.0) {           // 1300 <= Mileage < 1900
                    reimburse = 140.0
                            + 0.10 * (miles - 1300);

                } else if (miles < 2600.0) {           // 1900 <= Mileage < 2600
                    reimburse = 165.0
                            + 0.08 * (miles - 1900);

                } else {                               // 2600 <= Mileage
                    reimburse = 195.0
                            + 0.06 * (miles - 2600);
                }
            }

            // Keep track of total reimbursement
            reimburseTotal += reimburse;

            // Append a formatted line of data for the output table
            out.append(getLineData(miles, reimburse));
        }

        // Append blank line for neatness
        out.append("\n");

        // Append extra data after the main table
        out.append(getSummary(reimburseTotal, dataCount, validCount));

        // Execute output
        output(out.toString(), OUTPUT_FILENAME);

        // Close Scanner to finalize IO operations
        sc.close();
    }

    // *************************************************************************

    /**
     * Returns a constant header. It is the exact same string every time
     * this method is called.
     *
     * @return The table header for the output string.
     */
    private static String getHeader() {
        return "   Mileage       Reimbursement\n__________    ________________";
    }

    // *************************************************************************

    /**
     * Forms a formatted string for use in the output table.
     *
     * @param mileValue      The value of mileage.
     * @param reimburseValue The reimbursement determined by the mileage value.
     * @return A string of formatted data for the output table.
     */
    private static String getLineData(double mileValue, double reimburseValue) {
        // Create StringBuilder for data line
        StringBuilder stringBuilder = new StringBuilder();

        // Utilize Toolkit for neatness
        Toolkit toolkit = new Toolkit();

        // Append new line
        stringBuilder.append("\n");

        // Append mileage
        stringBuilder.append(toolkit.leftPad(mileValue, 10, "#,##0.0"));

        // Append column divider
        stringBuilder.append("    "); // 4 spaces

        // Append reimbursement based on mileage value
        if (mileValue < 0) {
            // Invalid mileage (Negative)
            stringBuilder.append(toolkit.padString("*****", 16, " ", ""));
        } else {
            // Valid mileage (Non-negative)
            stringBuilder.append(toolkit.leftPad(reimburseValue, 16, "$#,##0.00"));
        }

        // Output formatted line of data
        return stringBuilder.toString();
    }

    // *************************************************************************

    /**
     * Creates the last few lines in the output table for extra information.
     *
     * @param totalMoney The total reimbursement.
     * @param numLines   The number of data values processed.
     * @param numValid   The number of data values that were non-negative.
     * @return The output summary as a string.
     */
    private static String getSummary(double totalMoney, int numLines, int numValid) {
        // Create StringBuilder for summary
        StringBuilder stringBuilder = new StringBuilder();

        // Create DecimalFormat for money format
        DecimalFormat decimalFormat = new DecimalFormat("$#,##0.00");

        // Append summary
        stringBuilder.append("\nTotal amount of reimbursement: ");
        stringBuilder.append(decimalFormat.format(totalMoney));
        stringBuilder.append("\nNumber of data values processed: ");
        stringBuilder.append(numLines);
        stringBuilder.append("\nNumber of valid data values processed: ");
        stringBuilder.append(numValid);

        // Output summary
        return stringBuilder.toString();
    }

    // *************************************************************************

    /**
     * Executes the output operations. This method writes to the output file
     * and also echoes the data to console.
     *
     * @param table    A completed string to write.
     * @param filename The name of the output file, which will be written to the
     *                 working directory.
     * @throws IOException In case there are IO problems.
     */
    private static void output(String table, String filename) throws IOException {
        // Create output writer
        PrintWriter printWriter =
                new PrintWriter(new BufferedWriter(new FileWriter(filename)));

        // Execute output
        printWriter.print(table);
        System.out.print(table);

        // Finalize IO operations
        printWriter.close();
    }

}