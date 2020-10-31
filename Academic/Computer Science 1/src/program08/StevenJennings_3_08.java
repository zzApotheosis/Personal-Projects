/*
 * Created by Steven Jennings on 04 October 2017.
 *
 * Program #8, CS 1050, Section 3
 *
 * Assignment description:
 * Calculate mileage reimbursements using arrays and methods.
 * This uses the same reimbursement table as found in program6.
 *
 * System Information:
 * Operating System: Arch Linux (amd64)
 * Official Oracle JDK Version: Version 8, Update 152 (build 1.8.0_152-b16)
 * IDE Version: IntelliJ IDEA 2017.2.5
 *
 * Extra notes:
 * Written from scratch. If it was up to me, I would consider 0 miles to be
 * a valid mileage.
 *
 * Vocabulary word of choice:
 * Oneiromancy
 * (n) Divination through dreams; the practice of predicting the future through
 * interpretation of dreams.
 * (n) The interpretation of dreams in order to foretell the future.
 *
 * Quote of choice:
 * "We hang the petty thieves and appoint the great ones to public office."
 * - Aesop
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

package program08;

import providedCode.Toolkit;

import java.io.*;
import java.text.DecimalFormat;
import java.util.Scanner;

/**
 * The main class for program 8.
 * <p>
 * Calculates mileage reimbursements using arrays and methods.
 * Note, this uses the same reimbursement table as found in program6.
 *
 * @author Steven Jennings
 * @version 0.0.0
 * @since 04 October 2017
 */
public class StevenJennings_3_08 {

    /**
     * The main method.
     *
     * @param args Unused.
     * @throws IOException In case there are IO problems.
     *                     I include exceptions in these method signatures
     *                     so that I don't have to write try/catch blocks
     *                     everywhere.
     */
    public static void main(String[] args) throws IOException {
        /*
         * Similarly to my data matrix in Program 4, this 2D array holds
         * all data for this program in this format:
         *
         * data[x][y]
         *
         *     x  Col0     Col1
         * y    _________________________
         * Row0 | mileage0 reimbursement0
         * Row1 | mileage1 reimbursement1
         * Row2 | mileage2 reimbursement2
         * etc...
         *
         * As you can see, it has a constant width of 2, and a variable height
         * based on the number of elements in the input file, which is easily
         * obtainable because it is the first element in the input file itself.
         */
        double[][] data;           // Program data
        double totalReimbursement; // Total reimbursement
        int validCount;            // Number of valid data
        double avgMoney;           // Average reimbursement
        double avgMiles;           // Average mileage

        // Explain program
        explain();

        // BEGIN PROCESSING DATA

        // Get input from file
        data = readInput();

        /*
         * Calculate reimbursement values.
         *
         * It should be noted that OBJECTS passed into parameters are actually
         * pointers. This includes arrays, so by passing my data matrix into
         * the calcReimbursements() method, I am actually passing in a pointer to the
         * original array right here in the main method. By doing this, all of
         * the modifications done to the array in the calcReimbursements() method are
         * directly reflected back to the data matrix in the main method.
         */
        calcReimbursements(data);

        // Calculate total reimbursement
        totalReimbursement = calcTotalReimbursement(data);

        // Calculate number of valid data lines
        validCount = calcNumValidValues(data);

        // Calculate averages
        avgMoney = calcAvgReimbursements(data);
        avgMiles = calcAvgMileage(data);

        // Execute output
        output(data, totalReimbursement, validCount, avgMoney, avgMiles);
    }

    // *************************************************************************

    /**
     * Explains the program.
     */
    private static void explain() {
        System.out.println("This program calculates mileage reimbursements" +
                " using arrays and methods.\n");
    }

    // *************************************************************************

    /**
     * Reads the input file for data.
     *
     * @return The data matrix with raw program data.
     * @throws IOException In case there are IO problems.
     */
    private static double[][] readInput() throws IOException {
        // IO-related objects and variables
        final String INPUT_FILENAME = "resources/Main_08_Input.txt"; // Input file name
        Scanner sc = new Scanner(new File(INPUT_FILENAME)); // File scanner
        int numOfNums = sc.nextInt(); // The first number in the input file
        double[][] out = new double[2][numOfNums]; // Method output data matrix

        /*
         * Reminder: double[][] out is what becomes the data matrix for the
         * whole program. It has a constant width of 2 and a variable height
         * of "length" depending on the amount of data to process in the
         * input file.
         */

        // Assign data from file to array
        for (int i = 0; i < numOfNums; i++) {
            out[0][i] = sc.nextDouble();
        }

        // Close Scanner
        sc.close();

        return out; // Output raw program data
    }

    // *************************************************************************

    /**
     * Calculates reimbursement rates based on mileage.
     *
     * @param in The input data matrix with mileage. The original array will
     *           be modified because arrays are not primitive data types.
     */
    private static void calcReimbursements(double[][] in) {
        /*
         * For each mileage value in the array at index [0][y],
         * determine its reimbursement, and store it at its
         * partner index [1][y].
         *
         * Reminder:
         * The [0][y] index is the mileage value at line y.
         * Likewise,
         * the [1][y] index is the calculated reimbursement for that mileage.
         */
        for (int y = 0; y < in[0].length; y++) {
            if (in[0][y] <= 0.0) {                    // Non-positive mileage
                in[1][y] = 0.0;

            } else if (in[0][y] < 400.0) {            // 0 < Mileage < 400
                in[1][y] = 0.0
                        + 0.18 * (in[0][y]);

            } else if (in[0][y] < 900) {              // 400 <= Mileage < 900
                in[1][y] = 65.0
                        + 0.15 * (in[0][y] - 400.0);

            } else if (in[0][y] < 1300.0) {           // 900 <= Mileage < 1300
                in[1][y] = 115.0
                        + 0.12 * (in[0][y] - 900.0);

            } else if (in[0][y] < 1900.0) {           // 1300 <= Mileage < 1900
                in[1][y] = 140.0
                        + 0.10 * (in[0][y] - 1300);

            } else if (in[0][y] < 2600.0) {           // 1900 <= Mileage < 2600
                in[1][y] = 165.0
                        + 0.08 * (in[0][y] - 1900);

            } else {                                  // 2600 <= Mileage
                in[1][y] = 195.0
                        + 0.06 * (in[0][y] - 2600);
            }
        }
    }

    // *************************************************************************

    /**
     * Calculates the total reimbursement.
     *
     * @param in The program data, complete with reimbursement calculations.
     * @return The total reimbursement summation.
     */
    private static double calcTotalReimbursement(double[][] in) {
        // Initialize total reimbursement counter
        double total = 0.0;

        // Calculate total reimbursement
        for (int i = 0; i < in[0].length; i++) {
            total += in[1][i];
        }

        // Output total reimbursement
        return total;
    }

    // *************************************************************************

    /**
     * Calculates the number of valid data values in the data.
     *
     * @param in The input data (Must have mileage values to process).
     * @return The number of valid data values.
     */
    private static int calcNumValidValues(double[][] in) {
        // Initialize valid line counter
        int counter = 0;

        // Calculate number of valid lines
        for (int i = 0; i < in[0].length; i++) {
            if (in[0][i] > 0.0) {
                counter++;
            }
        }

        // Output number of valid lines
        return counter;
    }

    // *************************************************************************

    /**
     * Calculates the average reimbursement value.
     *
     * @param in The input data.
     * @return The average reimbursement value.
     */
    private static double calcAvgReimbursements(double[][] in) {
        // Initialize method variables
        double avg = 0.0; // Average reimbursement
        int ctr = 0; // Counter

        // Accumulate total reimbursement
        for (int i = 0; i < in[1].length; i++) {
            if (in[1][i] > 0.0) {
                avg += in[1][i];
                ctr++;
            }
        }

        // Calculate Average Reimbursement
        if (ctr != 0) {
            avg /= ctr;
        } else {
            avg = 0.0;
        }

        // Method output
        return avg;
    }

    // *************************************************************************

    /**
     * Calculates the average mileage value.
     *
     * @param in The input data.
     * @return The average mileage value.
     */
    private static double calcAvgMileage(double[][] in) {
        // Method variables
        double avg = 0.0; // Average mileage
        int ctr = 0; // Counter

        // Accumulate total mileage
        for (int i = 0; i < in[0].length; i++) {
            if (in[0][i] > 0.0) {
                avg += in[0][i];
                ctr++;
            }
        }

        // Perform calculation
        if (ctr != 0) {
            avg /= ctr;
        } else {
            avg = 0.0;
        }

        // Method output
        return avg;
    }

    // *************************************************************************

    /**
     * Writes and prints the program results to the output file and
     * console, respectively.
     *
     * @param in                 The program data.
     * @param totalReimbursement The option to write to file.
     * @param validCount         The number of valid pieces of data.
     * @param avgMoney           The calculated average of reimbursement.
     * @param avgMiles           The calculated average of mileage.
     * @throws IOException In case there are IO problems.
     */
    private static void output(double[][] in,
                               double totalReimbursement,
                               int validCount,
                               double avgMoney,
                               double avgMiles) throws IOException {
        // Create variables and objects
        final String OUTPUT_FILENAME = "resources/Main_08_Output.txt";
        PrintWriter printWriter = new PrintWriter(new BufferedWriter(
                new FileWriter(OUTPUT_FILENAME)));
        String out = createTable(
                in,
                totalReimbursement,
                validCount,
                avgMoney,
                avgMiles);

        // Execute output
        printWriter.print(out);
        System.out.print(out);

        // Finalize IO operations
        printWriter.close();
    }

    // *************************************************************************

    /**
     * Creates the output table.
     *
     * @param in                 The program data.
     * @param totalReimbursement The total amount of reimbursement.
     * @param validCount         The number of valid pieces of data.
     * @param avgMoney           The calculated average of reimbursement.
     * @param avgMiles           The calculated average of mileage.
     * @return The finished output table.
     */
    private static String createTable(double[][] in,
                                      double totalReimbursement,
                                      int validCount,
                                      double avgMoney,
                                      double avgMiles) {
        // Initialize string builder for output table
        StringBuilder out = new StringBuilder();

        // Append header
        out.append(getHeader());

        // Append data per line
        for (int y = 0; y < in[0].length; y++) {
            out.append(getLineData(in, y));
        }

        // Append extra summary information
        out.append(getSummary(
                totalReimbursement,
                in[0].length,
                validCount,
                avgMoney,
                avgMiles));

        return out.toString(); // Output completed table
    }

    // *************************************************************************

    /**
     * Gets the output table header as a String.
     *
     * @return The table header.
     */
    private static String getHeader() {
        return new StringBuilder()
                .append("   Mileage       Reimbursement")
                .append("\n__________    ________________")
                .toString();
    }

    // *************************************************************************

    /**
     * Gets a line of formatted data for use in the output table.
     *
     * @param in    The input data.
     * @param index The index to process.
     * @return A line of formatted data.
     */
    private static String getLineData(double[][] in, int index) {
        // Method variables and objects
        StringBuilder lineData = new StringBuilder(); // Line of data
        Toolkit toolkit = new Toolkit(); // For neatness

        // Append data
        lineData.append("\n"); // New line of data
        lineData.append(toolkit.leftPad(in[0][index], 10, "#,##0.0"));
        lineData.append("    "); // 4 space column divider
        if (in[0][index] <= 0.0) {
            // Special case for invalid mileage (Non-positive mileage)
            lineData.append(toolkit.padString("*****", 16, " ", ""));
        } else {
            // Reimbursement for valid mileage (Positive mileage)
            lineData.append(toolkit.leftPad(in[1][index], 16, "$#,##0.00"));
        }

        // Method output
        return lineData.toString();
    }

    // *************************************************************************

    /**
     * Gets the summary information for use at the end of the output table.
     *
     * @param totalMoney The total amount of reimbursement.
     * @param length     The number of data values processed.
     * @param valid      The number of valid data lines.
     * @param avgMoney   The calculated average of reimbursement.
     * @param avgMiles   The calculated average of mileage.
     * @return The summary string.
     */
    private static String getSummary(double totalMoney,
                                     int length,
                                     int valid,
                                     double avgMoney,
                                     double avgMiles) {
        // Method variables and objects
        StringBuilder summary = new StringBuilder();
        DecimalFormat moneyFormat = new DecimalFormat("$#,##0.00");
        DecimalFormat milesFormat = new DecimalFormat("#,##0.00");

        // Append summary
        summary.append("\n\nTotal amount of reimbursement: ");
        summary.append(moneyFormat.format(totalMoney));
        summary.append("\nNumber of data values processed: ");
        summary.append(length);
        summary.append("\nNumber of valid data values processed: ");
        summary.append(valid);
        summary.append("\nAverage reimbursement: ");
        summary.append(moneyFormat.format(avgMoney));
        summary.append("\nAverage valid mileage: ");
        summary.append(milesFormat.format(avgMiles));

        // Method output
        return summary.toString();
    }

}