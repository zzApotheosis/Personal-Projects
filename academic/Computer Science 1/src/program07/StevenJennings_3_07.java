/*
 * Created by Steven Jennings on 04 October 2017.
 *
 * Program #7, CS 1050, Section 3
 *
 * Assignment description:
 * Process grades and names from an input file; produce a report
 * in an output file based on the range of grades.
 *
 * System Information:
 * Operating System: Arch Linux (amd64)
 * Official Oracle JDK Version: Version 8, Update 152 (build 1.8.0_152-b16)
 * IDE Version: IntelliJ IDEA 2017.2.5
 *
 * Extra notes:
 * Written from scratch.
 *
 * Vocabulary word of choice:
 * Hiraeth
 * (n) Homesickness tinged with grief or sadness over the lost or departed.
 * (n) A homesickness for a home you cannot return to, or that never was.
 *
 * Quote of choice:
 * "A boy who won't stand up for himself becomes a man who can't stand up
 * for anything." - Baba (The Kite Runner, by Khaled Hosseini)
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

package program07;

import providedCode.Toolkit;

import java.io.*;
import java.util.Scanner;

/**
 * The main class for program 7.
 * <p>
 * Processes grades and names from an input file; produces a report
 * in an output file based on the range of grades.
 *
 * @author Steven Jennings
 * @version 0.0.0
 * @since 04 October 2017
 */
public class StevenJennings_3_07 {

    /**
     * The main method, which executes everything in itself.
     * No other methods were written.
     *
     * @param args Unused.
     * @throws IOException In case any IO problems happen.
     */
    public static void main(String[] args) throws IOException {
        // Create variables and objects
        String message;              // Message based on grade
        int grade;                   // Person's grade
        String name;                 // Person's name
        int dataCount = 0;           // Number of data pieces in the input file
        int limitCount = 0;          // Number of grades between 70 and 89
        double limitedAverage = 0.0; // Average of grades between 70 and 89
        double totalAverage = 0.0;   // Average of all grades
        StringBuilder out =
                new StringBuilder(); // Output table

        // Create IO-related objects
        final String INPUT_FILENAME = "resources/Main_07_Input.txt";
        final String OUTPUT_FILENAME = "resources/Main_07_Output.txt";
        File inFile = new File(INPUT_FILENAME);  // Input file
        FileWriter outFile =
                new FileWriter(OUTPUT_FILENAME); // Output file
        Scanner sc = new Scanner(inFile);        // Scanner
        PrintWriter printWriter =
                new PrintWriter(
                        new BufferedWriter(
                                outFile));       // Writer

        // Append output header
        out.append(getHeader());

        // Process data
        while (sc.hasNext()) {
            // Increment line counter
            dataCount++;

            // Get data per line
            grade = sc.nextInt();        // Get person's grade
            name = sc.nextLine().trim(); // Get person's trimmed name

            /*
             * Note: the trim() method will RETURN the trimmed string. It
             * doesn't actually change the value itself. So to make a properly
             * formatted name string, make sure to assign the result of
             * trim() to name, as shown above.
             *
             * See official documentation for the String class for more info:
             * https://docs.oracle.com/javase/7/docs/api/java/lang/String.html
             *
             * It took me a bit of debugging to figure this out.
             */
            // System.out.println(name); // DEBUG

            // Increment total grade
            totalAverage += grade;

            // Process data based on grade
            if (grade >= 90) {
                message = "OUTSTANDING";
            } else if (grade >= 70) {
                message = "Satisfactory";
                limitCount++; // Increment this specific counter
                limitedAverage += grade; // Increment this total
            } else {
                message = "FAILING";
            }

            // Append output table
            out.append(getLineData(name, grade, message));
        }

        // Calculate limited average (grades between 70 and 89)
        limitedAverage = limitedAverage / limitCount;

        // Calculate total average
        totalAverage = totalAverage / dataCount;

        // Append blank line for neatness
        out.append("\n");

        // Append Summary
        out.append(getSummary(
                dataCount, limitCount, limitedAverage, totalAverage));

        // Execute output
        System.out.print(out.toString());
        printWriter.print(out.toString());

        // Close objects to finalize IO operations
        sc.close();
        printWriter.close();
    }

    // *************************************************************************

    /**
     * Gets the header for the output table.
     *
     * @return The header of the output table.
     */
    private static String getHeader() {
        return "Name                  Grade   Message      "
                + "\n___________________________________________";
    }

    // *************************************************************************

    /**
     * Gets the data line for use in the output table.
     *
     * @param name    The name of the person.
     * @param grade   The person's grade.
     * @param message The message based on the person's grade.
     * @return The formatted line of data for the output table.
     */
    private static String getLineData(String name, int grade, String message) {
        // Create StringBuilder for line data
        StringBuilder stringBuilder = new StringBuilder();

        // Toolkit for neatness
        Toolkit toolkit = new Toolkit();

        // Append data
        stringBuilder.append("\n");
        stringBuilder.append(toolkit.padString(name, 20, "", " "));
        stringBuilder.append(" "); // Column divider between name and grade
        stringBuilder.append(toolkit.leftPad(grade, 6, "0"));
        stringBuilder.append("   "); // Column divider between grade and message
        stringBuilder.append(message);

        // Output data
        return stringBuilder.toString();
    }

    // *************************************************************************

    /**
     * Gets the summary for the output table.
     *
     * @param cnt      The number of data pieces processed.
     * @param limitCnt The number of grades between 70 and 89.
     * @param limitAvg The average of grades between 70 and 89.
     * @param totalAvg The total average of all grades.
     * @return The Summary String.
     */
    private static String getSummary(int cnt,
                                     int limitCnt,
                                     double limitAvg,
                                     double totalAvg) {
        // Create StringBuilder for summary
        StringBuilder stringBuilder = new StringBuilder();

        // Use Toolkit for neatness
        Toolkit toolkit = new Toolkit();

        // Append summary
        stringBuilder.append("\nNumber of data lines processed:");
        stringBuilder.append(toolkit.leftPad(cnt, 15, "0", "."));
        stringBuilder.append("\nNumber of grades between 70 and 89:");
        stringBuilder.append(toolkit.leftPad(limitCnt, 11, "0", "."));
        stringBuilder.append("\nAverage of grades between 70 and 89:");
        stringBuilder.append(toolkit.leftPad(limitAvg, 12, "#,##0.0", "."));
        stringBuilder.append("\nAverage of all grades:");
        stringBuilder.append(toolkit.leftPad(totalAvg, 26, "#,##0.0", "."));

        // Output summary
        return stringBuilder.toString();
    }

}