/*
 * Created by Steven Jennings on 19 October 2017.
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

package program10;

import providedCode.Employee;
import providedCode.EmployeeParameters;
import providedCode.Toolkit;

import java.io.*;
import java.text.DecimalFormat;
import java.util.LinkedList;
import java.util.Scanner;

/**
 * Handles all of the data processing for the assignment.
 *
 * @author Steven Jennings
 * @version 0.0.0
 * @since 19 October 2017
 */
class DataHandler {

    // Class fields
    private EmployeeParameters parameters;   // Provided parameters
    private Employee[] employees; // Array of employee data

    // Constants
    private final String INPUT_FILENAME = "resources/Main_10_Input.txt";   // Input
    private final String OUTPUT_FILENAME = "resources/Main_10_Output.txt"; // Output

    // *************************************************************************

    /**
     * Default constructor initializes fields.
     */
    DataHandler() {
        initialize();
    }

    // *************************************************************************

    /**
     * Custom constructor allows for more control.
     *
     * @param automatic Controls whether or not the user wants to
     *                  include input and data processing all in the
     *                  constructor. If false, the user must manually
     *                  call the input and process methods.
     */
    DataHandler(boolean automatic) {
        // Perform initializations
        initialize();

        // Construct the object with data already loaded
        if (automatic) {
            this.getInput();
            this.process();
        }
    }

    // *************************************************************************

    /**
     * Initializes the fields of the object. For use only with constructors.
     */
    private void initialize() {
        // Initialize fields
        this.parameters = new EmployeeParameters();
        this.employees = null; // This will be filled in later

        // Get the employee parameters from file
        try {
            this.parameters.getEmployeeParameters();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    // *************************************************************************

    /**
     * Gets the class data from file.
     */
    void getInput() {
        // Create method variables and objects
        int numEmployees = 0; // Counter starts at 0
        Scanner sc = null; // Input file scanner
        LinkedList<Object> temporaryBuffer =
                new LinkedList<>(); // Object to temporarily store fetched input

        // Instantiate scanner
        try {
            sc = new Scanner(new File(this.INPUT_FILENAME));
        } catch (FileNotFoundException e) {
            e.printStackTrace();
            System.exit(-1); // Stop program if file is not found
        }

        // Fetch data from file, assuming the number of elements in the input
        // file is divisible by 3
        while (sc.hasNext()) {
            // Fetch raw data
            try {
                temporaryBuffer.add(sc.nextDouble()); // Number of hours worked
                temporaryBuffer.add(sc.nextDouble()); // Employee's pay rate
                temporaryBuffer.add(sc.nextLine().trim()); // Employee's name
            } catch (NullPointerException e) {
                System.err.println("Input file is not properly formatted!");
                System.exit(-1); // Exit if input file is not in valid format
            }

            // Increment employee counter
            numEmployees++;

            // Check for possible violation of maximum employees
            if (numEmployees > this.parameters.maxEmployees) {
                System.err.print("Too many employees to process. ");
                System.err.println("Exiting program.");
                System.exit(-1);
            }
        }

        /*
         * Now that we know how many employees there are, we can initialize
         * the employee array, which is a class field.
         *
         * PLEASE NOTE: By creating an array of objects, we have created
         * an object that holds multiple objects. Think of it like a chest!
         *
         * However, these individual objects have NOT been initialized,
         * which is why we need to manually initialize every
         * object in the array, as shown below.
         */
        this.employees = new Employee[numEmployees];
        for (int i = 0; i < this.employees.length; i++) {
            this.employees[i] = new Employee();
        }

        /*
         * And now we can assign the contents of the temporary buffer
         * to the employee array. Note: we need to cast the data in the
         * temporary buffer to their appropriate data types because
         * they are actually stored as raw objects in the LinkedList.
         */
        for (int i = 0; i < this.employees.length; i++) {
            this.employees[i].hoursWorked = (double) temporaryBuffer.get(3 * i);
            this.employees[i].payRate = (double) temporaryBuffer.get(3 * i + 1);
            this.employees[i].name = (String) temporaryBuffer.get(3 * i + 2);
        }

        // Finalize IO operations
        sc.close();
    }

    // *************************************************************************

    /**
     * Processes each employee's data in these ways:
     * 1) Calculates Gross Pay
     * 2) Calculates IRA Investment Amount
     * 3) Calculates Adjusted Gross Pay
     * 4) Calculates Taxes
     * 5) Calculates Net Pay
     * 6) Calculates Savings Amount
     * 7) Calculates Wealth
     */
    void process() {
        /*
         * First of all, we calculate each employee's gross pay.
         * I spaced out the if statement for easier readability.
         *
         * Also, these formulas were derived, not given.
         */
        for (Employee person : this.employees) {
            if (person.hoursWorked <= 0.0) {
                // No pay for you, lazy!
                person.grossPay = 0.0;


            } else if (person.hoursWorked <= 40.0) {
                // Regular pay
                person.grossPay =
                        person.payRate *
                                person.hoursWorked;


            } else if (person.hoursWorked <= 50.0) {
                // Time and a half
                person.grossPay =
                        person.payRate *
                                (1.5 * person.hoursWorked - 20.0);


            } else {
                // Double time for everything above 50 hours
                person.grossPay =
                        person.payRate *
                                (2.0 * person.hoursWorked - 45.0);
            }
        }

        // Next, calculate IRA Investment amount per employee
        for (Employee person : this.employees) {
            person.iraAmount =
                    person.grossPay *
                            (this.parameters.iraRate / 100.0);
        }

        // Next, calculate adjusted gross pay per employee
        for (Employee person : this.employees) {
            person.adjustedGrossPay = person.grossPay - person.iraAmount;
        }

        // Next, calculate taxes per employee
        for (Employee person : this.employees) {
            person.taxAmount = person.adjustedGrossPay *
                    (this.parameters.federalWithholdingRate +
                            this.parameters.stateWithholdingRate) / 100.0;
        }

        // Next, calculate net pay per employee
        for (Employee person : this.employees) {
            person.netPay = person.adjustedGrossPay - person.taxAmount;
        }

        // Next, calculate savings amount per employee
        for (Employee person : this.employees) {
            person.savingsAmount = person.netPay *
                    (parameters.savingsRate / 100.0);
        }

        // Finally, calculate wealth accumulation per employee
        for (Employee person : this.employees) {
            person.wealth = person.savingsAmount +
                    person.iraAmount;
        }

        /*
         * At this point, ALL of the required program data has
         * been properly and appropriately calculated.
         */
    }

    // *************************************************************************

    /**
     * Outputs the original data in a readable table. The data is echoed to
     * console and written to file.
     */
    void output() {
        // Create method objects and variables
        String out = createTable();
        PrintWriter printWriter;

        // Initialize PrintWriter
        try {
            printWriter = new PrintWriter(
                    new BufferedWriter(
                            new FileWriter(
                                    this.OUTPUT_FILENAME)));
        } catch (IOException e) {
            e.printStackTrace();
            System.err.println("Output failed.");
            return;
        }

        // Execute output
        printWriter.print(out);
        System.out.print(out);

        // Finalize IO operations
        printWriter.flush();
        printWriter.close();
    }

    // *************************************************************************

    /**
     * Creates the output table for use in output modules.
     *
     * @return The output table as a properly formatted string.
     */
    private String createTable() {
        // Method variables and objects
        StringBuilder table = new StringBuilder(); // To build the table
        String header; // Table header
        String tableData; // Table data
        String summary; // Table summary

        // Get each section of the table
        header = getHeader(); // Method as described in 6a
        tableData = getTableData(); // Method as described in 6b
        summary = getSummary(); // Method as described in 6c

        /*
         * Append each section to the whole table.
         *
         * The first time the output table is created, it is sorted by
         * order of the input file.
         */
        table.append(header);
        table.append(tableData);
        table.append("\n\n"); // Extra space for neatness
        table.append(summary);

        /*
         * Next, I create another table of the data with alphabetically
         * sorted employee names.
         */
        Toolkit.selectionSortArrayOfClass(this.employees,
                this.employees.length, "NAME");
        table.append("\n\n");
        table.append(header); // It'll be the same header
        table.append(getTableData()); // Different table data
        table.append("\n\n");
        table.append(summary); // Same summary

        /*
         * Finally, I create another table of the data with numerically
         * sorted gross pay values.
         */
        Toolkit.selectionSortArrayOfClass(this.employees,
                this.employees.length, "GROSS PAY");
        table.append("\n\n");
        table.append(header); // Same header
        table.append(getTableData()); // Different table data
        table.append("\n\n");
        table.append(summary); // Same summary

        /*
         * At this point, the entire output string is ready to execute.
         */
        return table.toString();
    }

    // *************************************************************************

    /**
     * Gets the header as a formatted string.
     *
     * @return The table header.
     */
    private String getHeader() {
        // Methods variables and objects
        StringBuilder header = new StringBuilder();
        String title = "Mobile Apps Galore, Inc. - Payroll Report";
        String columnHeaders = "Name                     Gross Pay   Net Pay" +
                "   Wealth    Taxes   Hours  Pay Rate";
        String divider = "__________________________________________________" +
                "______________________________";

        // Center the title with the table
        while (title.length() < columnHeaders.length()) {
            title = " " + title + " ";
        }

        // Append header
        header.append(title);
        header.append("\n\n"); // Extra space between title and column headers
        header.append(columnHeaders);
        header.append("\n");
        header.append(divider);

        // Method output
        return header.toString();
    }

    // *************************************************************************

    /**
     * Gets the raw formatted table as a string for use in the larger
     * output table.
     *
     * @return The table data.
     */
    private String getTableData() {
        // Method variables and objects
        StringBuilder tableData = new StringBuilder();

        // Append formatted data line for each employee
        for (Employee person : this.employees) {
            tableData.append("\n"); // New line
            tableData.append(Toolkit.padString( // Employee's name
                    person.name, 21, "", " "));
            tableData.append(" ");
            tableData.append(Toolkit.leftPad( // Gross Pay
                    person.grossPay, 12, "#,##0.00"));
            tableData.append(" ");
            tableData.append(Toolkit.leftPad( // Net Pay
                    person.netPay, 9, "#,##0.00"));
            tableData.append(" ");
            tableData.append(Toolkit.leftPad( // Wealth
                    person.wealth, 8, "#,##0.00"));
            tableData.append(" ");
            tableData.append(Toolkit.leftPad( // Taxes
                    person.taxAmount, 8, "#,##0.00"));
            tableData.append(" ");
            tableData.append(Toolkit.leftPad( // Hours
                    person.hoursWorked, 7, "#,##0.00"));
            tableData.append(" ");
            tableData.append(Toolkit.leftPad( // Pay Rate
                    person.payRate, 9, "#,##0.00"));
        }

        return tableData.toString();
    }

    // *************************************************************************

    /**
     * Gets the extra information for the end of the output table.
     * <p>
     * Note: It's usually better practice to do all of your data processing
     * together, but in this case, the specifications explicitly require a
     * separate method for these calculations.
     * <p>
     * See part 6 and 6c of the specs.
     *
     * @return The extra table information.
     */
    private String getSummary() {
        // Temporary method variables.
        double[] totals = new double[6];   // Totals of the data
        double[] averages = new double[6]; // Averages of the data

        // StringBuilder for summary
        StringBuilder summary = new StringBuilder();

        // DecimalFormat for neatness
        DecimalFormat decimalFormat = new DecimalFormat("#,##0.0%");

        // Calculate totals
        for (Employee person : this.employees) {
            totals[0] += person.grossPay;
            totals[1] += person.netPay;
            totals[2] += person.wealth;
            totals[3] += person.taxAmount;
            totals[4] += person.hoursWorked;
            totals[5] += person.payRate;
        }

        // Calculate Averages
        for (int i = 0; i < averages.length; i++) {
            averages[i] = totals[i] / this.employees.length;
        }

        // Begin appending the summary
        // Append Summary Header
        summary.append("SUMMARY");
        summary.append("\n");
        summary.append("_____________________________________________________");
        summary.append("___________________________");

        // The totals
        summary.append("\n");
        summary.append("TOTALS");
        summary.append("\n______");
        summary.append("\nGross Pay:");
        summary.append(Toolkit.leftPad(totals[0], 24, "#,##0.00", "."));
        summary.append("\nNet Pay:");
        summary.append(Toolkit.leftPad(totals[1], 36, "#,##0.00", "."));
        summary.append("\nWealth:");
        summary.append(Toolkit.leftPad(totals[2], 46, "#,##0.00", "."));
        summary.append("\nTaxes:");
        summary.append(Toolkit.leftPad(totals[3], 56, "#,##0.00", "."));
        summary.append("\nHours:");
        summary.append(Toolkit.leftPad(totals[4], 64, "#,##0.00", "."));
        summary.append("\nPay Rate:");
        summary.append(Toolkit.padString("N/A", 71, ".", ""));

        // The averages
        summary.append("\n\n");
        summary.append("AVERAGES");
        summary.append("\n________");
        summary.append("\nGross Pay:");
        summary.append(Toolkit.leftPad(averages[0], 24, "#,##0.00", "."));
        summary.append("\nNet Pay:");
        summary.append(Toolkit.leftPad(averages[1], 36, "#,##0.00", "."));
        summary.append("\nWealth:");
        summary.append(Toolkit.leftPad(averages[2], 46, "#,##0.00", "."));
        summary.append("\nTaxes:");
        summary.append(Toolkit.leftPad(averages[3], 56, "#,##0.00", "."));
        summary.append("\nHours:");
        summary.append(Toolkit.leftPad(averages[4], 64, "#,##0.00", "."));
        summary.append("\nPay Rate:");
        summary.append(Toolkit.leftPad(averages[5], 71, "#,##0.00", "."));

        // Extra information
        summary.append("\n\nEXTRA INFORMATION");
        summary.append("\n_________________");
        summary.append("\nEmployee count: ");
        summary.append(this.employees.length);
        summary.append(" (Max: ");
        summary.append(this.parameters.maxEmployees);
        summary.append(")");
        summary.append("\nSavings rate: ");
        summary.append(decimalFormat.format(
                this.parameters.savingsRate / 100.0));
        summary.append("\nIRA rate: ");
        summary.append(decimalFormat.format(
                this.parameters.iraRate / 100.0));
        summary.append("\nFederal withholding rate: ");
        summary.append(decimalFormat.format(
                this.parameters.federalWithholdingRate / 100.0));
        summary.append("\nState withholding rate: ");
        summary.append(decimalFormat.format(
                this.parameters.stateWithholdingRate / 100.0));

        // End report
        summary.append("\n\n");
        summary.append("-----------------------------------");
        summary.append("END REPORT");
        summary.append("-----------------------------------");

        // Method output
        return summary.toString();
    }

}