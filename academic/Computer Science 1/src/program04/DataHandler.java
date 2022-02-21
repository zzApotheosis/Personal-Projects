/*
 * Created by Steven Jennings on 13 September 2017.
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

package program04;

import java.io.*;
import java.util.InputMismatchException;
import java.util.LinkedList;
import java.util.Scanner;

/**
 * Handles all of the data processing for the assignment.
 *
 * @author Steven Jennings
 * @since 13 September 2017
 */
class DataHandler {

    // File names
    private final String INPUT_FILE_NAME = "resources/Main_04_Input.txt";
    private final String OUTPUT_FILE_NAME = "resources/Main_04_Output.txt";

    /*
     * 2D array for all program data
     * data[x][y]
     *
     * The format goes as follows:
     *     x  Col0      Col1         Col2           Col3     Col4
     * y    ___________________________________________________________
     * Row0 | grossPay0 savingsRate0 savingsAmount0 iraRate0 iraAmount0
     * Row1 | grossPay1 savingsRate1 savingsAmount1 iraRate1 iraAmount1
     * Row2 | grossPay2 savingsRate2 savingsAmount2 iraRate2 iraAmount2
     * etc...
     */
    double[][] data;

    double totalGrossPay;      // Total Gross Pay
    double avgGrossPay;        // Average Gross Pay
    double totalSavingsAmount; // Total Savings Amount
    double avgSavingsAmount;   // Average Savings Amount
    double totalIRAAmount;     // Total IRA Amount
    double avgIRAAmount;       // Average IRA Amount
    int numTotalLines;         // Number of lines in the input file
    int numValidLines;         // Number of valid lines in the input file
    double savedAndInvested;   // Total amount saved and invested

    /**
     * Constructor initializes data to 0.
     */
    DataHandler() {
        this.totalGrossPay = 0.0;
        this.avgGrossPay = 0.0;
        this.totalSavingsAmount = 0.0;
        this.avgSavingsAmount = 0.0;
        this.totalIRAAmount = 0.0;
        this.avgIRAAmount = 0.0;
        this.numTotalLines = 0;
        this.numValidLines = 0;
        this.savedAndInvested = 0.0;
    }

    /**
     * Gets data from the input file in the working directory.
     */
    void getInput() {
        // Input file
        File inputFile = new File(this.INPUT_FILE_NAME);

        // Prepare file scanner
        Scanner sc = null;
        try {
            sc = new Scanner(inputFile);
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }

        /*
         * Temporary variable to store data; later organized into data matrix
         *
         * By using this LinkedList, I have enabled a virtually infinite
         * number of potential lines to process in the input file,
         * assuming every single line is in
         * "[Gross pay] [Savings rate] [IRA rate]" format.
         */
        LinkedList<Double> tempNumBuffer = new LinkedList<>();

        // Fetch data from input file and count number of lines
        try {
            while (sc.hasNext()) {
                tempNumBuffer.add(sc.nextDouble()); // Gross Pay per line
                tempNumBuffer.add(sc.nextDouble()); // Savings Rate per line
                tempNumBuffer.add(sc.nextDouble()); // IRA Rate per line
                this.numTotalLines++;
            }
        } catch (NullPointerException | InputMismatchException e) {
            e.printStackTrace();
        } finally {
            // Close the scanner
            sc.close();
        }

        /*
         * Organize LinkedList into data matrix
         *
         * Assuming the input file has only 3 elements per line,
         * 3 existing elements + 2 calculated elements = 5 total elements
         * per line. So a data matrix of width 5, and height numTotalLines
         * is created.
         */
        this.data = new double[5][this.numTotalLines];
        for (int i = 0, j = 0; i < this.numTotalLines; i++) {
            this.data[0][i] = tempNumBuffer.get(j);
            j++;
            this.data[1][i] = tempNumBuffer.get(j);
            j++;
            this.data[3][i] = tempNumBuffer.get(j);
            j++;
        }
    }

    /**
     * Makes all calculations required to complete the assignment
     * within specifications.
     */
    void process() {
        // Calculate savings amount and IRA amount per line
        for (int y = 0; y < this.numTotalLines; y++) {
            if (lineIsValid(y)) {
                calculateLine(y);
                this.totalGrossPay += this.data[0][y];
                this.totalSavingsAmount += this.data[2][y];
                this.totalIRAAmount += this.data[4][y];
            } else {
                noCalculation(y);
            }
        }

        // Calculate averages
        if (this.numValidLines > 0) {
            this.avgGrossPay = this.totalGrossPay / this.numValidLines;
            this.avgSavingsAmount = this.totalSavingsAmount / this.numValidLines;
            this.avgIRAAmount = this.totalIRAAmount / this.numValidLines;
        } else {
            this.avgGrossPay = 0.0;
            this.avgSavingsAmount = 0.0;
            this.avgIRAAmount = 0.0;
        }

        // Calculate total saved and invested
        this.savedAndInvested = this.totalSavingsAmount + this.totalIRAAmount;
    }

    /**
     * Writes the output in a readable format.
     *
     * @param toConsole Enables output to console.
     * @param toFile    Enables output to file.
     */
    void writeOutput(boolean toConsole, boolean toFile) {
        // Cancel method if both boolean arguments are false
        if (!toConsole && !toFile) {
            return;
        }

        // Create output table
        String out = new OutputFormatter().createOutput(this);

        // If the argument toConsole is true, print to console
        if (toConsole) {
            System.out.print(out);
        }

        // If the argument toFile is true, write to file
        if (toFile) {
            // Create IO-related objects
            FileWriter fileWriter;
            BufferedWriter bufferedWriter;

            // Execute write operations
            try {
                fileWriter = new FileWriter(this.OUTPUT_FILE_NAME);
                bufferedWriter = new BufferedWriter(fileWriter);
                bufferedWriter.write(out);
                bufferedWriter.close();
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }

    /**
     * Performs the actual calculations for savings amount
     * and IRA amount on the given line.
     *
     * @param y The line at which the savings and IRA calculations
     *          should be made.
     */
    private void calculateLine(int y) {
        // Calculate the savings amount for the given line
        this.data[2][y] = this.data[0][y] * (this.data[1][y] / 100.0);

        // Calculate the IRA amount for the given line
        this.data[4][y] = this.data[0][y] * (this.data[3][y] / 100.0);
    }

    /**
     * Calculates whether or not the given line is valid.
     * <p>
     * The line is valid if it contains only positive elements.
     *
     * @param y The line to evaluate.
     * @return The validity of the given line.
     */
    private boolean lineIsValid(int y) {
        if (data[0][y] <= 0 || data[1][y] <= 0 || data[3][y] <= 0) {
            return false;
        }

        this.numValidLines++; // Increment valid lines if the line is valid

        return true;
    }

    /**
     * If there is an invalid line, this method makes sure to leave
     * its spot in the output table blank by assigning -1 to
     * its value in the data matrix.
     * <p>
     * Note: -1 is an arbitrary value, and has no special meaning.
     * When this program creates the output table, it checks each value
     * in the data matrix; if it is -1, then it leaves that spot blank
     * in the output table.
     *
     * @param y The line to evaluate.
     */
    private void noCalculation(int y) {
        data[2][y] = -1.0;
        data[4][y] = -1.0;
    }

}