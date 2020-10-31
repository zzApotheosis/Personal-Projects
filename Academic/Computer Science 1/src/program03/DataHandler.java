/*
 * Created by Steven Jennings on 06 September 2017.
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

package program03;

import java.util.NoSuchElementException;
import java.util.StringTokenizer;

/**
 * Handles all of the data processing for the assignment.
 *
 * @author Steven Jennings
 * @since 06 September 2017
 */
class DataHandler {

    // Declare class-wide variables/objects
    // This handles the messy JOptionPane code
    private JOPHandler msgBox;

    // User's input in string format
    private String userInput;

    // Gross pay, inputted by the user
    private double grossPay;

    // Savings rate, inputted by the user
    private double savingsRate;

    // IRA rate, inputted by the user
    private double iraRate;

    // Calculated savings amount
    private double savingsAmount;

    // Calculated IRA amount
    private double iraAmount;

    // Total amount saved and invested
    private double totalAmount;

    /**
     * Constructor initializes object data.
     */
    DataHandler() {
        this.msgBox = new JOPHandler();
        this.userInput = "";
        this.grossPay = 0.0;
        this.savingsRate = 0.0;
        this.iraRate = 0.0;
        this.savingsAmount = 0.0;
        this.iraAmount = 0.0;
        this.totalAmount = 0.0;
    }

    /**
     * Gets the input from the user
     * with a JOptionPane input dialog.
     */
    void getInput() {
        this.userInput = msgBox.showInputDialogBox();

        // Exit program if user cancels
        if (this.userInput == null) {
            System.exit(-1);
        }
    }

    /**
     * Calculates all data required.
     */
    void calculate() {
        // Break user input into the three variables
        StringTokenizer st = new StringTokenizer(this.userInput, " ");
        try {
            this.grossPay = Double.parseDouble(st.nextToken());
            this.savingsRate = Double.parseDouble(st.nextToken());
            this.iraRate = Double.parseDouble(st.nextToken());
        } catch (NumberFormatException | NoSuchElementException e) {
            // In case the user enters gibberish or nothing at all
            System.exit(-1);
        }

        // Calculate savings and IRA investment
        this.savingsAmount = grossPay * (savingsRate / 100.0);
        this.iraAmount = grossPay * (iraRate / 100.0);
        this.totalAmount = this.savingsAmount + this.iraAmount;
    }

    /**
     * Shows the output dialog box through
     * a JOptionPane message dialog.
     */
    void showOutput() {
        msgBox.showOutputDialogBox(this.grossPay, this.savingsRate, this.iraRate,
                this.savingsAmount, this.iraAmount, this.totalAmount);
    }

}