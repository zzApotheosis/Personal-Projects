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

package program09;

import providedCode.Toolkit;

import java.util.InputMismatchException;
import java.util.Scanner;

/**
 * The calculation class for use in program 9.
 *
 * @author Steven Jennings
 * @version 0.0.0
 * @since 19 October 2017
 */
class StevenJennings_3_09_Calc {

    // Class fields
    private double grossPay;      // Gross pay
    private double savingsRate;   // Savings rate (in percentage format)
    private double iraRate;       // IRA rate (in percentage format)
    private double savingsAmount; // Savings amount
    private double iraAmount;     // IRA amount
    private double totalAmount;   // Savings and IRA investment combined

    /**
     * A constructor that initializes the instance variables to default values
     * of 0. This constructor has no parameters.
     * <p>
     * Special note for any students: constructors DO NOT need a return type!
     */
    StevenJennings_3_09_Calc() {
        this.grossPay = 0.0;
        this.savingsRate = 0.0;
        this.iraRate = 0.0;
        this.savingsAmount = 0.0;
        this.iraAmount = 0.0;
        this.totalAmount = 0.0;
    }

    // *************************************************************************

    /**
     * A constructor with three parameters that initializes the instance
     * variables to the parameter values. This constructor is intentionally
     * unused, as the specification requires.
     *
     * @param initGP The initial value of gross pay.
     * @param initSR The initial value of the savings rate.
     * @param initIR The initial value of the IRA rate.
     */
    StevenJennings_3_09_Calc(double initGP, double initSR, double initIR) {
        this.grossPay = initGP;
        this.savingsRate = initSR;
        this.iraRate = initIR;
        this.savingsAmount = 0.0;
        this.iraAmount = 0.0;
        this.totalAmount = 0.0;
    }

    // *************************************************************************

    /**
     * A method for the user to input gross pay, savings rate, and IRA investment
     * rate. Include prompts. Allow values only greater than 0.
     */
    void getInput() {
        // Create method objects and variables
        Scanner sc = new Scanner(System.in);

        // Get the gross pay from the user through console
        System.out.print("Enter the gross pay: ");
        try {
            this.grossPay = sc.nextDouble();
            if (this.grossPay <= 0) {
                throw new NumberFormatException("Only enter values greater than 0!");
            }
        } catch (InputMismatchException | NumberFormatException e) {
            e.printStackTrace();
            System.exit(-1);
        }

        // Get the Savings rate from the user through console
        System.out.print("Enter the savings rate %: ");
        try {
            this.savingsRate = sc.nextDouble();
            if (this.savingsRate <= 0) {
                throw new NumberFormatException("Only enter values greater than 0!");
            }
        } catch (InputMismatchException | NumberFormatException e) {
            e.printStackTrace();
            System.exit(-1);
        }

        // Get the IRA rate from the user through console
        System.out.print("Enter the IRA rate %: ");
        try {
            this.iraRate = sc.nextDouble();
            if (this.iraRate <= 0) {
                throw new NumberFormatException("Only enter values greater than 0!");
            }
        } catch (InputMismatchException | NumberFormatException e) {
            e.printStackTrace();
            System.exit(-1);
        }

        // Finalize IO operations
        sc.close();
    }

    // *************************************************************************

    /**
     * Performs the calculation for the savings amount.
     */
    void calculateSavings() {
        this.savingsAmount = this.grossPay * (this.savingsRate / 100.0);
    }

    // *************************************************************************

    /**
     * Performs the calculation for the IRA amount.
     */
    void calculateIRA() {
        this.iraAmount = this.grossPay * (this.iraRate / 100.0);
    }

    // *************************************************************************

    /**
     * Calculates the total of both savings amount and IRA investment amount.
     */
    void calculateTotal() {
        this.totalAmount = this.iraAmount + this.savingsAmount;
    }

    // *************************************************************************

    /**
     * Outputs the results of the program.
     */
    void output() {
        // Execute output
        System.out.println(); // New line for neatness.
        System.out.println(
                "Gross pay:" +
                        Toolkit.leftPad(this.grossPay, 39, "$#,##0.00", "."));
        System.out.println(
                "Savings rate %:" +
                        Toolkit.leftPad(this.savingsRate / 100.0, 34, "#,##0.0%", "."));
        System.out.println(
                "IRA rate %:" +
                        Toolkit.leftPad(this.iraRate / 100.0, 38, "#,##0.0%", "."));
        System.out.println(
                "Savings amount:" +
                        Toolkit.leftPad(this.savingsAmount, 34, "$#,##0.00", "."));
        System.out.println(
                "IRA amount:" +
                        Toolkit.leftPad(this.iraAmount, 38, "$#,##0.00", "."));
        System.out.println(
                "Total of savings and IRA amounts:" +
                        Toolkit.leftPad(this.totalAmount, 16, "$#,##0.00", "."));
    }

}