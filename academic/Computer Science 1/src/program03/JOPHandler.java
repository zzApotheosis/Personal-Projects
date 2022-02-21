/*
 * Created by Steven Jennings on 31 August 2017.
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

import javax.swing.*;
import java.text.DecimalFormat;

/**
 * Provides a nice interface for use with the JOptionPane.
 *
 * @author Steven Jennings
 * @since 31 August 2017
 */
class JOPHandler {

    // Declare class-wide variables
    // This is the dialog box object for IO
    private JOptionPane pane;
    // This is the decimal format to make the output nicely formatted
    private DecimalFormat df;

    /**
     * Constructor. No parameters, and only initializes
     * the class-wide varibles/objects.
     */
    JOPHandler() {
        this.pane = new JOptionPane();
        this.df = new DecimalFormat("#,##0.00");
    }

    /**
     * This is the primary method to use in order to
     * get input from the user.
     *
     * @return String This returns the user's input.
     */
    String showInputDialogBox() {
        return pane.showInputDialog(null,
                "Enter gross pay, " +
                        "savings rate (in percentage format),\n" +
                        "and IRA rate (in percentage format) " +
                        "separated by spaces. I.E. \"1500 10 10\"");
    }

    /**
     * This will properly display a formatted JOptionPane
     * for the user.
     *
     * @param grossPay      The gross pay.
     * @param savingsRate   The savings rate.
     * @param iraRate       The IRA rate.
     * @param savingsAmount The calculated savings amount.
     * @param iraAmount     The calculated IRA investment amount.
     * @param totalAmount   The calculated total amount.
     */
    void showOutputDialogBox(double grossPay, double savingsRate, double iraRate,
                             double savingsAmount, double iraAmount,
                             double totalAmount) {
        String out = "Gross pay: $" + df.format(grossPay)
                + "\nSavings rate: " + df.format(savingsRate) + "%"
                + "\nSavings amount: $" + df.format(savingsAmount)
                + "\nIRA rate: " + df.format(iraRate) + "%"
                + "\nIRA investment amount: $" + df.format(iraAmount)
                + "\nTotal of savings and IRA amounts: $"
                + df.format(totalAmount)
                + "\nAuthor: Steven Jennings";
        pane.showMessageDialog(null, out);
    }

}