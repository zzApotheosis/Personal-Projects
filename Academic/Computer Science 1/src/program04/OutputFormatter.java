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

import providedCode.Toolkit;

/**
 * Provides a nice interface to use for the output formatting.
 *
 * @author Steven Jennings
 * @since 13 September 2017
 */
class OutputFormatter {

    /**
     * Constructor.
     */
    OutputFormatter() {
    }

    /**
     * This creates the output string so that it's easy to print/write
     * the output data.
     * <p>
     * Also, note that this method is very buggy. It will work perfectly
     * for this specific assignment, but will probably look terrible
     * with other input files due to this method's lack of organization.
     *
     * @param in A copy of the DataHandler class for all the data.
     * @return The properly formatted string object for program output.
     */
    static String createOutput(DataHandler in) {
        // Output StringBuilder
        StringBuilder stringBuilder = new StringBuilder();

        // Toolkit for neatness
        Toolkit toolkit = new Toolkit();

        // Append header
        stringBuilder.append("Gross Pay   Savings Rate   Savings Amount" +
                "   IRA Rate   IRA Amount");

        // Append Divider
        stringBuilder.append("\n_________   ____________   ______________" +
                "   ________   __________");

        // Append Data
        for (int y = 0; y < in.numTotalLines; y++) {
            stringBuilder.append("\n"); // New line

            // Gross Pay
            stringBuilder.append(toolkit.leftPad(in.data[0][y], 9, "0.00"));
            stringBuilder.append("   ");

            // Savings Rate
            stringBuilder.append(toolkit.leftPad(in.data[1][y], 12, "0.00"));
            stringBuilder.append("   ");

            // Savings Amount
            if (in.data[2][y] == -1.0) {
                stringBuilder.append("              "); // 14 spaces
            } else {
                stringBuilder.append(toolkit.leftPad(in.data[2][y], 14, "0.00"));
            }
            stringBuilder.append("   ");

            // IRA Rate
            stringBuilder.append(toolkit.leftPad(in.data[3][y], 8, "0.00"));
            stringBuilder.append("   ");

            // IRA Amount
            if (in.data[4][y] == -1.0) {
                stringBuilder.append("          "); // 10 spaces
            } else {
                stringBuilder.append(toolkit.leftPad(
                        in.data[4][y], 10, "0.00"));
            }
        }

        // Append output summary
        stringBuilder.append("\n\nNumber of data lines read: ");
        stringBuilder.append(in.numTotalLines);
        stringBuilder.append("\nNumber of valid data lines: ");
        stringBuilder.append(in.numValidLines);
        stringBuilder.append("\n\n                Totals    Averages");
        stringBuilder.append("\n             _________    ________");
        stringBuilder.append("\nGross Pay");
        stringBuilder.append("   "); // 3 spaces
        stringBuilder.append(toolkit.leftPad(in.totalGrossPay, 10, "#,##0.00"));
        stringBuilder.append("  "); // 2 spaces
        stringBuilder.append(toolkit.leftPad(in.avgGrossPay, 10, "#,##0.00"));
        stringBuilder.append("\nSavings");
        stringBuilder.append("     "); // 5 spaces
        stringBuilder.append(toolkit.leftPad(in.totalSavingsAmount, 10, "#,##0.00"));
        stringBuilder.append("  "); // 2 spaces
        stringBuilder.append(toolkit.leftPad(in.avgSavingsAmount, 10, "#,##0.00"));
        stringBuilder.append("\nIRA");
        stringBuilder.append("         "); // 9 spaces
        stringBuilder.append(toolkit.leftPad(in.totalIRAAmount, 10, "#,##0.00"));
        stringBuilder.append("  "); // 2 spaces
        stringBuilder.append(toolkit.leftPad(in.avgIRAAmount, 10, "#,##0.00"));
        stringBuilder.append("\nSavings+IRA");
        stringBuilder.append(" "); // 1 space
        stringBuilder.append(toolkit.leftPad(in.savedAndInvested, 10, "#,##0.00"));

        return stringBuilder.toString();
    }

}