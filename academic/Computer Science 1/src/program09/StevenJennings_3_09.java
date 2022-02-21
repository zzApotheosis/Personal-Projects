/*
 * Created by Steven Jennings on 09 October 2017.
 *
 * Program #9, CS 1050, Section 3
 *
 * Assignment description:
 * Calculate savings and investment amounts using Java classes. This builds on
 * the ideas in Assignments #2 and #5.
 *
 * System Information:
 * Operating System: Arch Linux (amd64)
 * Official Oracle JDK Version: Version 8, Update 152 (build 1.8.0_152-b16)
 * IDE Version: IntelliJ IDEA 2017.2.5
 *
 * Extra notes:
 * Written from scratch. If it was up to me, I'd rather name my external class
 * much differently (probably something like DataHandler). But for the sake of
 * organization for professor Kramer, it shall be named
 * "StevenJennings_3_09_Calc"
 *
 * Vocabulary word of choice:
 * Zephyr
 * (n) A soft, gentle breeze.
 * (n) A fine cotton gingham.
 *
 * Quote of choice:
 * "While there's life, there's hope." - Cicero
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

/**
 * The main class for program 9.
 * <p>
 * Calculates savings and investment amounts using Java classes. This builds on
 * the ideas in Assignments #2 and #5.
 *
 * @author Steven Jennings
 * @version 0.0.0
 * @since 09 October 2017
 */
public class StevenJennings_3_09 {

    /**
     * The main method.
     *
     * @param args Unused.
     */
    public static void main(String[] args) {
        // Explain the program
        explain();

        // Instantiate object
        StevenJennings_3_09_Calc myObject = new StevenJennings_3_09_Calc();

        // Get user input. Only allows values greater than 0
        myObject.getInput();

        // Perform calculations
        myObject.calculateSavings();
        myObject.calculateIRA();
        myObject.calculateTotal();

        // Output the results to terminal
        myObject.output();
    }

    // *************************************************************************

    /**
     * Explains the program.
     */
    private static void explain() {
        System.out.println("This program calculates savings and IRA amounts " +
                "based on gross pay.\nAuthor: Steven Jennings\n");
    }

}