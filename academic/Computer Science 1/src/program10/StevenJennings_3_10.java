/*
 * Created by Steven Jennings on 19 October 2017.
 *
 * Program #10, CS 1050, Section 3
 *
 * Assignment description:
 * Calculate payroll data using classes.
 *
 * System Information:
 * Operating System: Arch Linux (amd64)
 * Official Oracle JDK Version: Version 8, Update 152 (build 1.8.0_152-b16)
 * IDE Version: IntelliJ IDEA 2017.2.5
 *
 * Extra notes:
 * Written from scratch. For anyone reading this, please note that I
 * added an extra field in the Employee class to keep track of wealth,
 * which is Savings Amount + IRA Investment.
 *
 * Vocabulary word of choice:
 * Apollo
 * (n) Greek God of music, arts, knowledge, healing, plague, prophecy, poetry,
 * manly beauty, and archery.
 *
 * Quote of choice:
 * "Never discourage anyone [...] who continually makes progress,
 * no matter how slow." - Plato
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

/**
 * The main class for program 10.
 * <p>
 * Calculates payroll data using classes.
 *
 * @author Steven Jennings
 * @version 0.0.0
 * @since 19 October 2017
 */
public class StevenJennings_3_10 {

    /**
     * The main method, with only a few method calls.
     *
     *
     * @param args Unused.
     */
    public static void main(String[] args) {
        // Create data handler
        DataHandler dataHandler = new DataHandler();

        // Explains the program
        explain();

        // Get input from file
        dataHandler.getInput();

        // Process data
        dataHandler.process();

        // Execute output
        dataHandler.output();
    }

    // *************************************************************************

    /**
     * Explains the program.
     */
    private static void explain() {
        System.out.println("This program calculates employee payroll data" +
                " using classes.\nAuthor: Steven Jennings\n");
    }

}