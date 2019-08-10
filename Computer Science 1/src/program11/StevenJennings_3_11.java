/*
 * Created by Steven Jennings on 19 October 2017.
 *
 * Program #11, CS 1050, Section 3
 *
 * Assignment description:
 * Work with ArrayLists: Sum and average ArrayList entries.
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
 * Chicken
 * (n) A domestic fowl kept for its eggs or meat, especially a young one.
 *
 * Quote of choice:
 * "The key to everything is patience. You get the chicken by hatching the egg,
 * not by smashing it." - Arnold H. Glasow
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

package program11;

/**
 * The main class for program 11.
 *
 * Works with ArrayLists. Calculates the total and average of elements in
 * an ArrayList.
 *
 * @author Steven Jennings
 * @version 0.0.0
 * @since 19 October 2017
 */
public class StevenJennings_3_11 {

    /**
     * The main method.
     *
     * @param args Unused.
     */
    public static void main(String[] args) {
        // Create variables and objects
        DataHandler dataHandler = new DataHandler();

        // Explain the program
        explain();

        // Get input
        dataHandler.getInput();

        // Process data
        dataHandler.process();

        // Output
        dataHandler.output();
    }

    // *************************************************************************

    /**
     * Explains the program to the user.
     */
    private static void explain() {
        System.out.println("This program calculates the sum and average" +
                "\nof the entries in an ArrayList." +
                "\nAuthor: Steven Jennings\n");
    }

}