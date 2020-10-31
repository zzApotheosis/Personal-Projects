/*
 * Created by Steven Jennings on 01 September 2017.
 *
 * Program #4, CS 1050, Section 3
 *
 * Assignment description:
 * Calculate gross pay, savings, and investment using
 * files for input and output.
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
 * Paracosm
 * (n) A detailed imaginary world.
 * (n) A prolonged fantasy world invented by children; can have a definite
 * geography and language and history.
 *
 * Quote of choice:
 * "Chance is always powerful. Let your hook be always cast; in the pool
 * where you least expect it, there will be a fish." - Ovid
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

/**
 * The main class for program 4.
 * <p>
 * Calculates gross pay, savings, and investment using
 * files for input and output.
 *
 * @author Steven Jennings
 * @version 0.0.0
 * @since 01 September 2017
 */
public class StevenJennings_3_04 {

    /**
     * The main method has only a few method calls
     * to execute the whole program.
     *
     * @param args Unused.
     */
    public static void main(String[] args) {
        // Declare data handler object
        DataHandler dataHandler = new DataHandler();

        // Get program input
        dataHandler.getInput();

        // Process data
        dataHandler.process();

        // Write to output file
        dataHandler.writeOutput(true, true);
    }

}