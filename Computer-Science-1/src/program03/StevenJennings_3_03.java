/*
 * Created by Steven Jennings on 31 August 2017.
 *
 * Program #3 (Revision #2), CS 1050, Section 3
 *
 * Assignment description:
 * Calculate gross pay, savings, and investment using JOptionPane
 * for input and output.
 *
 * System Information:
 * Operating System: Arch Linux (amd64)
 * Official Oracle JDK Version: Version 8, Update 152 (build 1.8.0_152-b16)
 * IDE Version: IntelliJ IDEA 2017.2.5
 *
 * Extra notes:
 * Written from scratch. First time implementing Javadoc tags!
 * This is the second revision of the original submitted .java file.
 *
 * Vocabulary word of choice:
 * Kairos
 * (n) A propitious moment for decision or action.
 * (n) The right, critical, or opportune moment.
 *
 * Quote of choice:
 * "Just as we believe by faith that the greatest happiness of the next life
 * consists simply in the contemplation of this divine majesty, likewise we
 * experience that we derive the greatest joy of which we are capable in this
 * life; to give her the dick, even though it is much less perfect."
 * - Rene Descartes
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

/**
 * The main class for program 3.
 * <p>
 * Calculates gross pay, savings, and investment using JOptionPane
 * for input and output.
 *
 * @author Steven Jennings
 * @version 0.0.5
 * @since 31 August 2017
 */
public class StevenJennings_3_03 {

    /**
     * This is the main method, with only a few method calls.
     * Everything else is handled through objects.
     *
     * @param args Unused.
     */
    public static void main(String[] args) {
        // Initialize variables/objects
        DataHandler dataHandler = new DataHandler();

        // Get user input
        dataHandler.getInput();

        // Calculate savings and IRA investment amounts.
        dataHandler.calculate();

        // Show output
        dataHandler.showOutput();
    }

}