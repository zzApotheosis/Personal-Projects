package providedCode;

/**
 * Interactive Average Main - Average numbers using a class
 * <p>
 * This program asks the user to input real numbers,
 * calculates the average of these numbers, and prints the results.
 * It uses the class InteractiveAverageClass to hold the numbers.
 * This is Program Assignment #1 revised to use a class.
 * <p>
 * Methods used:
 * <p>
 * explain() - explain the program to the user
 * outputResults(InteractiveAverageClass theNumbers, double average) -
 * output the numbers and their average
 * <p>
 * Class methods used
 * getNumbers() - get values for the numbers obtained from the user
 * calcAvg() - return the average of the numbers
 * outputNumbers() - output the values of the numbers and a message
 * <p>
 * Notice that this main program does not know how many numbers are
 * involved. Therefore, the InteractiveAverageClass could be changed
 * to process any number of numbers and still use this main program!
 */

public class InteractiveAverageMain {

    public static void main(String[] args) {
        InteractiveAverageClass theNumbers =
                new InteractiveAverageClass();    // Input values: real numbers
        double average;                        // Average of input values

        // Explain the program to the user
        explain();

        // Input the numbers
        theNumbers.getNumbers();

        // Determine the average of the numbers
        average = theNumbers.calcAverage();

        // Output the results. Notice how a *class* name is passed as a parameter
        outputResults(theNumbers, average);
    } // End main

    // ***************************************************************

    // Explain the program to the user
    public static void explain() {
        System.out.println(
                "This program averages real numbers which are entered by the user.\n" +
                        "The output is the numbers and their average.\n");
    } // End explain

    // ***************************************************************

    // Output the values of the two numbers and their average
    public static void outputResults(
            InteractiveAverageClass numbers, double avg) {
        numbers.outputNumbers();
        System.out.println("\nThe average is " + avg);
    } // End outputResults
} // End class