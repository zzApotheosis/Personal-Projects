/*
 * Created by Steven Jennings on 25 October 2017.
 */

package teaching.demoCode.methods;

import java.util.InputMismatchException;
import java.util.Scanner;

/**
 * @author Steven Jennings
 * @version 0.0.0
 * @since 25 October 2017
 */
public class MethodsDemo {

    /**
     * The main method.
     *
     * @param args Unused.
     */
    public static void main(String[] args) {
        /*
         * Here's a simple method that just explains the program!
         *
         * The actual code can be found below, just under the main method.
         * The method is called "explain". Pretty self-explanatory, eh?
         */
        explain();

        /*
         * Let's start with a mathematical exponent example.
         *
         * In Java, you can do basic arithmetic operations like addition,
         * subtraction, division, and multiplication, but you can't do
         * something that commonly appears in algebra: exponents.
         *
         * We can write a method that calculates powers for us!
         *
         * Let's start by defining some variables that we will manipulate
         * with methods.
         */
        int x = 3;     // Our variable
        int exponent = 2; // The myExponential to raise x to
        int answer;    // A variable to hold the answer

        /*
         * We have a pretty basic set of variables to work with now.
         *
         * Let's run our numbers through the myExponential() method and store the
         * answer in our variable "answer".
         */
        answer = myExponential(x, exponent);
        System.out.println(x + "^" + exponent + " = " + answer);

        // *********************************************************************

        /*
         * Next, let's try another easy scenario.
         *
         * Let's say we want to find the average of two numbers, but we don't
         * want to put the calculations within our main code. It would look
         * too messy, and probably not very easy to understand if the project
         * was large and complex enough.
         *
         * Let's make some variables to work with.
         */
        double num1 = 50.0; // Our first number is 50.0
        double num2 = 30.0; // Our second number is 30.0
        double averageOfTwoNums;     // Our average between the two numbers

        /*
         * Now, let's run our numbers through a method that calculates
         * the average for us.
         */
        averageOfTwoNums = average(num1, num2);
        System.out.println("The average of " + num1 + " and " + num2 + " is " + averageOfTwoNums);

        // *********************************************************************

        /*
         * Now, let's try writing a slightly more complex method.
         *
         * Let's say that we wanted a method to calculate the average of
         * MULTIPLE values at once. Instead of writing n number of methods for
         * n number of numbers we want to average, let's write a method that
         * accepts a single array of values to average.
         *
         * Remember: In Java, an array is an object that holds multiple values
         * within itself, and can be accessed per index of the array. See my
         * ArraysDemo for more information.
         *
         * Let's make some variables to work with this example.
         */
        double[] collectionOfNumbers = new double[]{
                0.0,
                10.0,
                3.14,   // pi!
                2.72,   // e!
                1337.0, // Gamers, you know what this means!
                1.414,  // Pythagoras' constant! More accurately: Math.sqrt(2.0)
                1.618,  // The golden ratio!
                4.669,  // Feigenbaum constant
                37.0    // Permittivity of ethylene glycol at room temperature
        };
        double averageOfManyNums;

        /*
         * So now we have an array of numbers. Let's run it through a slightly
         * more complex method which will calculate the average of all
         * the values in the array!
         *
         * Remember: This method is useful for when you want to calculate
         * the average of ANY number of values that you have!
         */
        averageOfManyNums = average(collectionOfNumbers);
        System.out.println("The average of my array is " + averageOfManyNums);

        /*
         * You may notice that the "average" method is defined twice. Blasphemy,
         * right?
         *
         * It's actually not blasphemy. In Java, this is called
         * "method overloading" and it is particularly useful when you want
         * to process data differently, but use the same English-syntax meaning
         * across your program.
         *
         * In our example, we have written a method called "average" which
         * averages two simple values. We have also written a method called
         * "average" which calculates the average of all the values in an
         * input array of doubles.
         *
         * It's easy to see that you might want to use the simpler method
         * to calculate the average between two numbers, but you might
         * want to use the more complex method which calculates the average
         * of all elements in an array.
         *
         * It really just depends on the task at hand, and what you're trying
         * to accomplish.
         */

        // *********************************************************************

        /*
         * For a final demonstration, let's say we're writing a program that
         * depends on what the user inputs to the program. Using the same
         * reasoning as we did previously, it would be good practice to write
         * a separate method that handles the getting of user input.
         *
         * Let's make a few variables for this demonstration.
         */
        double userInput1;
        double userInput2;
        double userInputResult;

        /*
         * We have two userInput variables, both doubles. And we have one
         * userInputResult variable, also type double. We're going to use the
         * same method in the first demo of this program, and average the two
         * numbers.
         */
        userInput1 = getUserInput();
        userInput2 = getUserInput();
        userInputResult = average(userInput1, userInput2);
        System.out.print("The average of " + userInput1 + " and " + userInput2);
        System.out.println(" is " + userInputResult);

        /*
         * Hopefully this has been a good demonstration for how methods work
         * in Java. Feel free to play with the existing code, or even write
         * your own!
         *
         * More information about methods can be found in the reference pdf.
         */
    }

    /**
     * Explains the program to the user.
     */
    private static void explain() {
        System.out.println("This is a methods demonstration!");
        System.out.println("Hopefully this helps with understanding how methods work!");
        System.out.println("Author: Steven Jennings\n");
    }

    /**
     * Calculates the value of a base number raised to an exponential myExponential.
     * <p>
     * Note: The built-in Math class has a better implementation of this
     * algorithm, but for the sake of example, we have written our own method.
     *
     * @param base        The base number value.
     * @param raisedPower The exponent value.
     */
    private static int myExponential(int base, int raisedPower) {
        // Check for certain conditions
        if (base == 0) {
            return 0; // Don't even bother with the calculations if the base is 0
        } else if (base == 1) {
            return 1; // Likewise, just return a 1 if the base is 1
        } else if (base == -1) {
            // Check for even or odd myExponential
            if (raisedPower % 2 == 1) {
                return -1; // Return -1 if the raised myExponential is an odd number
            } else {
                return 1; // Return 1 if the raised myExponential is an even number
            }
        }

        // Define method variables
        int original = base; // The original value of base

        // Perform calculation
        for (int i = 1; i < raisedPower; i++) {
            base *= original;
        }

        // Method output
        return base;
    }

    /**
     * Calculates the average between two values. Pretty simple!
     *
     * @param x0 The first value in the calculations.
     * @param x1 The second value in the calculations.
     * @return The average of the two input values.
     */
    private static double average(double x0, double x1) {
        // Declare method variables
        double out;

        // Perform calculation
        out = (x0 + x1) / 2.0;

        // Method output
        return out;
    }

    /**
     * Calculates the average of all elements in an array of doubles.
     *
     * @param manyNums The input array to process.
     * @return The average of all elements in the input array.
     */
    private static double average(double[] manyNums) {
        // Declare method variables
        double out = 0.0;   // Output variable

        /*
         * Accumulate all values into one total value.
         *
         * Also, we could have used a "for each" loop here, but let's just
         * stick with the standard for loop.
         */
        for (int i = 0; i < manyNums.length; i++) {
            out += manyNums[i];
        }

        // Divide the total value by the number of values in the array
        out /= manyNums.length;

        // Method output
        return out;
    }

    /**
     * Gets a number value from the user.
     *
     * @return Any number value given by the user.
     */
    private static double getUserInput() {
        // Declare method variables and objects
        Scanner sc;
        double out;

        /*
         * Get user input.
         *
         * Notice: If you run the unmodified program, this is where the program
         * "pauses" to get the user input!
         */
        while (true) {
            // Create a clean scanner, in case the loop happens more than once
            sc = new Scanner(System.in);

            // Prompt for number
            System.out.print("Enter any number value: ");

            // Try to fetch the number from user
            try {
                out = sc.nextDouble();
                break;
            } catch (InputMismatchException e) {
                System.out.println("Enter an actual value!");
            }
        }

        // Method output
        return out;
    }

}