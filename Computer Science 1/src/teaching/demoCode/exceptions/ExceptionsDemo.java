/*
 * Created by Steven Jennings on 08 November 2017.
 *
 *******************************************************************************
 *
 * Exceptions in Java are like fancy if statements. They are particularly
 * useful in Java when some piece of code in the program fails, and the
 * programmer needs to handle that failure in a certain way.
 *
 * When an exception occurs in a program, and it is NOT handled, then the whole
 * program ends there, regardless of whether or not the whole program was
 * completed. Handling exceptions in any programming language is very important,
 * because (like if statements) they determine the flow of the program.
 *
 * The best analogy to use for exceptions is the if statement, as we have
 * established. Exceptions handle the branching/flow of the program a lot like
 * if statements.
 */

package teaching.demoCode.exceptions;

/**
 * @author Steven Jennings
 * @version 0.0.0
 */
public class ExceptionsDemo {

    /**
     * The main method.
     *
     * @param args Unused.
     */
    public static void main(String[] args) {
        /*
         * Let's start with a basic example. This example is a bit more advanced
         * from the topics that are covered in other demonstrations, but the
         * syntax is pretty consistent. It goes like this:
         *
         * try {
         *     // Try to do stuff
         * } catch (<Exception type> <variable name>) {
         *     // Handle the exception
         * } finally {
         *     // Do more stuff
         * }
         *
         * If you've been around Java long enough, you've probably encountered
         * an ArrayIndexOutOfBoundsException when working with arrays. It's a
         * common error!
         *
         * Let's write a try/catch block to handle such an exception.
         */
        int[] myArray = new int[10];

        /*
         * Let's assign a bunch of values to our new array.
         */
        myArray[0] = -20;
        myArray[1] = 42; // The answer to life, the universe, and everything
        myArray[2] = 10;
        myArray[3] = -33;
        myArray[4] = 4;
        myArray[5] = 7;
        myArray[6] = 100;
        myArray[7] = 78;
        myArray[8] = -44;
        myArray[9] = 1234567890;

        /*
         * So now we have a full array of integers!
         *
         * BUT,
         *
         * What happens if we try to access an index that doesn't exist?
         * The syntax is perfectly valid, and the compiler will not detect
         * this error, but the error WILL be detected at runtime, which will
         * throw an exception at that point in the call stack!
         *
         * IMPORTANT: This statement WILL throw an exception!!! Surround it
         * in a try/catch block to properly handle the error. Or simply comment
         * the statement out of the program so it doesn't compile to bytecode.
         *
         * I.E.
         * try {
         *     myArray[10] = 0;
         * } catch (ArrayIndexOutOfBoundsException e) {
         *     System.err.println("Array out of bounds");
         * }
         */
        myArray[10] = 0;

        /*
         * The proper way to handle exceptions is always through try/catch
         * blocks. Let's write a simple try/catch block using basic syntax.
         */
        try {
            // Literally trying to execute some code
            myArray[10] = 0;
        } catch (ArrayIndexOutOfBoundsException e) {
            // In case an exception occurs in the try block, handle it in some way
            System.err.println(e.getMessage());
        }

        /*
         * The example shown above is always the way to handle exceptions.
         * A try/catch block must be written to ensure any errors in the
         * program are handled correctly and appropriately.
         *
         * An optional clause in a try/catch block is the "finally" block.
         * The finally block always executes after the try/catch block is
         * executed.
         *
         * For all intents and purposes, there is no difference
         * between the finally clause and simply writing code after the whole
         * try/catch block. The reason the finally clause is used in a
         * trt/catch block is to serve as "clean-up" code, for when the
         * programmer needs to finish some sort of task, but keep the code in
         * a nice readable format.
         */
        try {
            // Literally trying to execute some code
            myArray[11] = 30;
        } catch (ArrayIndexOutOfBoundsException e) {
            // Handle the exception in some way
            e.printStackTrace();
        } finally {
            System.out.println("This statement always executes!");
        }

        /*
         * The above example executes exactly the same as the following code.
         */
        try {
            // Literally trying to execute some code
            myArray[11] = 30;
        } catch (ArrayIndexOutOfBoundsException e) {
            // Handle the exception in some way
            e.printStackTrace();
        }
        System.out.println("This statement always executes!");

        /*
         * That wraps up just about everything you need to know about exceptions
         * in Computer Science in general. Exceptions exist in many high-level
         * programming languages; they are not exclusive to Java.
         *
         * The important information to know is this:
         * Exceptions are imperative to handling errors that occur in a
         * program. When the programmer needs to write code that is likely to
         * create an error, that error should be handled appropriately in a
         * try/catch block.
         */
    }

}