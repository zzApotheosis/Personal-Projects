/*
 * Created by Steven Jennings on 23 October 2017.
 *
 *******************************************************************************
 *
 * Arrays are powerful, but they can be tricky. The best real-world analogy
 * to relate arrays would be real-world chests. A chest is an object that can
 * hold or store other objects. In Java, there is only one built-in tool
 * to represent a real-world chest, and that is an array.
 *
 * An array in Java can hold multiple values depending on how large of an array
 * you create. This is called the size or length of the array. If you want to
 * store 10 values (of the same data type), you should create an array of size
 * 10 (also of the same data type).
 *
 * It should be noted that arrays can only store the data type that they are
 * created to store. For instance,
 *
 * double[] doubleArray
 *
 * would NOT be able to store a String value like "Hello World!" because
 * doubleArray is an array of only doubles, as denoted by "double[]".
 *
 * Note: You may notice this declaration is in the same "format" as in the
 * Classes, Objects, and Instances demonstration! Every single declaration in
 * Java is in this format:
 * <Type> <Reference Variable Name>;
 *
 * or for non-primitive data types:
 * <Type> <Reference Variable Name> = new <Constructor Name>();
 *
 * or for primitive data types:
 * <Type> <Reference Variable Name> = <Literal>;
 *
 * where <Literal> is a primitive data value like an int (3) or double (3.0).
 *
 * Now let's look at a demonstration.
 */

package teaching.demoCode.arrays;

/**
 * @author Steven Jennings
 * @version 0.0.0
 * @since 23 October 2017
 */
public class ArraysDemo {

    public static void main(String[] args) {
        /*
         * First, let's make some reference variables for our arrays.
         */
        int[] integerArray;
        double[] doubleArray;
        String[] stringArray;

        /*
         * Right now, we have just told the compiler "Hey compiler, we want to
         * have these three reference variables, each one with type int[],
         * double[], and String[]." So if we were to run the program with just
         * these three lines of code, it would just create these reference
         * variables in memory, and that's it. Pretty pointless!
         *
         * Let's go ahead and actually create the arrays, and assign some
         * values to them.
         */
        integerArray = new int[]{ // Size 5
                0, // Index 0
                1, // Index 1
                2, // Index 2
                3, // Index 3
                4  // Index 4
        };
        doubleArray = new double[]{ // Size 5
                5.0, // Index 0
                6.0, // Index 1
                7.0, // Index 2
                8.0, // Index 3
                9.0  // Index 4
        };
        stringArray = new String[]{ // Size 3
                "Hello", // Index 0
                " ",     // Index 1
                "World!" // Index 2
        };

        /*
         * Note: This is the literal way to initialize arrays with pre-written
         * values. Normally, you would just want to declare the array with a
         * given length or size like so:
         *
         * integerArray = new int[5];
         * doubleArray = new double[5];
         * stringArray = new String[3];
         *
         * Let's check out the contents of our new arrays, starting with the
         * first index in each of our three arrays. Also, don't worry about this
         * type of loop. It's called a "for each" loop. In this case, it just
         * prints out the value "for each" index in the integerArray, and
         * vice versa.
         */
        for (int value : integerArray) {
            System.out.println(value);
        }
        for (double value : doubleArray) {
            System.out.println(value);
        }
        for (String value : stringArray) {
            System.out.print(value);
        }
        System.out.println(); // Extra line for neatness

        /*
         * At this point, we should have seen this in console:
         * 0
         * 1
         * 2
         * 3
         * 4
         * 5.0
         * 6.0
         * 7.0
         * 8.0
         * 9.0
         * Hello World!
         *
         * because these values are stored at different indexes in our arrays!
         *
         * Now, let's modify some values in our arrays.
         */
        integerArray[0] = 10;       // Set the value at index 0 to 10
        integerArray[1] = 11;       // Set the value at index 1 to 11
        integerArray[2] = 12;       // Set the value at index 2 to 12
        doubleArray[0] = 15.0;      // Set the value at index 0 to 15.0
        doubleArray[1] = 16.0;      // Set the value at index 1 to 16.0
        doubleArray[2] = 17.0;      // Set the value at index 2 to 17.0
        stringArray[0] = "Goodbye"; // Set the value at index 0 to "Goodbye"
        stringArray[1] = " ";       // Set the value at index 1 to " "
        stringArray[2] = "World!";  // Set the value at index 2 to "World!"

        /*
         * Now that we have modified these values in our arrays, they will
         * be reflected when we access these values through a simple test
         * like System output.
         */
        for (int value : integerArray) {
            System.out.println(value);
        }
        for (double value : doubleArray) {
            System.out.println(value);
        }
        for (String value : stringArray) {
            System.out.print(value);
        }
        System.out.println(); // Extra line for neatness

        /*
         * Since we modified the values in the arrays, they are properly stored
         * in memory as you can see from the console output!
         *
         * Another thing to note: You can fetch the length of any array like so:
         * int length = myArray.length;
         * assuming your array is called "myArray"
         *
         * These are called the properties of the object (better known as fields)
         * and these properties exist for every instance of the object! See
         * my ClassObjectInstanceDemo program for more information about that.
         */
        System.out.println("Length of integerArray: " + integerArray.length);
        System.out.println("Length of doubleArray: " + doubleArray.length);
        System.out.println("Length of stringArray: " + stringArray.length);

        /*
         * This should be a good start on how arrays work in Java.
         * Feel free to play around with the code and experiment with it
         * yourself!
         */
    }

}