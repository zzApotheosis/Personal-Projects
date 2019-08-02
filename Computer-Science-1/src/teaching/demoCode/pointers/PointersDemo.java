/*
 * Created by Steven Jennings on 14 November 2017.
 *
 *******************************************************************************
 *
 * A very important concept to understand in Computer Science is the concept
 * of pointers. Here is a condensed definition: A pointer is a value in memory
 * that "points" to another memory location. This value is typically represented
 * as a variable in a program.
 *
 * In C and C++, pointers can be explicitly defined by the programmer, and even
 * the pointer's value can be modified by the programmer.
 *
 * Pointers work a little bit differently in Java. In Java, the programmer can
 * NOT explicitly define pointers; they are handled by the Java compiler and
 * the Java Virtual Machine. But that doesn't mean pointers don't exist in Java
 * code. A programmer can take advantage of pointers in Java just like any
 * other language, but this approach is different from a language like C or C++.
 */

package teaching.demoCode.pointers;

/**
 * @author Steven Jennings
 * @version 0.0.0
 */
public class PointersDemo {

    /**
     * The main method.
     *
     * @param args Unused.
     */
    public static void main(String[] args) {
        /*
         * Let's start with a basic pointers example. Assume that you want to
         * modify the contents of an array inside a method without creating
         * a new array. We can do this with pointers.
         */
        double[] myArray = new double[]{
                1.0,
                42.0, // The answer to life, the universe, and everything
                13.37,
                1.337,
                -133.7,
                20.0,
                3.14,
                -0.987654321
        };

        /*
         * So we have created a basic array of Doubles. Note, these are Double
         * objects, not the primitive data type "double".
         *
         * Let's check their values using an enhanced for loop.
         */
        for (double value : myArray) {
            System.out.println("BEFORE: " + value);
        }

        /*
         * At this point, we have confirmed that our array is indeed filled with
         * the values defined above.
         *
         * Now, this part is very important. Let's say we want to modify our
         * array in some way, but we want to modify the array in a separate
         * method instead of doing it in the main method. We use separate
         * methods because it makes for cleaner code.
         *
         * At this step, our reference variable "myArray" is a POINTER to our
         * double array, which is an object. So when we pass "myArray" into
         * the method changeArrayInPlace(), we pass in a POINTER to the object
         * right here in the main method. So the method's parameter "in" is set
         * to POINT to our array object right here in the main method.
         *
         * Because the parameter points to our object in the main method,
         * any changes made to the "in" reference variable in the separate
         * method is directly reflected back to the object right here in the
         * main method.
         */
        changeArrayInPlace(myArray);

        /*
         * We didn't return any values from the method, so that might be a bit
         * misleading, but as explained above, we passed in a POINTER to our
         * method, which modified the object in our main method. Since we can
         * safely assume our data was modified in the method and reflected back
         * to the main method, we can check the data now.
         */
        for (double value : myArray) {
            System.out.println("AFTER: " + value);
        }

        /*
         * So you can see in the before/after output that we have successfully
         * modified our array object right here in the main method by passing in
         * a POINTER to the separate method.
         *
         * An important thing to mention is that if the programmer wants to
         * create a copy of an object, the programmer must use the clone()
         * method, which is available to every single object in the entire Java
         * language. So if an independent copy of an object needs to be made,
         * this is how to do it.
         *
         * Note: Try removing the clone() method, and see how that affects
         * the output!
         * Instead of this:
         * double[] anotherArray = myArray.clone();
         * Try this:
         * double[] anotherArray = myArray;
         *
         * You'll notice that they both end up holding the same values.
         * This is because when you assign "myArray" to "anotherArray", what
         * you're actually doing is assigning the SAME POINTER held by "myArray"
         * to "anotherArray" which means that they both point to the exact same
         * object in memory. Using the clone() method prevents this, because it
         * creates an independent copy of the object!
         */
        double[] anotherArray = myArray.clone();

        /*
         * Let's pass this array through our separate method, and we will see
         * that our two arrays in the main method are completely independent of
         * one another!
         */
        changeArrayInPlace(anotherArray);

        for (double value : myArray) {
            System.out.println("myArray: " + value);
        }

        for (double value : anotherArray) {
            System.out.println("anotherArray: " + value);
        }

        /*
         * Hopefully this has been a helpful demonstration on how pointers work
         * in Java. Feel free to experiment with the code to understand more
         * about it.
         *
         * The important information to take away from this is that pointers
         * in Computer Science in general is a value that "points" to a separate
         * memory location. In Java, pointers are almost always represented as
         * reference variables, and the programmer typically does not need to
         * keep track of the pointer's actual memory location, or the memory
         * location it points to.
         */
    }

    /**
     * Changes an array without creating a new array.
     *
     * @param in The input array.
     */
    private static void changeArrayInPlace(double[] in) {
        /*
         * This is VERY important to understand. The method variable/parameter
         * "in" is a variable that points to an object in memory.
         *
         * IMPORTANT: EVERY SINGLE REFERENCE VARIABLE IN JAVA IS A POINTER.
         *
         * Since the parameter "in" is a pointer to a memory location, the
         * variable points to the very same object that exists in the main
         * method!
         *
         * This is how the object in the main method gets modified.
         */
        for (int i = 0; i < in.length; i++) {
            in[i] /= 2.0; // Divide every element by 2!
        }
    }

}