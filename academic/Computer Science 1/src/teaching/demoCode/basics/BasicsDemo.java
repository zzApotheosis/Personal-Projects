/*
 * Created by Steven Jennings on 26 October 2017.
 *
 *******************************************************************************
 *
 * Java is a rather sophisticated programming language that can be intimidating
 * for beginners. But like everything, it just takes time to learn. There isn't
 * a single "a-ha" moment in Java where a person suddenly understands everything.
 * Rather, a person will have multiple "a-ha" moments when learning Java, each
 * for a different or new concept. I have roughly 4 years of Java experience,
 * and even I am learning new things about Java.
 *
 * The reason that any and all programming languages exists is to accomplish a
 * certain task. I am an Electrical Engineer(ing Student) and it is widely
 * accepted among all engineers that our typical roles heavily involve,
 * but might not be limited to: design, analysis, theory, and application.
 *
 * The same can be said in most, if not all Computer Science curricula and
 * applications. Every program that any person has ever written has had a task
 * to accomplish and a reason for its existence.
 *
 * This demonstration program will attempt to lay out the basics of Java in a
 * nice, readable format. This is intended for people who are just getting into
 * Java and Computer Science in general.
 *
 * Note: This demonstration barely scratches the surface of everything that
 * Java has to offer. Everything taught in this demonstration should serve
 * as a good head-start to Java, but just remember that this is the tip
 * of the iceberg!
 */

package teaching.demoCode.basics;

/**
 * @author Steven Jennings
 * @version 0.0.0
 * @since 26 October 2017
 */
public class BasicsDemo {

    public static void main(String[] args) {
        /*
         * Change the number in the following line to learn about different
         * basic concepts in Java!
         *
         * Valid numbers: 0 to 7
         */
        doBasicExample(0);
    }

    private static void doBasicExample(int choice) {
        switch (choice) {
            // System output
            case 0:
                /*
                 * For example 0, let's start with the most notoriously basic
                 * program you can possibly write: The "Hello World" program.
                 *
                 * For this, we need to understand how to write data to the
                 * console (or terminal).
                 *
                 * The "Hello World" program is almost always written with a
                 * single statement in any programming language.
                 * In Java, it goes like this:
                 */
                System.out.println("Hello World!");

                /*
                 * There is also the print() method, which does the exact same
                 * thing as println(), except it doesn't place the carat on the
                 * next line. This would be useful for when you want to write
                 * things on the same line.
                 *
                 * Note: Since we used println() in the previous line, the carat
                 * was placed on the next line for us automatically. When we
                 * write "Goodbye World!" to system output, the carat is still
                 * on the same line, and we can show that with the following
                 * statements.
                 */
                System.out.print("Goodbye World!");
                System.out.println(" This is on the same line!");

                break; // End case 0

            // If statements
            case 1:
                /*
                 * In this example, let's learn about program branching.
                 *
                 * In the HUGE majority of programs you would write in your
                 * programming career, your programs will NOT be perfectly
                 * linear. They will behave differently based on certain
                 * conditions met, and those conditions depend on whatever
                 * the task is.
                 *
                 * Let's look at the most basic way to branch a program:
                 * The if statement.
                 *
                 * Let's say we have a variable in our program that
                 * represents a condition we want our program to branch for.
                 * This can be expressed with an if statement, like so:
                 */
                int myCondition = 10;
                if (myCondition > 100) {
                    System.out.println("This is stage 1!");
                }

                /*
                 * This is the most basic way to branch a program. With the
                 * code written above, the program would check to see if the
                 * variable called "myCondition" is greater than 100. If that
                 * condition is true, then it would execute the code inside
                 * the if statement.
                 *
                 * Next, let's look at another way to write an if statement,
                 * or specifically, an if-else statement
                 */
                if (myCondition <= 0) {
                    System.out.println("This is stage 2!");
                } else {
                    System.out.println("This is stage 3!");
                }

                /*
                 * With the above code, the program would branch and execute
                 * one of those blocks of code based on the condition. In
                 * plain English, the code is saying this:
                 *
                 * "If myCondition is less than or equal to 0, then execute
                 * the following code.
                 *
                 * Otherwise... Execute the following code."
                 *
                 * Finally, let's look at the most complex way to write an
                 * if statement (which is still very simple).
                 */
                if (myCondition == 10) {
                    System.out.println("This is stage 4!");
                } else if (myCondition < 10) {
                    System.out.println("This is stage 5!");
                } else if (myCondition > 10) {
                    System.out.println("This is stage 6!");
                } else {
                    System.out.println("This is stage 7!");
                }

                /*
                 * The above code would, again, branch and execute one of the
                 * given blocks of code based on the conditions given.
                 *
                 * In plain English, this is what that code says:
                 *
                 * "If myCondition is equal to 10, then execute the following
                 * code.
                 *
                 * Otherwise... If myCondition is less than 10, then execute the
                 * following code.
                 *
                 * Otherwise... If myCondition is greater than 10, then execute
                 * the following code.
                 *
                 * Otherwise... Execute the following code."
                 *
                 * There is a special type of if statement that uses a ternary
                 * operator and can condense an entire standard if statement into
                 * a single line of code. The cool thing about this special type
                 * of if statement is that it can be used pretty much anywhere,
                 * even inside other statements.
                 *
                 * The syntax looks a little weird at first, but it goes like
                 * this:
                 * <Is this condition true> ? <If so, do this> : <Otherwise, do this>
                 */
                System.out.println(
                        myCondition == 10 ? "This is stage 8!" : "This is stage 9!"
                );

                break; // End Case 1

            // Loops
            case 2:
                /*
                 * At this point (and depending on where you are in your
                 * Computer Science course), you probably know how to branch
                 * your program with if statements. Another critical concept
                 * to understand in Java (and all programming languages for
                 * that matter) is the loop, and executions thereof.
                 *
                 * There are several different types of loops in Java, including
                 * the "for" loop, "while" loop, "do while" loop, and the
                 * uncommon "enhanced for" loop.
                 *
                 * Let's start with the for loop! Its syntax goes like this:
                 * for (<initialize control variables>;
                 *      <check condition of the loop>;
                 *      <change control variables>) {
                 *     // Code to execute for every iteration of the loop
                 * }
                 *
                 * Or more simply:
                 * for (<initialize>; <check>; <control>) {
                 *     // Do stuff
                 * }
                 */
                for (int i = 0; i < 3; i++) {
                    System.out.println("This is the for loop! " + i);
                }

                /*
                 * The for loop is a pretty basic loop, but the while loop is
                 * also a very basic loop! Pretty much all of the loops you
                 * write will either be for loops or while loops.
                 *
                 * Here's the syntax for a while loop:
                 * while (<condition is true>) {
                 *     // Code to execute for every iteration of the loop
                 * }
                 *
                 * This one is very basic... because it is! The while loop
                 * allows you to be more flexible with the conditions in which
                 * the loop iterates, but also can be a little more difficult
                 * to keep track of control variables, mainly because the
                 * control variables exist outside of the loop's statement.
                 */
                int externalControlVariable = 0; // We'll need this
                while (externalControlVariable < 3) {
                    System.out.println("This is the while loop! " +
                            externalControlVariable);
                    externalControlVariable++; // Increment control variable
                }

                /*
                 * Typically, programmers like to go by a universal rule of
                 * thumb when choosing which loop to use:
                 * If you know exactly how many times you need to loop through
                 * (or reiterate) some code, you should use a for loop. If you
                 * don't know how many times you should loop your code (I.E.
                 * It depends on something in your code), then you should use
                 * a while loop.
                 *
                 * There's also the do while loop, which is pretty much the
                 * same as the while loop. The ONLY difference between the while
                 * loop and the do while loop is that the do while loop executes
                 * the block of code, and THEN checks the looping condition,
                 * whereas the regular while loop will check the looping
                 * condition, and THEN execute the block of code.
                 *
                 * Its syntax goes like this:
                 * do {
                 *     // Code and stuff
                 * } while (<condition is true>);
                 */
                externalControlVariable = 0;
                do {
                    System.out.println("This is the do while loop! " +
                            externalControlVariable);
                    externalControlVariable++;
                } while (externalControlVariable < 3);

                /*
                 * Usually, a programmer will just end up using a regular while
                 * loop, but in some special circumstances, one might want to
                 * use a do while loop, but that's for pretty rare occasions.
                 * It's still good to know how it works anyway.
                 *
                 * The next and final type of loop you'll probably ever
                 * encounter is the enhanced for loop. This type of loop is useful
                 * particularly for arrays because you can perform some
                 * code operation on every index in an array. The enhanced for loop
                 * is mainly used with arrays, but can be used with other
                 * objects as well, like a LinkedList, which is one of my
                 * favorite ways to store data.
                 *
                 * The syntax for the enhanced for loop goes like this:
                 * for (<Type> <variable name> : <existing object>) {
                 *     // Do stuff
                 * }
                 *
                 * That's the general idea, but in plain English, it's like this:
                 * for (<Each data type> <whatever> : <in this thing>) {
                 *     // Do this with whatever
                 * }
                 */
                int[] temporaryArray = new int[]{
                        100,
                        200,
                        300
                };
                // Here's the enhanced for loop!
                for (int value : temporaryArray) {
                    System.out.println("This is the enhanced for loop! " + value);
                }

                /*
                 * That wraps up pretty much all the loops you'll ever use in
                 * your Java life.
                 *
                 * Just remember and study these types of loops (mostly just
                 * the first two, but it's nice to know the others):
                 * For
                 * While
                 * Do While
                 * Enhanced for
                 */

                break; // End case 2

            // Conditions
            case 3:
                /*
                 * Conditions are very important in any programming language
                 * because they pretty much ALWAYS determine how your program
                 * branches and flows.
                 *
                 * Conditions always evaluate to boolean expressions in Java,
                 * which is a primitive data type by the way, so you could also
                 * have boolean variables. The types of variable boolean
                 * expressions in Java are very commonly used as mathematical
                 * equalities and inequalities:
                 *
                 * <  --- Less than
                 * <= --- Less than or equal to
                 * == --- Equal to
                 * >= --- Greater than or equal to
                 * >  --- Greater than
                 * !  --- Not
                 * != --- Not equal to
                 */
                boolean isValid; // This will hold our boolean condition value
                isValid = (0 == 1);
                System.out.println("0 = 1? " + isValid);
                isValid = (1 > 0);
                System.out.println("1 > 0? " + isValid);
                isValid = (1 >= 0);
                System.out.println("1 >= 0? " + isValid);
                isValid = (1 < 0);
                System.out.println("1 < 0? " + isValid);
                isValid = (1 <= 0);
                System.out.println("1 <= 0? " + isValid);
                isValid = (1 != 0);
                System.out.println("1 != 0? " + isValid);

                /*
                 * Conditions are almost exclusively used in loops and if
                 * statements, but they can be useful in other scenarios as well.
                 */

                break; // End case 3

            // Assignment Operator
            case 4:
                /*
                 * You might notice that the equals sign (=) is used a bit
                 * differently in programming than in typical Algebra uses.
                 * In pretty much every programming language, the equals sign
                 * is the assignment operator.
                 *
                 * In plain English, it means "Take the stuff on the right
                 * and assign it to the stuff on the left."
                 *
                 * The assignment operator almost ALWAYS finishes calculations
                 * and operations on the right side of the assignment statement
                 * before storing the result on the left side.
                 *
                 * Let's start with very basic assignment statements; these are
                 * almost trivial.
                 */
                int value1 = 25;
                double value2 = 13.37;
                String value3 = "\"Hello World!\"";
                System.out.println("value1 holds a value of " + value1);
                System.out.println("value2 holds a value of " + value2);
                System.out.println("value3 holds a value of " + value3);

                /*
                 * In this example, it's very simple. There are no calculations
                 * to be done on the right side, so the program will store the
                 * literal values 25, 13.37, and "Hello World!" into their
                 * respective variables.
                 *
                 * Next, let's do some actual calculations.
                 */
                value1 = 20 / 5; // Obviously 4
                value2 = 3.14 * 10; // Obviously 31.4
                value3 = "\"Goodbye" + " World!\""; // This is called concatenation
                System.out.println("value1 holds a value of " + value1);
                System.out.println("value2 holds a value of " + value2);
                System.out.println("value3 holds a value of " + value3);

                /*
                 * You can also do calculations with your already existing
                 * variables!
                 *
                 * Note: It is perfectly valid in Java (and pretty much all
                 * programming languages) to use the same variable on both sides
                 * of the assignment statement. Remember, the assignment
                 * statement ALWAYS finishes whatever is on the right side
                 * of the statement BEFORE storing it on the left. So in this
                 * case, we take the existing values in our variables, perform
                 * some operation on them, and then store the result in the
                 * same variables.
                 */
                value1 = value1 / 5; // Divide value1 by 5, then store in value1
                value2 = value2 / 2; // Divide value2 by 2, then store in value2
                System.out.println("value1 holds a value of " + value1);
                System.out.println("value2 holds a value of " + value2);

                /*
                 * An important thing to remember about assignment operators
                 * is that you can combine basic math functions with the
                 * assignment operator (=) to shorten the length of the
                 * statement, which makes for cleaner code.
                 */
                value1 += 5; // Increment by 5
                value2 += 5.0; // Increment by 5.0
                System.out.println("value1 holds a value of " + value1);
                System.out.println("value2 holds a value of " + value2);

                break; // End case 4

            // Ways to calculate in Java
            case 5:
                /*
                 * There are many different ways to perform basic arithmetic in
                 * Java (except exponents, there's no built-in way to do those),
                 * and here is the basic list of basic arithmetic operators:
                 * Addition:       +
                 * Subtraction:    -
                 * Multiplication: *
                 * Division:       /
                 * Modulo:         %
                 *
                 * If you have never seen modulo before, it's simply the
                 * remainder of a division operation.
                 *
                 * Let's create some numbers to use in our basic calculations.
                 */
                int i1 = 5;
                int i2 = 7;
                double d1 = 5.0;
                double d2 = 7.0;

                /*
                 * Now, let's perform some calculations with these numbers!
                 */
                // Addition
                System.out.println(i1 + " + " + i2 + " = " + (i1 + i2));
                System.out.println(d1 + " + " + d2 + " = " + (d1 + d2));
                // Subtraction
                System.out.println(i1 + " - " + i2 + " = " + (i1 - i2));
                System.out.println(d1 + " - " + d2 + " = " + (d1 - d2));
                // Multiplication
                System.out.println(i1 + " * " + i2 + " = " + (i1 * i2));
                System.out.println(d1 + " * " + d2 + " = " + (d1 * d2));
                // Division (This will get weird for beginners)
                System.out.println(i1 + " / " + i2 + " = " + (i1 / i2));
                System.out.println(d1 + " / " + d2 + " = " + (d1 / d2));
                // Modulo
                System.out.println(i1 + " % " + i2 + " = " + (i1 % i2));
                System.out.println(d1 + " % " + d2 + " = " + (d1 % d2));

                /*
                 * You probably noticed the division operation for our integers
                 * gave a "wrong" answer. 5 / 7 is obviously not equal to 0.
                 * Actually, in Java, this is true. This is a SPECIAL type of
                 * division called integer division and it  is very useful in
                 * many situations.
                 *
                 * Understanding integer division isn't difficult. When you
                 * have a dividend and a divisor, you will get a whole number
                 * for an answer AS WELL AS a remainder if the numbers don't
                 * divide evenly. In our example, 5 / 7 = 0, remainder 5. So
                 * the integer part of that division will yield a value of 0.
                 *
                 * You might also notice that the remainder in a division
                 * operation is the same as our modulo operation. That is true.
                 * The modulo operator does integer division, and then returns
                 * the remainder value. (This is particularly useful in
                 * determining whether or not a number is even or odd!)
                 */

                break; // End case 5

            // Casting
            case 6:
                /*
                 * Casting is a powerful tool used when the programmer needs to
                 * "force" some operand to become a different type for some
                 * other need. It's not really "forcing" a different type,
                 * because you can't "force" a String object to become an array.
                 *
                 * The most basic casting example can be done with an average
                 * between two integers. Earlier, we learned that integer division
                 * is the division between two integers that always results in
                 * a whole integer number, instead of a decimal value. What if
                 * we want to get the average of two integer numbers, or even
                 * several integer numbers? We can do this with casting.
                 *
                 * The syntax for casting goes like this:
                 * (type) <operand>
                 */
                double average;
                int num1 = 10;
                int num2 = 31;

                /*
                 * So we have our two integers, and a variable to represent the
                 * average between those two numbers.
                 *
                 * Remember: the assignment statement almost ALWAYS executes the
                 * stuff on the right side before it assigns it to the left side.
                 */
                average = (double) (num1 + num2) / 2; // Note: 2 is an integer
                System.out.println("num1 = " + num1);
                System.out.println("num2 = " + num2);
                System.out.println("average = " + average);

                /*
                 * num1 and num2 are both integers, so adding them together
                 * would result in an integer. Dividing this integer result by
                 * 2 would be integer division, which would normally result
                 * in a single whole number as is regular integer division.
                 * However, by casting the result of (num1 + num2) to be a
                 * double, we force the operation to use regular division
                 * instead of integer division, which results in an actually
                 * valid average!
                 *
                 * The following is a much more advanced casting example:
                 * Casting is useful in many scenarios. In some of my applications
                 * for my Computer Science 1 course, I stored values in a
                 * LinkedList of Objects, which consisted of many different data
                 * types which I knew of beforehand. In order to organize the
                 * data into usable arrays, I had to cast their appropriate
                 * values to their appropriate data types! You can find this
                 * example in my DataHandler class for my solution to Program 10.
                 */

                break; // End case 6

            // Increment and decrement operators
            case 7:
                /*
                 * In many cases, especially in loops, you will want to increment
                 * or decrement some variable depending on your situation.
                 * One of the most common uses for the increment operator is for
                 * a variable that behaves like a counter.
                 *
                 * We'll just stick with the basic syntax and usage in our
                 * example here. The actual operators in code are these symbols:
                 * Increment: ++
                 * Decrement: --
                 *
                 * Fun fact: If you have heard of the C and C++ programming
                 * languages, C++ was cleverly named using this operator!
                 * It hints that C++ is a "step above" C, like an incremented
                 * programming language.
                 *
                 * Let's start with some variable declarations.
                 */
                int counter = 0;
                int variable;

                /*
                 * Now, let's use the increment operator to assign values to our
                 * variable. When you put the ++ in front of the variable you
                 * want to increment, it will FIRST increment, and THEN perform
                 * the assignment.
                 */
                variable = ++counter;
                System.out.println("counter = " + counter);
                System.out.println("variable = " + variable);

                /*
                 * So at this point, both our variables hold a value of 1.
                 *
                 * The other way to use the increment operator is by putting the
                 * ++ behind the variable you want to increment. This achieves
                 * the same outcome for the variable you increment, but this may
                 * have different effects on other operations you might do, like
                 * an assignment statement. Observe.
                 *
                 * Note: Placing the increment operator behind the target
                 * variable is one of those rare scenarios where the assignment
                 * happens first, and THEN the increment happens.
                 */
                variable = counter++;
                System.out.println("counter = " + counter);
                System.out.println("variable = " + variable);

                /*
                 * At this point, counter = 2, and variable = 1. This is
                 * because the assignment happened first (variable = counter),
                 * and THEN the increment happened (counter++).
                 *
                 * The decrement operator works the exact same way as the
                 * increment operator, except it obviously decrements instead
                 * of increments. Observe.
                 */
                variable = counter--;
                System.out.println("counter = " + counter);
                System.out.println("variable = " + variable);

                /*
                 * So now, counter = 1, and variable = 2 for the same reasoning
                 * as above.
                 *
                 * And finally, we place the decrement operator in front of the
                 * target variable.
                 */
                variable = --counter;
                System.out.println("counter = " + counter);
                System.out.println("variable = " + variable);

                break; // End case 7

            // In case the user chooses a number that's not in this list
            default:
                System.out.println("That's not a valid choice! Try another number.");
                break; // End default case
        }
    }

}