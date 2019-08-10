/*
 * Created by Steven Jennings on 23 October 2017.
 */

package teaching.demoCode.classesObjectsAndInstances;

/**
 * The Pencil class! One of the most basic objects in everyday human lives.
 *
 * @author Steven Jennings
 * @version 0.0.0
 * @since 23 October 2017
 */
public class Pencil { // Class is public so we can use it in other packages!

    // Class fields
    private int amountGraphite;        // Amount of grahpite
    private int amountEraser;          // Amount of eraser
    private String material;           // What the pencil is made of
    private static String type = "#2"; // The type of pencil (#2 in most cases)

    // *************************************************************************

    /*
     * These are the "constructors" of the object.
     *
     * A constructor is a special type of "method" that quite literally
     * "constructs" the object when you create an instance of that object.
     *
     * Also note that the constructor is almost ALWAYS named the same as the
     * class name itself. In this case, our constructors must be called
     * "Pencil".
     */

    /**
     * Default constructor.
     */
    public Pencil() {
        this.amountGraphite = 100;    // 100 units of graphite by default
        this.amountEraser = 100;      // 100 units of eraser by default
        this.material = "Mechanical"; // Mechanical pencil by default
    }

    // *************************************************************************

    /*
     * Now, we define the "functions" of the object!
     * (Better known as "methods" in Java)
     */

    /**
     * Uses the pencil once.
     */
    public void writePencil() {
        writePencil(1); // Uses the pencil once
    }

    /**
     * Uses the pencil's graphite (writes) a custom number of times.
     * <p>
     * Note: You may notice that this method is named the exact same as
     * the one above! This is called "method overloading" and it is useful
     * when you want to be more specific in the way you execute a
     * particular task. In this case, we might want to be more specific in
     * the number of times we "use" our pencil.
     *
     * @param use The number of times to "use" the pencil's graphite (write).
     */
    public void writePencil(int use) {
        // Check if the pencil has enough graphite to use
        if (this.amountGraphite - use >= 0) {
            this.amountGraphite -= use;
            System.out.println("Wrote with the pencil " + use + " times!");
        } else {
            this.amountGraphite = 0;
            System.out.println("Not enough graphite! :(");
        }

        /*
         * So you can see that the pencil has been used this many times,
         * so the pencil loses a bit of graphite!
         */
    }

    /**
     * Uses the eraser once.
     */
    public void useEraser() {
        useEraser(1);
    }

    /**
     * Uses the pencil's eraser a custom number of times.
     * <p>
     * Note: This is also an overloaded method!
     *
     * @param use The number of times to "use" the eraser.
     */
    public void useEraser(int use) {
        if (this.amountEraser - use >= 0) {
            this.amountEraser -= use;
            System.out.println("Used the eraser " + use + " times!");
        } else {
            this.amountEraser = 0;
            System.out.println("Not enough eraser! :(");
        }

        /*
         * Similarly, the eraser gets used this many times, so it loses
         * a bit of its eraser!
         */
    }

    /**
     * Adds more graphite to the pencil.
     * <p>
     * This is very similar to our writePencil() method, except in this case,
     * we are ADDING graphite to our pencil!
     *
     * @param moreGraphite The amount of graphite to add to the pencil.
     */
    public void addGraphite(int moreGraphite) {
        System.out.println("Adding " + moreGraphite + " more graphite!");
        this.amountGraphite += moreGraphite;
    }

    /**
     * Puts a new eraser in the pencil. In real life, you don't "add" more
     * eraser to your pencil. You just get a new eraser, and that's what this
     * method simulates!
     */
    public void newEraser() {
        System.out.println("Got a new eraser! :)");
        this.amountEraser = 100;
    }

    /*
     * In the following section, we have what's known as "getters" and "setters".
     *
     * A "getter" is a method in a class that quite literally gets a specified
     * field of the class.
     *
     * A "setter" is also a method in a class that also quite literally sets
     * a specified field of the class.
     *
     * There is typically a getter/setter pair for each field in a class.
     *
     * It's considered good practice to use these methods to interface directly
     * with the object's properties (better known as "fields"). This means that
     * the fields of an object should almost always be private to itself,
     * and only itself.
     */

    /**
     * Gets the material of the pencil. Remember, it's "Mechanical" by default!
     *
     * @return The pencil material.
     */
    public String getMaterial() {
        return this.material;
    }

    /**
     * Gets the amount of graphite remaining.
     *
     * @return The amount of graphite.
     */
    public int getGraphite() {
        return this.amountGraphite;
    }

    /**
     * Gets the amount of eraser remaining.
     *
     * @return The amount of eraser remaining.
     */
    public int getEraser() {
        return this.amountEraser;
    }

    /**
     * Gets the pencil's type.
     *
     * @return The pencil's type.
     */
    public static String getType() {
        return type;
        /*
         * Notice we cannot use "this.type" because "this" refers to "this"
         * instance of the class!
         */
    }

    /*
     * These are the setters!
     */

    /**
     * Sets the new material of the pencil.
     *
     * @param newMaterial The new material.
     */
    public void setMaterial(String newMaterial) {
        this.material = newMaterial;
    }

    /**
     * Sets the new amount of graphite in the pencil.
     *
     * @param newGraphiteAmount The new amount of graphite.
     */
    public void setGraphiteAmount(int newGraphiteAmount) {
        this.amountGraphite = newGraphiteAmount;
    }

    /**
     * Sets the new amount of eraser in the pencil.
     *
     * @param newEraserAmount The new amount of eraser.
     */
    public void setEraserAmount(int newEraserAmount) {
        this.amountEraser = newEraserAmount;
    }

    /**
     * Sets the "type" of EVERY instance of this class to a new "type".
     * <p>
     * Note: In this case, "type" is just the pencil's type, but the word
     * type means other things in Java as well.
     *
     * @param newType The new type of pencil.
     */
    public static void setType(String newType) {
        type = newType;
    }

    /*
     * You might be wondering why getters and setters might be used in
     * a case like our pencil example.
     *
     * Short answer: Java conventions.
     * Long answer: It depends.
     *
     * In our basic example program, we just use the simple methods that we
     * wrote above, but let's say you were writing a program that's a little
     * more complex. There MIGHT be a scenario where you would need to manually
     * set the fields of a pencil, and that's why it's typically good practice
     * to write getters and setters, even if you might not use them.
     */

}