/*
 * Created by Steven Jennings on 23 October 2017.
 *
 *******************************************************************************
 *
 * One of the most important concepts to understand in Java is
 * the concept of Object-Oriented Programming.
 *
 * In reality, everything can be modeled as an object.
 * A pencil, for example, is a very simple object that has a handful of
 * identifiable properties that make it unique.
 *
 * You can do things with the pencil that might alter the properties of
 * the pencil itself. You could write something with the pencil.
 * You could erase things you have written. You could add graphite to the
 * pencil if it was mechanical. Likewise, you could remove the graphite.
 *
 * The properties of a real object can be modeled as
 * the fields of an object in Java. Likewise, the things that can happen
 * to the pencil can be modeled as functions (better known as methods) in Java.
 *
 * In our example, the object is the pencil.
 * The properties of the pencil include (but might not be limited to):
 * - The amount of graphite
 * - The amount of eraser
 * - The material of the pencil
 *
 * These are very basic properties that can be modeled in this demonstration!
 */

package teaching.demoCode.classesObjectsAndInstances;

/**
 * @author Steven Jennings
 * @version 0.0.0
 * @since 23 October 2017
 */
public class ClassObjectInstanceDemo {

    /**
     * The main method.
     *
     * @param args Unused.
     */
    public static void main(String[] args) {
        /*
         * This is where we create what's known as an "instance" of our object.
         *
         * Having written the class called "Pencil.java", we can create
         * as many instances of our class as we want! (Provided the computer has
         * enough memory to allocate for these objects)
         *
         * In this demonstration, we create three independent instances of the
         * Pencil class.
         *
         * NOTE: A declaration statement like this always goes in this format:
         * <Type> <Reference Variable Name> = new <Constructor Method Name>();
         * Pencil pencil = new Pencil();
         *
         * It's also perfectly valid to declare variables without initializing:
         * <Type> <Reference Variable Name>;
         * Pencil pencil;
         */
        Pencil pencil1 = new Pencil();
        Pencil pencil2 = new Pencil();
        Pencil pencil3 = new Pencil();

        /*
         * First off, we have brand new pencils! Let's check how much stuff
         * they have.
         */
        System.out.print("Pencil 1 has " + pencil1.getGraphite());
        System.out.println(" graphite and " + pencil1.getEraser() + " eraser left!");
        System.out.print("Pencil 2 has " + pencil2.getGraphite());
        System.out.println(" graphite and " + pencil2.getEraser() + " eraser left!");
        System.out.print("Pencil 3 has " + pencil3.getGraphite());
        System.out.println(" graphite and " + pencil3.getEraser() + " eraser left!");

        /*
         * We have a lot of things we can "do" with the pencil, and we have
         * written these things that we can do as "methods" in the Pencil class.
         *
         * In this demonstration, let's just stick with some basic operations.
         * We'll use the first pencil a lot, use the second pencil a bit,
         * and use the third pencil barely at all.
         *
         * NOTE: The dot-notation is frequently used in Java and always
         * denotes a method or field inside that reference variable. In this
         * case, the writePencil() method inside the pencil1 instance is called.
         */
        pencil1.writePencil(80); // Used 80 units of graphite
        pencil2.writePencil(50); // Used 50 units of graphite
        pencil3.writePencil(10); // Used 10 units of graphite

        /*
         * And let's say we made a few writing mistakes with our pencils,
         * so we have to use the pencils' erasers a bit.
         */
        pencil1.useEraser(30); // Made 30 units worth of eraser mistakes
        pencil2.useEraser(10); // Made 10 units worth of eraser mistakes
        pencil3.useEraser(5); // Made 5 units worth of eraser mistakes

        /*
         * So now that we have "used" our three pencils, we can check how much
         * graphite and eraser our pencils have left by fetching their values,
         * per instance!
         */
        System.out.print("Pencil 1 has " + pencil1.getGraphite());
        System.out.println(" graphite and " + pencil1.getEraser() + " eraser left!");
        System.out.print("Pencil 2 has " + pencil2.getGraphite());
        System.out.println(" graphite and " + pencil2.getEraser() + " eraser left!");
        System.out.print("Pencil 3 has " + pencil3.getGraphite());
        System.out.println(" graphite and " + pencil3.getEraser() + " eraser left!");

        /*
         * We want to keep using our pencils even if their erasers run down,
         * so let's replace their erasers!
         */
        pencil1.newEraser();
        pencil2.newEraser();
        pencil3.newEraser();

        /*
         * So after we got new erasers for each pencil, they should have
         * updated values, right? Let's check.
         */
        System.out.print("Pencil 1 has " + pencil1.getGraphite());
        System.out.println(" graphite and " + pencil1.getEraser() + " eraser left!");
        System.out.print("Pencil 2 has " + pencil2.getGraphite());
        System.out.println(" graphite and " + pencil2.getEraser() + " eraser left!");
        System.out.print("Pencil 3 has " + pencil3.getGraphite());
        System.out.println(" graphite and " + pencil3.getEraser() + " eraser left!");

        /*
         * And since we used so much graphite, let's add back what we lost.
         */
        pencil1.addGraphite(80); // Added 80 units of graphite to pencil1
        pencil2.addGraphite(50); // Added 50 units of graphite to pencil2
        pencil3.addGraphite(10); // Added 10 units of graphite to pencil3

        /*
         * At this point, our pencil instances should be back to brand new!
         */
        System.out.print("Pencil 1 has " + pencil1.getGraphite());
        System.out.println(" graphite and " + pencil1.getEraser() + " eraser left!");
        System.out.print("Pencil 2 has " + pencil2.getGraphite());
        System.out.println(" graphite and " + pencil2.getEraser() + " eraser left!");
        System.out.print("Pencil 3 has " + pencil3.getGraphite());
        System.out.println(" graphite and " + pencil3.getEraser() + " eraser left!");

        /*
         * So hopefully this helps with understanding how classes, objects,
         * and instances work together in Java. Feel free to play around
         * with the code yourself and experiment!
         *
         * Here's all the important information to take away from this demo:
         * A class is an object's "definition" or "blueprint"
         * An object is an INSTANTIATION of a class.
         * And "instance" pretty much just means "object".
         */
    }

}