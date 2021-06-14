/*
 * Created by Steven Jennings on 26 October 2017.
 *
 *******************************************************************************
 *
 * An important concept to understand in basic Java development is the use
 * of "static" variables and methods.
 *
 * In a class, there are regular fields and methods that we can refer to,
 * and these would all belong to that particular instance. This means that
 * regular variables and methods are unique inside every instance of a class,
 * and that they can be modified independently of each other (My Pencil example
 * makes this clear! See ClassObjectInstanceDemo.java).
 *
 * The "static" keyword introduces a new concept. A "static" variable means that
 * particular variables is SHARED across all instances of a class. So if you
 * modify a static variable in one instance of a class, that change will be
 * reflected between ALL instances of the same class.
 *
 * For example, if we look at our Pencil class in the ClassObjectInstanceDemo,
 * we have three separate instances of our pencil. If we added in a static field
 * that referred to the type of pencil it was (we all pretty much only use
 * #2 pencils nowadays), then that field in the pencil would be the same across
 * all instances of the pencil. So if we changed the "type" of pencil that
 * pencil1 was, then that change would be reflected in pencil2 and pencil3,
 * and all instances of the Pencil class.
 *
 * That's quite the mouthful, so let's look at an example right here in this
 * demonstration. We will be using the same pencil class in the
 * ClassObjectInstanceDemo example, so if you've been through that demonstration,
 * this might come easier to you!
 */

package teaching.demoCode.keywordStatic;

import teaching.demoCode.classesObjectsAndInstances.Pencil;

/**
 * @author Steven Jennings
 * @version 0.0.0
 * @since 26 October 2017
 */
public class StaticDemo {

    /**
     * The main method.
     *
     * @param args Unused.
     */
    public static void main(String[] args) {
        /*
         * First off, let's create some new Pencil instances!
         *
         * We'll start with just two.
         */
        Pencil pencil1 = new Pencil();
        Pencil pencil2 = new Pencil();

        /*
         * We have two pencils now! Maybe not in real life, but we do in this
         * Java program.
         *
         * Let's check what type of pencil we have. It should be the default
         * "#2" pencil!
         */
        System.out.println("Pencil 1 is type: " + pencil1.getType());
        System.out.println("Pencil 2 is type: " + pencil2.getType());

        /*
         * There is ONE static field in the Pencil class that refers to the
         * "type" of pencil it is. I've never seen a #1 pencil; we've all seen
         * #2 pencils everywhere, but let's say that for SOME reason, we wanted
         * to make all of our pencils type #1. We can do that with static
         * fields!
         *
         * By default, all instances of our Pencil class are type "#2".
         * But if we wanted to change the type of EVERY instance of our pencil
         * class, we could do so simply by changing the static field!
         *
         * So, since we know that a static field is SHARED across ALL instances
         * of a class, if we change the field in one instance, it will be
         * reflected across ALL instances. Let's do it.
         */
        pencil1.setType("#1"); // Set the type to "#1"

        /*
         * Since we changed the static field of the Pencil class, that change
         * will be reflected between ALL instances of the Pencil class!
         *
         * Observe.
         */
        System.out.println("Pencil 1 is type: " + pencil1.getType());
        System.out.println("Pencil 2 is type: " + pencil2.getType());

        /*
         * You can see that we modified the static field in the pencil1 object,
         * but that change was reflected in the pencil2 object as well, even
         * though we didn't directly change the field in the pencil1 object!
         *
         * Let's change it one more time, to some other value.
         */
        pencil2.setType("New type! :)");

        /*
         * Now see how it changed all instances of our Pencil!
         */
        System.out.println("Pencil 1 is type: " + pencil1.getType());
        System.out.println("Pencil 2 is type: " + pencil2.getType());

        /*
         * For one last example, let's change the static variable THROUGH
         * the Pencil class itself!
         */
        Pencil.setType("Changed from the Pencil class!");

        /*
         * And let's check the type one last time.
         */
        System.out.println("Pencil 1 is type: " + pencil1.getType());
        System.out.println("Pencil 2 is type: " + pencil2.getType());

        /*
         * Hopefully this has been helpful in understanding how the static
         * keyword works in Java. Feel free to change the code and experiment
         * for yourself!
         *
         * Note: Your IDE might complain about accessing a static member
         * through an instance reference, as we did in this demonstration.
         * And this is true, normally per Java conventions, you do NOT change
         * static members through non-static contexts. But for the sake of
         * demonstration, we changed the static field by changing it in our
         * instance references.
         *
         * The important information to take away from this demonstration is
         * that STATIC members (fields and methods) are shared across ALL
         * instances of a class.
         */
    }

}