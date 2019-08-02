/*
 * Created by Steven Jennings on 02 November 2017.
 */

package zzapo;

import org.apache.commons.math3.complex.Complex;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.sound.sampled.*;
import javax.swing.*;
import java.io.File;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.LinkedList;
import java.util.Random;

/**
 * @author Steven Jennings
 * @version 0.0.0
 * @since 02 November 2017
 */
public class JenningsUtil {

    /**
     * Intentionally unreachable constructor.
     */
    @Contract(pure = true)
    private JenningsUtil() {
        // Class should not be instantiable.
    }

    /**
     * Converts an array of multi-byte data to an array of single-value doubles.
     * This method respects little endian format versus big endian format, and
     * also depends on the byte width of the input data.
     * <p>
     * I.E. A byte width of 2 will treat the input data as 16-bit samples, and
     * vice versa.
     *
     * @param in          The input array of bytes.
     * @param numChannels The number of channels in the input digital signal.
     * @param byteWidth   The size of each data value to process, I.E. sample size in bytes.
     * @param isBigEndian Whether or not the data is stored as big endian or little endian.
     * @return A properly formatted array of single-value doubles.
     * @throws IllegalArgumentException In case the parameters are incompatible with each other.
     */
    public static double[] multiBytesToSingleDoubles(byte[] in,
                                                     int numChannels,
                                                     int byteWidth,
                                                     boolean isBigEndian) throws IllegalArgumentException {
        // Check for violation of byte width. It MUST NOT be greater than 32 bits
        if (byteWidth > 4) {
            throw new IllegalArgumentException("Byte width must not be greater than 4.");
        }

        // Check for illegal byteWidth and byte array length conflict
        if (in.length % byteWidth != 0) {
            throw new IllegalArgumentException("Byte width is not compatible with input byte array.");
        }

        // Declare method variables and objects
        double[] out = new double[in.length / byteWidth];
        int[] temp = new int[out.length];

        // Convert multi-byte values to single-integer values
        if (isBigEndian) { // Big endian format
            for (int i = 0; i < temp.length; i++) {
                for (int j = 0; j < byteWidth; j++) {
                    temp[i] |= (int) in[byteWidth * i + j];
                    if (j != byteWidth - 1) // Shift the bits 8 places to the left to make room for the next byte
                        temp[i] <<= 8;
                }
            }
        } else { // Little endian format
            for (int i = 0; i < temp.length; i++) {
                for (int j = byteWidth - 1; j >= 0; j--) {
                    temp[i] |= (int) in[byteWidth * i + j];
                    if (j != 0) // Shift the bits 8 places to the left to make room for the next byte
                        temp[i] <<= 8;
                }
            }
        }

        // Prepare method output
        for (int i = 0; i < out.length; i++) {
            out[i] = (double) temp[i];
        }

        // Method output
        return out;
    }

    /**
     * Splits a raw, digital data signal (byte array) into specified channels.
     *
     * @param in                The raw, digital data signal.
     * @param numChannels       Number of channels in the signal. (2 if stereo, 1 if mono, etc.)
     * @param channelSelection  The channel of raw data to return. (0 for Left, 1 for Right, etc.)
     * @param sampleSizeInBytes The sample size in bytes. (2 bytes for most digital signals/music files)
     * @return A byte array of raw data for a single channel.
     * @throws IllegalArgumentException If the arguments are invalid.
     */
    @Contract(pure = true)
    public static byte[] rawDataToSingleChannel(@NotNull byte[] in,
                                                int numChannels,
                                                int channelSelection,
                                                int sampleSizeInBytes) throws IllegalArgumentException {
        // Check for argument validity
        if (in.length % numChannels != 0 || channelSelection >= numChannels) {
            throw new IllegalArgumentException("Parameters are invalid.");
        }

        // Declare method variables and objects
        byte[] out = new byte[in.length / numChannels];

        // Split raw data into single channel
        for (int i = 0; i < out.length; i += sampleSizeInBytes) {
            for (int j = 0; j < sampleSizeInBytes; j++) {
                out[i + j] = in[sampleSizeInBytes * (i + channelSelection) + j];
            }
        }

        // Method output
        return out;
    }

    /**
     * Calculates the magnitudes of a Fast Fourier Transform
     *
     * @param in The input complex data array.
     * @return An array of doubles representing the magnitudes of a Fast Fourier Transform.
     * @see org.apache.commons.math3.transform.FastFourierTransformer
     */
    public static double[] calculateFFTMagnitudes(@NotNull Complex[] in) {
        // Declare method variables and objects
        double[] out = new double[in.length];

        // Calculate magnitudes
        for (int i = 0; i < out.length; i++) {
            out[i] = Math.sqrt(Math.pow(in[i].getReal(), 2.0) + Math.pow(in[i].getImaginary(), 2.0));
        }

        // Method output
        return out;
    }

    /**
     * Fetches the <code>SourceDataLine</code> for use with audio processing,
     * or digital signal processing.
     *
     * @param audioFormat The format of audio to use. Includes sample rate, channel count, big endianness, etc.
     * @return The <code>SourceDataLine</code> for further processing.
     * @throws LineUnavailableException In case the requested line is unavailable.
     */
    public static SourceDataLine getLine(AudioFormat audioFormat) throws LineUnavailableException {
        SourceDataLine res;
        DataLine.Info info = new DataLine.Info(SourceDataLine.class, audioFormat);
        res = (SourceDataLine) AudioSystem.getLine(info);
        res.open(audioFormat);
        return res;
    }

    /**
     * Prints a regular println() message to the system with a timestamp. Useful
     * for system logging.
     *
     * @param msg The message to print to console.
     */
    public static void println(String msg) {
        String timeStamp = new SimpleDateFormat("HH:mm:ss <] ").format(new Date());
        System.out.println(timeStamp + msg);
    }

    /**
     * Behaves just like the println() in the system's <code>PrintStream</code>,
     * but always includes a basic timestamp.
     */
    public static void println() {
        println("");
    }

    /**
     * Prints a regular print() message to the system with a timestamp. Useful
     * for system logging.
     *
     * @param msg The message to print to console.
     */
    public static void print(String msg) {
        String timeStamp = new SimpleDateFormat("HH:mm:ss <] ").format(new Date());
        System.out.print(timeStamp + msg);
    }

    /**
     * Displays a handy file selector.
     *
     * @param prompt The title of the window.
     * @return The selected file if successful. Otherwise: null.
     */
    @Nullable
    public static File selectFile(String prompt) {
        // Declare method variables and objects
        JFileChooser fc = new JFileChooser();

        // Set the file chooser's title
        fc.setDialogTitle(prompt);

        // Show file chooser, and assign the resulting value to result
        int result = fc.showOpenDialog(null);

        // If the file chooser was successful, set the class's music file to the selected file
        if (result == JFileChooser.APPROVE_OPTION) {
            return fc.getSelectedFile(); // If successful, return the file
        }

        // Return null otherwise
        return null;
    }

    /**
     * Displays a file selector with the default title.
     *
     * @return The selected file if successful. Otherwise: null.
     */
    public static File selectFile() {
        return selectFile("Select File");
    }

    /**
     * Fetches the full name of a given file (without the path).
     *
     * @param in The input file.
     * @return The full name of the file without path.
     */
    public static String getFileName(File in) {
        return new JFileChooser().getName(in);
    }

    /**
     * Clamps an input value such that the input will be forced to be between
     * the given min/max values. This can be thought of as signal clipping.
     *
     * @param in  A value to clamp.
     * @param min The minimum value.
     * @param max The maximum value.
     * @return The clamped value.
     */
    @Contract(pure = true)
    public static double clamp(double in, double min, double max) {
        if (in <= min) {
            return min;
        } else if (in >= max) {
            return max;
        } else {
            return in;
        }
    }

    /**
     * Determines whether or not the given positive integer is a power of 2.
     *
     * @param in A number to evaluate.
     * @return Whether or not the given number is a power of 2.
     */
    @Contract(pure = true)
    public static boolean isPowerOf2(int in) {
        return in > 0 && (in & (in - 0b1)) == 0;
    }

    /**
     * Converts a double value in percentage format to decimal format.
     *
     * @param in A percentage value.
     * @return The decimal value.
     */
    @Contract(pure = true)
    public static double percentToDecimal(double in) {
        return in / 100.0;
    }

    /**
     * Converts a double value in decimal format to percentage format.
     *
     * @param in A decimal value.
     * @return The percentage value.
     */
    @Contract(pure = true)
    public static double decimalToPercent(double in) {
        return in * 100.0;
    }

    /**
     * Gotta include the famous hello world program somewhere!
     */
    public static void noob() {
        System.out.println("Hello world!");
    }

    /**
     * Turns an input string into a pseudo-randomly generated number for use
     * in cryptographic applications.
     *
     * @param in The input string.
     * @return A pseudo-randomly generated number.
     */
    public static long cryptoStringToLong(String in) {
        Random random = new Random();
        // Hardcoded seed to introduce slightly less random behavior.
        // We want to reproduce results, so it can't be extremely random
        // as found in more secure cryptographic applications.
        random.setSeed(0);
        byte[] data = in.getBytes();
        long out = 0;
        int currentRandNum = 0;
        for (int i = 0; i < in.length(); i++) {
            // Cycle the number generator for more random behavior.
            for (int j = 0; j < random.nextInt(data[i]); j++) {
                random.nextInt();
            }

            currentRandNum = random.nextInt();

            out += currentRandNum;
        }
        if (random.nextBoolean()) {
            out *= -1; // 50% chance to make the result negative
        }
        return out;
    }

    /**
     * Allows the user to select a standard character set to be returned as a
     * character array.
     * <p>
     * The first bit represents lowercase characters. The second bit represents
     * uppercase characters. The third bit represents numbers. The fourth
     * bit represents all the possible symbols on a standard qwerty keyboard.
     *
     * @param selection The selection of possible characters in the character
     *                  set, selected as a 4-bit binary number in string format.
     *                  This parameter will be parsed to properly select
     *                  the desired character set to be returned as a character
     *                  array data type.
     * @return The desired character set.
     */
    public static char[] selectCharSet(String selection) throws IllegalArgumentException {
        // Define character sets and initialize variables
        char[] out;
        LinkedList<Character> buffer = new LinkedList<>();
        final char[] lowercaseSet =
                "abcdefghijklmnopqrstuvwxyz"
                        .toCharArray();
        final char[] uppercaseSet =
                "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                        .toCharArray();
        final char[] numberSet =
                "0123456789"
                        .toCharArray();
        final char[] symbolSet =
                "`~!@#$%^&*()-_=+[{]}\\|;:'\",<.>/?"
                        .toCharArray();
        int outLen = 0;
        char[] selectionArray = selection.toCharArray();

        // Evaluate input for illegal format
        if (selection.length() != 4) {
            throw new IllegalArgumentException();
        }
        for (char c : selectionArray) {
            if (c != "0".charAt(0) && c != "1".charAt(0)) {
                throw new IllegalArgumentException();
            }
        }

        // Parse selection and add corresponding character subsets to buffer
        if (selectionArray[0] == "1".charAt(0)) {
            outLen += lowercaseSet.length;
            for (char c : lowercaseSet) {
                buffer.add(c);
            }
        }
        if (selectionArray[1] == "1".charAt(0)) {
            outLen += uppercaseSet.length;
            for (char c : uppercaseSet) {
                buffer.add(c);
            }
        }
        if (selectionArray[2] == "1".charAt(0)) {
            outLen += numberSet.length;
            for (char c : numberSet) {
                buffer.add(c);
            }
        }
        if (selectionArray[3] == "1".charAt(0)) {
            outLen += symbolSet.length;
            for (char c : symbolSet) {
                buffer.add(c);
            }
        }

        // Prepare output character set
        out = new char[outLen];

        // Copy selected character subsets to output character set
        int index = 0;
        while (buffer.size() > 0) {
            out[index] = buffer.pop();
            index++;
        }

        // Output
        return out;
    }
}