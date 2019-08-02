/*
 * Created by Steven Jennings on 02 November 2017.
 *
 * This is my own personal collection of useful Java functions!
 *
 *******************************************************************************
 *
 * Copyright 2017 Steven Jennings
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package util;

import org.apache.commons.math3.complex.Complex;

import javax.sound.sampled.*;
import javax.swing.*;
import java.io.File;
import java.lang.reflect.Type;
import java.text.SimpleDateFormat;
import java.util.Date;

/**
 * @author Steven Jennings
 * @version 0.0.0
 * @since 02 November 2017
 */
public class JenningsUtil {

    /**
     * Intentionally unreachable constructor.
     */
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
     * @param byteWidth   The size of each data value to process, I.E. sample size.
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
    public static byte[] rawDataToSingleChannel(byte[] in,
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
    public static double[] calculateFFTMagnitudes(Complex[] in) {
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
     * @return The selected file if successful. Otherwise: null.
     */

    /**
     * Displays a handy file selector.
     *
     * @param prompt The title of the window.
     * @return The selected file if successful. Otherwise: null.
     */
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
     * Fetches the full name of a given file (without the path).
     *
     * @param in The input file.
     * @return The full name of the file without path.
     */
    public static String getFileName(File in) {
        return new JFileChooser().getName(in);
    }

    /**
     * Forces the given value to be between two given values.
     *
     * @param in  The value to clamp.
     * @param min The minimum value.
     * @param max The maximum value.
     * @return The clamped value.
     */
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
     * @param in The number to evaluate.
     * @return Whether or not the given number is a power of 2.
     */
    public static boolean isPowerOf2(int in) {
        if (in <= 0) {
            return false;
        } else {
            return (in & (in - 0b1)) == 0;
        }
    }
}