/*
 * Created by Steven Jennings on 03 November 2017.
 */

package util.handlers;

import main.GLSConfig;
import main.SystemMode;
import org.apache.commons.math3.complex.Complex;
import org.apache.commons.math3.transform.DftNormalization;
import org.apache.commons.math3.transform.FastFourierTransformer;
import org.apache.commons.math3.transform.TransformType;
import util.JenningsUtil;

import javax.sound.sampled.*;
import java.io.File;
import java.io.IOException;

/**
 * @author Steven Jennings
 * @version 0.0.0
 */
public class AnalysisHandler {

    /**
     * Performs a simple analysis on the given audio file.
     *
     * @param properties The system properties.
     * @param input      The input audio file.
     */
    public static void analyze(GLSConfig properties, File input) {
        // Declare method variables and objects
        AudioInputStream baseInput;
        AudioInputStream decodedInput;
        AudioFormat baseFormat;
        AudioFormat decodedFormat;

        // Begin executing code which has Exception-throwing potential
        try {
            // Instantiate objects
            baseInput = AudioSystem.getAudioInputStream(input);
            baseFormat = baseInput.getFormat();
            decodedFormat = new AudioFormat(
                    AudioFormat.Encoding.PCM_SIGNED,  // The encoding is in signed values
                    baseFormat.getSampleRate(),       // Sample rate
                    16,                               // Sample size
                    baseFormat.getChannels(),         // Number of channels
                    baseFormat.getChannels() * 2,     // Frame size in bytes
                    baseFormat.getSampleRate(),       // Frame rate
                    false                             // Big endian = false (so it's little endian)
            );
            decodedInput = AudioSystem.getAudioInputStream(decodedFormat, baseInput);

            // Add the format to the system properties
            properties.setTargetFormat(decodedFormat);

            // Determine Nyquist Rate, and update the channel definitions in system properties
            determineHalfNyquist(properties, decodedFormat);

            // Display some basic information about the analysis of the file
            JenningsUtil.println("File name: " + properties.getFileName());
            JenningsUtil.println("Sample rate: " + decodedFormat.getSampleRate());
            JenningsUtil.println("Sample size: " + decodedFormat.getSampleSizeInBits());
            JenningsUtil.println("Channels: " + decodedFormat.getChannels());
            JenningsUtil.println("Endianness: " + (decodedFormat.isBigEndian() ? "Big Endian" : "Little Endian"));
            JenningsUtil.println("Encoding: " + decodedFormat.getEncoding());

            /*
             * If the system is running in manual threshold mode OR custom
             * sequencing mode, return here. We have the audio format for the
             * system and have displayed good information about the audio data,
             * so there is no reason for this analysis to continue.
             */
            if (properties.getMode() == SystemMode.manual || properties.getMode() == SystemMode.custom) {
                return;
            }

            // Begin handling raw audio data
            rawAnalysis(properties, decodedFormat, decodedInput);
            baseInput.close();
        } catch (Exception e) {
            System.err.println("ERROR: Audio analysis failed.");
            e.printStackTrace();
        }
    }

    /**
     * Performs the raw analysis of the given audio file.
     * <p>
     * NOTE: This is confirmed to work with audio files that have no more than
     * 2 channels (stereo, or mono).
     *
     * @param properties   The system properties.
     * @param targetFormat The target <code>AudioFormat</code>.
     * @param decodedInput The <code>AudioInputStream</code> to read from (the source music file).
     */
    private static void rawAnalysis(GLSConfig properties,
                                    AudioFormat targetFormat,
                                    AudioInputStream decodedInput) {
        // Declare method variables and objects
        byte[] data = new byte[properties.getBufferSize()]; // Raw data bytes
        byte[][] channelByteData = new byte[targetFormat.getChannels()][properties.getBufferSize() / targetFormat.getChannels()]; // Raw data bytes split into different channels
        double[][] singleValues = new double[channelByteData.length][channelByteData[0].length]; // Data values in different channels
        double[] masterSignal = new double[channelByteData[0].length]; // Values that hold the values of each channel additively
        FastFourierTransformer fft = new FastFourierTransformer(DftNormalization.STANDARD);
        Complex[] complexes;
        double[] magnitudes; // Magnitudes calculated for the master digital data signal
        double frequency;
        double[] channelDefinitions = properties.getChannelDefinitions(); // System property
        double[] thresholds = new double[8]; // Thresholds for 8 channels
        int[] channelCounters = new int[8]; // For 8 channels
        double thresholdSensitivity = properties.getThresholdSensitivity();

        // Create output data line
        SourceDataLine line = null;
        try {
            line = JenningsUtil.getLine(targetFormat);
        } catch (LineUnavailableException e) {
            e.printStackTrace();
        }

        // Check for data line's existence
        if (line != null) {
            // Begin raw analysis of audio file
            line.start();
            int nBytesRead = 0;
            while (nBytesRead != -1) {
                // Analyze the audio file in chunks of data
                try {
                    // Read a chunk of data from the audio file
                    nBytesRead = decodedInput.read(data, 0, data.length);

                    // If any bytes have been read, analyze the chunk of data
                    if (nBytesRead != -1) {

                        // For every channel in the music file (usually no more than 2 channels, stereo)
                        for (int i = 0; i < targetFormat.getChannels(); i++) {
                            // Convert raw data to single channel digital data signal arrays
                            channelByteData[i] = JenningsUtil.rawDataToSingleChannel(data, targetFormat.getChannels(), i, targetFormat.getSampleSizeInBits() / 8); // Split raw data into different channels

                            // Convert multi-byte data to single-value data
                            singleValues[i] = JenningsUtil.multiBytesToSingleDoubles(channelByteData[i], targetFormat.getChannels(), targetFormat.getSampleSizeInBits() / 8, targetFormat.isBigEndian());

                            // Attenuate signal so values are nearly guaranteed to not overflow
                            for (int j = 0; j < singleValues[0].length; j++) {
                                singleValues[i][j] /= (targetFormat.getChannels() * 5000.0);
                            }
                        }

                        // Add all channel data into one master signal (like a single audio channel, i.e. mono)
                        for (int i = 0; i < targetFormat.getChannels(); i++) {
                            for (int k = 0; k < singleValues[0].length; k++) {
                                masterSignal[k] += singleValues[i][k];
                            }
                        }

                        // Transform master signal from time domain to frequency domain
                        complexes = fft.transform(masterSignal, TransformType.FORWARD);

                        // Calculate FFT magnitudes
                        magnitudes = JenningsUtil.calculateFFTMagnitudes(complexes);

                        // Append average threshold values (PER GPIO CHANNEL)
                        for (int j = 0; j < magnitudes.length / 2; j++) { // magnitudes.length / 2 because the second half of the array is mirrored
                            // Determine frequency at the value of j
                            frequency = targetFormat.getSampleRate() * j / magnitudes.length;

                            // Append threshold values for each frequency band
                            if (frequency < channelDefinitions[0]) {
                                // Do for GPIO channel 0
                                thresholds[0] += magnitudes[j];
                                channelCounters[0]++;
                            } else if (frequency < channelDefinitions[1]) {
                                // Do for GPIO channel 1
                                thresholds[1] += magnitudes[j];
                                channelCounters[1]++;
                            } else if (frequency < channelDefinitions[2]) {
                                // Do for GPIO channel 2
                                thresholds[2] += magnitudes[j];
                                channelCounters[2]++;
                            } else if (frequency < channelDefinitions[3]) {
                                // Do for GPIO channel 3
                                thresholds[3] += magnitudes[j];
                                channelCounters[3]++;
                            } else if (frequency < channelDefinitions[4]) {
                                // Do for GPIO channel 4
                                thresholds[4] += magnitudes[j];
                                channelCounters[4]++;
                            } else if (frequency < channelDefinitions[5]) {
                                // Do for GPIO channel 5
                                thresholds[5] += magnitudes[j];
                                channelCounters[5]++;
                            } else if (frequency < channelDefinitions[6]) {
                                // Do for GPIO channel 6
                                thresholds[6] += magnitudes[j];
                                channelCounters[6]++;
                            } else if (frequency < channelDefinitions[7]) {
                                // Do for GPIO channel 7
                                thresholds[7] += magnitudes[j];
                                channelCounters[7]++;
                            }
                        }
                    }

                } catch (Exception e) {
                    System.err.println("ERROR: Audio analysis failed. The system" +
                            " was unable to read data from file.");
                    e.printStackTrace();
                }
            }
        }

        /*
         * Calculate averages per GPIO channel threshold.
         *
         * The Math.pow() mathematical function is there to serve as a constant
         * scalar for the threshold sensitivity, so that the user will typically
         * only use scalar values around 1.0 to set the target threshold
         * sensitivity.
         */
        for (int i = 0; i < thresholds.length; i++) {
            thresholds[i] /= (channelCounters[i] * Math.pow(10.0, 1.0) * thresholdSensitivity);
        }

        // Account for threshold smoothing
        double average = 0.0;
        double difference;
        for (int i = 0; i < thresholds.length; i++) {
            average += thresholds[i] / thresholds.length; // Calculate master average
        }
        for (int i = 0; i < 8; i++) {
            difference = thresholds[i] - average;
            thresholds[i] -= difference * properties.getThresholdSmoothness();
        }

        // END TESTING

        // Update thresholds in system properties
        properties.setThresholds(thresholds);

        // Finalize objects
        try {
            line.drain();
            line.stop();
            line.close();
            decodedInput.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    /**
     * Determines the maximum frequency of the input file. The Nyquist
     * Rate is equivalent to the sampling rate, but due to Nyquist's theories,
     * we are assigning the maximum possible frequency in the audio file as follows:
     * <p>
     * Maximum Frequency = Nyquist Rate / 2.0
     *
     * @param properties  The system properties.
     * @param audioFormat The target audio format.
     */
    private static void determineHalfNyquist(GLSConfig properties, AudioFormat audioFormat) {
        // Declare method variables and objects
        double maxFrequency;
        double[] channelDefinitions = properties.getChannelDefinitions();

        // Determine half Nyquist Rate
        maxFrequency = audioFormat.getSampleRate() / 2.0;

        // Update channel definitions
        channelDefinitions[7] = maxFrequency;

        if (channelDefinitions[channelDefinitions.length - 2] >= maxFrequency) {
            throw new IllegalStateException("The Nyquist Rate is too low for this file." +
                    "\nTry updating your channel definitions (make the channels use shorter " +
                    "frequency bands), or just use a file with a standard sampling rate.");
        }

        // Set the new channel definitions in system properties
        properties.setChannelDefinitions(channelDefinitions);
    }

}