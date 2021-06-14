/*
 * Created by Steven Jennings on 28 October 2017.
 */

package testing;

import org.apache.commons.math3.complex.Complex;
import org.apache.commons.math3.transform.DftNormalization;
import org.apache.commons.math3.transform.FastFourierTransformer;
import org.apache.commons.math3.transform.TransformType;
import util.JenningsUtil;

import javax.sound.sampled.*;
import javax.swing.*;
import java.io.*;

/**
 * @author Steven Jennings
 * @version 0.0.0
 */
public class TestBench {

    private volatile static int test = 0;

    private TestBench() {
        // Class is not instantiable
    }

    public synchronized static void main(String[] args) {
        File file = showFileChooser();

        Thread t1 = new Thread(() -> {
            System.out.println("Now playing song!");
            playSong(file);
        });

        t1.start();

        while (t1.isAlive()) {
            try {
                Thread.sleep(100);
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }

        System.out.println("Song finished!");
    }

    private static File showFileChooser() {
        File out = null;
        JFileChooser fc = new JFileChooser();
        fc.setDialogTitle("Choose Music File");
        int returnValue = fc.showOpenDialog(null);
        if (returnValue == JFileChooser.APPROVE_OPTION) {
            out = fc.getSelectedFile();
        } else {
            System.exit(0);
        }
        return out;
    }

    private static void playSong(File songFile) {
        try {
            AudioInputStream in;
            in = AudioSystem.getAudioInputStream(songFile);
            AudioInputStream din;
            AudioFormat baseFormat = in.getFormat();
            AudioFormat decodedFormat = new AudioFormat(
                    AudioFormat.Encoding.PCM_SIGNED,
                    baseFormat.getSampleRate(),
                    16,
                    baseFormat.getChannels(),
                    baseFormat.getChannels() * 2,
                    baseFormat.getSampleRate(),
                    false);
            din = AudioSystem.getAudioInputStream(decodedFormat, in);

            // Play now
            rawPlay(decodedFormat, din);
            in.close();
        } catch (Exception e) {
            System.err.println("Song playback error!");
            e.printStackTrace();
        }
    }

    private static void rawPlay(AudioFormat targetFormat, AudioInputStream din) throws IOException, LineUnavailableException {
        byte[] rawData = new byte[(int) Math.pow(2.0, 14.0)];
        FastFourierTransformer fft = new FastFourierTransformer(DftNormalization.STANDARD);
        Complex[] complexes;
        double[] magnitudes = null;
        int counter = 0;
        double[] data;

        byte[][] channelDataBytes = new byte[targetFormat.getChannels()][rawData.length / targetFormat.getChannels()];
        double[][] channelDataValues = new double[channelDataBytes.length][channelDataBytes[0].length / (targetFormat.getSampleSizeInBits() / 8)];
        double[] testMasterSignal = new double[channelDataValues[0].length];
        Complex[] testComplexes;
        double[] testMagnitudes = new double[testMasterSignal.length];

        System.out.println("Title: " + targetFormat.getProperty("name"));
        System.out.println("Sample rate: " + targetFormat.getSampleRate());
        System.out.println("Sample size: " + targetFormat.getSampleSizeInBits());
        System.out.println("Channels: " + targetFormat.getChannels());

        SourceDataLine line = getLine(targetFormat);
        if (line != null) {
            // Start
            line.start();
            int nBytesRead = 0, nBytesWritten = 0;
            while (nBytesRead != -1) {
                nBytesRead = din.read(rawData, 0, rawData.length);

                // Check for any data read
                if (nBytesRead != -1) {
                    // TESTING
                    data = convertRawBytesToAdditiveValues(rawData, targetFormat.isBigEndian());
                    complexes = fft.transform(data, TransformType.FORWARD);
                    magnitudes = calcMagnitudes(complexes);

                    for (int i = 0; i < targetFormat.getChannels(); i++) {
                        channelDataBytes[i] = JenningsUtil.rawDataToSingleChannel(rawData, targetFormat.getChannels(), i, targetFormat.getSampleSizeInBits() / 8);
                    }
                    for (int i = 0; i < targetFormat.getChannels(); i++) {
                        channelDataValues[i] = JenningsUtil.multiBytesToSingleDoubles(channelDataBytes[i], targetFormat.getChannels(), targetFormat.getSampleSizeInBits() / 8, targetFormat.isBigEndian());
                    }
                    // Reset masterSignal
                    for (int i = 0; i < testMasterSignal.length; i++) {
                        testMasterSignal[i] = 0.0;
                    }
                    for (int i = 0; i < channelDataValues[0].length; i++) {
                        for (int j = 0; j < targetFormat.getChannels(); j++) {
                            testMasterSignal[i] += channelDataValues[j][i];
                        }
                    }
                    testComplexes = fft.transform(testMasterSignal, TransformType.FORWARD);
                    testMagnitudes = JenningsUtil.calculateFFTMagnitudes(testComplexes);

                    counter++;
                    if (counter % 100 == 0) {
                        System.out.println(counter);
                    } else if (counter % 234 == 0) {
                        break;
                    }
                    // END TESTING

                    // Write data to buffer
                    nBytesWritten = line.write(rawData, 0, nBytesRead);
                }
            }

            // Execute quick output for last known FFT
            PrintWriter printWriter = new PrintWriter(new BufferedWriter(new FileWriter("resources/Output.txt")));
            for (int i = 0; i < magnitudes.length / 2; i++) {
                printWriter.println("Frequency: " + (targetFormat.getSampleRate() * i / magnitudes.length));
                printWriter.println("Magnitude: " + magnitudes[i] + " --- " + testMagnitudes[i]);
                printWriter.println();
            }
            printWriter.close();

            // Stop playback
            line.drain();
            line.stop();
            line.close();
            din.close();
        }
    }

    private static SourceDataLine getLine(AudioFormat audioFormat) throws LineUnavailableException {
        SourceDataLine res;
        DataLine.Info info = new DataLine.Info(SourceDataLine.class, audioFormat);
        res = (SourceDataLine) AudioSystem.getLine(info);
        res.open(audioFormat);
        return res;
    }

    private static double[] convertRawBytesToAdditiveValues(byte[] in, boolean isBigEndian) {
        int newLength = in.length / 2;
        byte[] left = new byte[newLength];
        byte[] right = new byte[newLength];
        double[] plainLeft;
        double[] plainRight;
        double[] out = new double[newLength / 2];

        left = rawToLeft(in);
        right = rawToRight(in);

        plainLeft = convertToPlainValues(left, isBigEndian);
        plainRight = convertToPlainValues(right, isBigEndian);

        for (int i = 0; i < out.length; i++) {
            out[i] = (plainLeft[i] + plainRight[i]) / 2.0;
        }

        return out;
    }

    private static byte[] rawToLeft(byte[] in) {
        byte[] out = new byte[in.length / 2];

        for (int i = 0; i < out.length; i += 2) {
            out[i] = in[2 * i];
            out[i + 1] = in[2 * i + 1];
        }

        return out;
    }

    private static byte[] rawToRight(byte[] in) {
        byte[] out = new byte[in.length / 2];

        for (int i = 0; i < out.length; i += 2) {
            out[i] = in[2 * i + 2];
            out[i + 1] = in[2 * i + 3];
        }

        return out;
    }

    private static double[] convertToPlainValues(byte[] in, boolean isBigEndian) {
        int[] temp = new int[in.length / 2];
        double[] out = new double[in.length / 2];

        if (isBigEndian) {
            for (int i = 0; i < temp.length; i++) {
                temp[i] |= (int) in[2 * i];
                temp[i] <<= 8;
                temp[i] |= (int) in[2 * i + 1];
            }
        } else {
            for (int i = 0; i < temp.length; i++) {
                temp[i] |= (int) in[2 * i + 1];
                temp[i] <<= 8;
                temp[i] |= (int) in[2 * i];
            }
        }

        for (int i = 0; i < out.length; i++) {
            out[i] = (double) temp[i];
        }

        return out;
    }

    private static double[] calcMagnitudes(Complex[] in) {
        double[] out = new double[in.length];

        for (int i = 0; i < in.length; i++) {
            out[i] = Math.sqrt(Math.pow(in[i].getReal(), 2.0) + Math.pow(in[i].getImaginary(), 2.0)) / 5000.0;
        }

        return out;
    }

    private static double[] averageMagnitudes(double[] left, double[] right) {
        double[] out = new double[left.length];

        for (int i = 0; i < out.length; i++) {
            out[i] = (left[i] + right[i]) / 2.0;
        }

        return out;
    }

}