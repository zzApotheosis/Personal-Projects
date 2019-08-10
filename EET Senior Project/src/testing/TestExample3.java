/*
 * Created by Steven Jennings on 15 October 2017.
 */

package testing;

import javaWavFileIO.WavFile;

import java.io.File;

/**
 * @author Steven Jennings
 * @version 0.0.0
 */
public class TestExample3 {

    public static void main(String[] args) throws Exception {
        // Test input data/signal
        double[] testData;
        double[] channel1;
        double[] channel2;
        StringBuilder out = new StringBuilder();

        // Size of data buffer
        int sampleCount = (int) Math.pow(2.0, 8.0);

        // Open the wav file specified as the first argument
        WavFile wavFile = WavFile.openWavFile(new File("test.wav"));

        // Display information
        wavFile.display();

        Thread.sleep(500);

        // Initialize data buffer (sampleCount * numChannels = numFrames)
        testData = new double[sampleCount * wavFile.getNumChannels()]; // could be any power of 2
        channel1 = new double[testData.length / 2];
        channel2 = new double[testData.length / 2];

        // Read data into data buffer
        wavFile.readFrames(testData, sampleCount);

        // Store data from channel 1 into array
        for (int i = 0, j = 0; i < testData.length; i += 2, j++) {
            channel1[j] = testData[i];
        }

        // Store data from channel 2 into array
        for (int i = 1, j = 0; i < testData.length; i += 2, j++) {
            channel2[j] = testData[i];
        }

        for (int i = 0; i < channel1.length; i++) {
            System.out.print  (i + " - ");
            System.out.print  (channel1[i]);
            System.out.print  (" : ");
            System.out.println(channel2[i]);
            System.out.println();
        }

        wavFile.close();
    }

}