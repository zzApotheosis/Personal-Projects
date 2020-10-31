package testing;

import javaWavFileIO.WavFile;
import org.apache.commons.math3.complex.Complex;
import org.apache.commons.math3.transform.DftNormalization;
import org.apache.commons.math3.transform.FastFourierTransformer;
import org.apache.commons.math3.transform.TransformType;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.PrintWriter;

public class TestExample1 {
    public static void main(String[] args) {
        StringBuilder out = new StringBuilder();

        try {
            // Open the wav file
            WavFile wavFile = WavFile.openWavFile(new File("testWavOut.wav"));

            // Display information about the wav file
            wavFile.display();

            // Get the number of audio channels in the wav file
            int numChannels = wavFile.getNumChannels();

            // Create a buffer of 128 frames
            // (1 frame = number of channels)
            // (number of channels = a constant value stored in the "fmt" sub-chunk)
            double[] buffer = new double[128 * numChannels];

            int framesRead;
            double min = Double.MAX_VALUE;
            double max = Double.MIN_VALUE;

            // TESTING
            FastFourierTransformer fft = new FastFourierTransformer(DftNormalization.STANDARD);
            Complex[] complexes;
            double[] frequencyDomain = new double[buffer.length];
            double samplingFrequency = wavFile.getSampleRate();

            do {
                // Read frames into buffer
                framesRead = wavFile.readFrames(buffer, 100);

                // Loop through frames and look for minimum and maximum value
                for (int s = 0; s < framesRead * numChannels; s++) {
                    if (buffer[s] > max) max = buffer[s];
                    if (buffer[s] < min) min = buffer[s];
                }

                // TESTING
                // Perform FFT on the data buffer
                complexes = fft.transform(buffer, TransformType.FORWARD);

                for (int i = 0; i < complexes.length; i++) {
                    frequencyDomain[i] = Math.sqrt(Math.pow(complexes[i].getReal(), 2.0) + Math.pow(complexes[i].getImaginary(), 2.0));
                }

                for (int i = 0; i < frequencyDomain.length / 2; i++) {
                    System.out.println("Frequency: " + (samplingFrequency * i / frequencyDomain.length)
                            + "    Energy Component: " + frequencyDomain[i]);
                    out.append("Frequency: " + (samplingFrequency * i / frequencyDomain.length)
                            + "    Energy Component: " + frequencyDomain[i] + "\n");
                }

            } while (framesRead != 0);

            // Close the wavFile
            wavFile.close();

            // Output the minimum and maximum value
//            System.out.printf("Min: %f, Max: %f\n", min, max);

            PrintWriter printWriter = new PrintWriter(new BufferedWriter(new FileWriter("Output.txt")));
            printWriter.print(out.toString());
            printWriter.close();
        } catch (Exception e) {
            System.err.println(e);
        }
    }
}