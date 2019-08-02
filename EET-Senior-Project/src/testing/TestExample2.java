/*
 * Created by Steven Jennings on 14 October 2017.
 */

package testing;

import org.apache.commons.math3.complex.Complex;
import org.apache.commons.math3.transform.DftNormalization;
import org.apache.commons.math3.transform.FastFourierTransformer;
import org.apache.commons.math3.transform.TransformType;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.PrintWriter;

/**
 * @author Steven Jennings
 * @version 0.0.0
 */
public class TestExample2 {

    public static void main(String[] args) throws Exception {
        // StringBuilder for output
        StringBuilder out = new StringBuilder();

        // Create test input signal
        double[] testSignal = new double[(int) Math.pow(2.0, 20)];

        // Sampling rate
        double samplingRate = 44100.0; // Arbitrary for now

        // Create typical sine wave for test signal
        for (int i = 0; i < testSignal.length; i++) {
            testSignal[i] = 10 * Math.sin(2.0 * Math.PI * (1.0 / 100.0) * i); // sin(2*pi*f*t)
        }

        FastFourierTransformer fft = new FastFourierTransformer(DftNormalization.STANDARD);
        Complex[] complexes = fft.transform(testSignal, TransformType.FORWARD);

        double magnitude;
        for (int i = 0; i < complexes.length; i++) {
            magnitude = Math.sqrt(Math.pow(complexes[i].getReal(), 2.0) + Math.pow(complexes[i].getImaginary(), 2.0));
            if (i % 50 == 0) {
                if (i != 0) out.append("\n\n");
                out.append("F: ");
                out.append(samplingRate * i / testSignal.length);
                out.append("\n");
                out.append("M: ");
                out.append(magnitude);
            }
        }

        // Write to output file
        PrintWriter printWriter = new PrintWriter(new BufferedWriter(new FileWriter("TestExample2Out.txt")));
        printWriter.print(out.toString());
        printWriter.close();
    }

}