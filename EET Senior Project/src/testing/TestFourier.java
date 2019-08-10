package testing;

import org.apache.commons.math3.complex.Complex;
import org.apache.commons.math3.transform.DftNormalization;
import org.apache.commons.math3.transform.FastFourierTransformer;
import org.apache.commons.math3.transform.TransformType;

public class TestFourier {

    public static void main(String[] args) {

        double samplingFrequency = 10; //hz, You will know this from your data and need to set it here


        double[] frequencyDomain = new double[input.length];

        FastFourierTransformer transformer = new FastFourierTransformer(DftNormalization.STANDARD);

        try {
            Complex[] complex = transformer.transform(input, TransformType.FORWARD);

            for (int i = 0; i < complex.length; i++) {
                double real = (complex[i].getReal());
                double imaginary = (complex[i].getImaginary());

                frequencyDomain[i] = Math.sqrt((real * real) + (imaginary * imaginary));
            }

        } catch (IllegalArgumentException e) {
            System.out.println(e);
        }

        //only to frequencyDomain.length/2 since second half is mirror image or first half
        for (int i = 0; i < frequencyDomain.length / 2; i++) {
            double frequency = samplingFrequency * i / frequencyDomain.length;
            System.out.println("Frequency: " + frequency + "\t\tEnergyComponent: " + frequencyDomain[i]);
        }
    }

    static double[] input = new double[]{
            0.017077407, //sample at 0 seconds
            1.611895528, //sample at 0.1 seconds
            2.063967663, //sample at 0.2 seconds
            1.598492541, //etc
            0.184678933,
            0.02654732,
            0.165869218,
            1.026139745,
            1.914179294,
            2.523684208,
            1.71795312,
            0.932131202,
            1.097366772,
            1.107912105,
            2.843777623,
            2.503608192,
            2.540595787,
            2.048111122,
            1.515498608,
            1.828077941,
            2.400006658,
            3.562953532,
            3.34333491,
            2.620231348,
            2.769874641,
            2.423059324,
            2.11147835,
            3.473525478,
            4.504105599,
            4.325642774,
            3.963498242,
            2.842688545,
            2.573038184,
            3.434226007,
            4.924115479,
            4.876122332,
            4.553580015,
            3.92554604,
            3.804585546,
            3.476610932,
            4.535171252,
            5.398007229,
            5.729933758,
            5.573444511,
            4.487695977,
            4.133046459,
            4.796637209,
            5.091399617,
            6.420441446,
            6.473462022,
            5.663322311,
            4.866446009,
            4.840966187,
            5.329697081,
            6.746910181,
            6.580067494,
            7.140083322,
            6.243532245,
            4.960520462,
            5.100901901,
            6.794495306,
            6.959324497,
            7.194674358,
            7.035874424
    };
}