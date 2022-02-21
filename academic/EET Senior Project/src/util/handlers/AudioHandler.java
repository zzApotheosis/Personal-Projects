/*
 * Created by Steven Jennings on 02 November 2017.
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

package util.handlers;

import main.GLSConfig;
import main.SystemMode;
import org.apache.commons.math3.complex.Complex;
import org.apache.commons.math3.transform.DftNormalization;
import org.apache.commons.math3.transform.FastFourierTransformer;
import org.apache.commons.math3.transform.TransformType;
import util.JenningsUtil;
import util.controllers.GPIOController;
import util.controllers.SequenceController;

import javax.sound.sampled.*;
import java.io.File;
import java.io.IOException;

/**
 * @author Steven Jennings
 * @version 0.0.0
 */
public class AudioHandler {

    /**
     * AudioHandler should not be instantiated.
     */
    private AudioHandler() {
        // Not instantiable
    }

    /**
     * Begins the execution of the Generic Light Show. This
     * passes on information to other modules which perform very detailed
     * operations on the digital audio signal.
     *
     * @param properties The system properties.
     * @param audioFile  The input file.
     */
    public static void initiateLightShow(GLSConfig properties, File audioFile) {
        try {
            AudioInputStream in;
            in = AudioSystem.getAudioInputStream(audioFile);
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
            rawOutput(properties, decodedFormat, din);
            in.close();
        } catch (Exception e) {
            System.err.println("Song playback error!");
            e.printStackTrace();
        }
    }

    /**
     * Performs raw audio IO operations and sends audio signal data to the
     * GPIO Controller.
     *
     * @param properties   The system properties.
     * @param targetFormat The target format of the audio data.
     * @param din          The input stream of audio data.
     * @throws IOException              For any IO problems.
     * @throws LineUnavailableException In case the output data line is unavailable.
     */
    private static void rawOutput(GLSConfig properties,
                                  AudioFormat targetFormat,
                                  AudioInputStream din) throws IOException, LineUnavailableException {
        // Declare method variables and objects
        byte[] rawData = new byte[properties.getBufferSize()];
        byte[][] channelDataBytes = new byte[targetFormat.getChannels()][rawData.length / targetFormat.getChannels()];
        double[][] channelDataValues = new double[targetFormat.getChannels()][channelDataBytes[0].length / (targetFormat.getSampleSizeInBits() / 8)];
        double[] masterSignal = new double[channelDataValues[0].length];
        double[] masterMagnitudes = null;
        FastFourierTransformer fft = new FastFourierTransformer(DftNormalization.STANDARD);
        Complex[] complexes;

        SourceDataLine line = JenningsUtil.getLine(targetFormat);
        if (line != null) {
            // Start listening for audio data
            line.start();

            // For custom sequencing mode, start the sequence
            if (properties.getMode() == SystemMode.custom) {
                Thread customSequencingThread = new Thread(SequenceController::start);
                customSequencingThread.start();
            }

            int nBytesRead = 0, nBytesWritten = 0;
            while (nBytesRead != -1) {
                nBytesRead = din.read(rawData, 0, rawData.length);

                // Check for any data read
                if (nBytesRead != -1) {

                    /*
                     * Branch for automatic and manual mode.
                     *
                     * Custom sequencing mode skips these calculations.
                     */
                    if (properties.getMode() == SystemMode.automatic || properties.getMode() == SystemMode.manual) {
                        // Split raw data into channels
                        for (int i = 0; i < targetFormat.getChannels(); i++) {
                            channelDataBytes[i] = JenningsUtil.rawDataToSingleChannel(rawData, targetFormat.getChannels(), i, targetFormat.getSampleSizeInBits() / 8);
                        }

                        // Convert multi-byte data to single-value data
                        for (int i = 0; i < targetFormat.getChannels(); i++) {
                            channelDataValues[i] = JenningsUtil.multiBytesToSingleDoubles(channelDataBytes[i], targetFormat.getChannels(), targetFormat.getSampleSizeInBits() / 8, targetFormat.isBigEndian());

                            // Attenuate data again (same as analysis)
                            for (int j = 0; j < channelDataValues[0].length; j++) {
                                channelDataValues[i][j] /= (targetFormat.getChannels() * 5000.0);
                            }
                        }

                        // Reset master signal's value
                        for (int i = 0; i < masterSignal.length; i++) {
                            masterSignal[i] = 0.0;
                        }

                        // Combine all channels' signals into one master signal in preparation for FFT
                        for (int i = 0; i < targetFormat.getChannels(); i++) {
                            for (int j = 0; j < channelDataValues[0].length; j++) {
                                masterSignal[j] += channelDataValues[i][j];
                            }
                        }

                        // Perform FFT
                        complexes = fft.transform(masterSignal, TransformType.FORWARD);

                        // Calculate magnitudes
                        masterMagnitudes = JenningsUtil.calculateFFTMagnitudes(complexes);
                    }

                    // Write data to output data line
                    nBytesWritten = line.write(rawData, 0, nBytesRead);

                    // Update GPIO channels AFTER audio data has been outputted to AUX OUT
                    if (properties.getMode() == SystemMode.automatic || properties.getMode() == SystemMode.manual) {
                        GPIOController.update(properties, masterMagnitudes);
                    }
                }
            }

            // Stop playback
            line.drain();
            line.stop();
            line.close();
            din.close();

            // If the system is running in custom sequencing mode,
            // send the signal to stop if it's still running
            if (SequenceController.isActive()) {
                SequenceController.stop();
            }
        }
    }
}