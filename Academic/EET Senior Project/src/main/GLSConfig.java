/*
 * Created by Steven Jennings on 09 November 2017.
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

package main;

import javax.sound.sampled.AudioFormat;

/**
 * @author Steven Jennings
 * @version 0.0.0
 */
public class GLSConfig {

    /*
     * These are the configuration values for the system.
     *
     * thresholds = An array of 8 double values representing the threshold
     * values for the 8 channels.
     *
     * thresholdSmoothness = A value that determines how smooth the final set
     * of threshold values should be. This value must not be less than 0.0, and
     * must not be greater than 1.0. It is a percentage value. A value of 0.0
     * means there is no smoothing, and a value of 1.0 means they will be
     * perfectly smoothed (I.E. all constant values)
     *
     * channelDefinitions = An array of 8 double values representing the upper
     * frequency limit of each channel's band. The last value is calculated
     * in the system based on the Nyquist rate.
     *
     * bufferSize = The size of the buffer of raw audio data to process.
     * Note: This value times the number of channels in the target audio format
     * MUST be a power of two.
     *
     * thresholdSensitivity = A scalar value representing the sensitivity of
     * the threshold values. For use only with automatic system mode.
     *
     * updateFrequency = A scalar value representing the inverse frequency at
     * which the GPIO channels should update. For use with Automatic and Manual
     * modes only.
     *
     * triggerCount = A constant value that counts the number of magnitudes
     * greater than the threshold value of that particular channel. If this
     * value is set too high, then the channel might struggle to ever activate.
     *
     * targetFormat = The target AudioFormat
     *
     * fileName = The name of the chosen audio file, without its path.
     */
    private SystemMode mode;
    private double[] thresholds;
    private double thresholdSmoothness;
    private double[] channelDefinitions;
    private int bufferSize;
    private double thresholdSensitivity;
    private int updateFrequency;
    private int triggerCount;
    private AudioFormat targetFormat;
    private String fileName;

    // Constructors

    /**
     * Default constructor initializes configuration values to default.
     */
    public GLSConfig() {
        initialize();
    }

    // Methods

    private void initialize() {
        this.mode = SystemMode.automatic;
        this.thresholds = new double[8];
        this.thresholdSmoothness = 0.0;
        this.channelDefinitions = new double[]{
                150.0,    // Channel 0: 0Hz     to   150Hz
                250.0,    // Channel 1: 150Hz   to   250Hz
                400.0,    // And so forth...
                600.0,
                900.0,
                1250.0,
                2500.0,
                -1.0      /*
                           * Channel 7 will be changed in the analyze() method
                           * based on the Nyquist rate of the input file, but will be:
                           *
                           * 6kHz   to   Nyquist Rate
                           */
        };
        this.bufferSize = (int) Math.pow(2.0, 14.0);
        this.thresholdSensitivity = 1.0;
        this.updateFrequency = 0;
        this.triggerCount = 3;
        this.targetFormat = null;
        this.fileName = null;
    }

    // Getters and Setters

    public AudioFormat getTargetFormat() {
        return targetFormat;
    }

    public double getThresholdSensitivity() {
        return thresholdSensitivity;
    }

    public double[] getChannelDefinitions() {
        return channelDefinitions;
    }

    public double[] getThresholds() {
        return thresholds;
    }

    public double getThresholdSmoothness() {
        return thresholdSmoothness;
    }

    public int getBufferSize() {
        return bufferSize;
    }

    public int getTriggerCount() {
        return triggerCount;
    }

    public int getUpdateFrequency() {
        return updateFrequency;
    }

    public String getFileName() {
        return fileName;
    }

    public SystemMode getMode() {
        return mode;
    }

    public void setBufferSize(int bufferSize) {
        this.bufferSize = bufferSize;
    }

    public void setChannelDefinitions(double[] channelDefinitions) {
        this.channelDefinitions = channelDefinitions;
    }

    public void setFileName(String fileName) {
        this.fileName = fileName;
    }

    public void setMode(SystemMode mode) {
        this.mode = mode;
    }

    public void setTargetFormat(AudioFormat targetFormat) {
        this.targetFormat = targetFormat;
    }

    public void setThresholds(double[] thresholds) {
        this.thresholds = thresholds;
    }

    public void setThresholdSmoothness(double thresholdSmoothness) {
        this.thresholdSmoothness = thresholdSmoothness;
    }

    public void setThresholdSensitivity(double thresholdSensitivity) {
        this.thresholdSensitivity = thresholdSensitivity;
    }

    public void setTriggerCount(int triggerCount) {
        this.triggerCount = triggerCount;
    }

    public void setUpdateFrequency(int updateFrequency) {
        this.updateFrequency = updateFrequency;
    }
}