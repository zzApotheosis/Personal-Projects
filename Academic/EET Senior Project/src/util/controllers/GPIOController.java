/*
 * Created by Steven Jennings on 25 August 2017.
 *
 * Pins used:
 * Channel 0 - GPIO 0 (Pin 11)
 * Channel 1 - GPIO 1 (Pin 12)
 * Channel 2 - GPIO 2 (Pin 13)
 * Channel 3 - GPIO 3 (Pin 15)
 * Channel 4 - GPIO 4 (Pin 16)
 * Channel 5 - GPIO 5 (Pin 18)
 * Channel 6 - GPIO 6 (Pin 22)
 * Channel 7 - GPIO 7 (Pin 7)
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

package util.controllers;

import com.pi4j.io.gpio.*;
import main.GLSConfig;
import main.SystemMode;
import util.JenningsUtil;

import javax.sound.sampled.AudioFormat;

/**
 * @author Steven Jennings
 * @version 0.0.0
 */
public class GPIOController {

    // Class fields
    private static final GpioController gpioController = GpioFactory.getInstance();
    private static final GpioPinDigitalOutput[] pins = new GpioPinDigitalOutput[]{
            gpioController.provisionDigitalOutputPin(RaspiPin.GPIO_00, "Channel 0", PinState.LOW),
            gpioController.provisionDigitalOutputPin(RaspiPin.GPIO_01, "Channel 1", PinState.LOW),
            gpioController.provisionDigitalOutputPin(RaspiPin.GPIO_02, "Channel 2", PinState.LOW),
            gpioController.provisionDigitalOutputPin(RaspiPin.GPIO_03, "Channel 3", PinState.LOW),
            gpioController.provisionDigitalOutputPin(RaspiPin.GPIO_04, "Channel 4", PinState.LOW),
            gpioController.provisionDigitalOutputPin(RaspiPin.GPIO_05, "Channel 5", PinState.LOW),
            gpioController.provisionDigitalOutputPin(RaspiPin.GPIO_06, "Channel 6", PinState.LOW),
            gpioController.provisionDigitalOutputPin(RaspiPin.GPIO_07, "Channel 7", PinState.LOW)
    };
    private static int channelDelays; // Information provided by the user
    private static int delayCounter; // The counter for channelDelays
    private static double[] thresholds; // The thresholds of the system, determined upon analysis

    /**
     * GPIOController should not be instantiated.
     */
    private GPIOController() {
        // Not instantiable
    }

    /**
     * Initializer for this mostly-static class.
     *
     * @param properties The system properties.
     */
    public static void initialize(GLSConfig properties) {
        // Set fields that are utilized for every SystemMode
        channelDelays = properties.getUpdateFrequency(); // Update delays for each channel
        delayCounter = 0; // Counter, which is limited to the static field channelDelays

        // Branch per SystemMode
        if (properties.getMode() == SystemMode.automatic || properties.getMode() == SystemMode.manual) {
            thresholds = properties.getThresholds();
        }
    }

    /**
     * Updates the states of the GPIO pins, based on the current digital audio
     * signal.
     *
     * @param properties The system properties.
     * @param magnitudes The magnitudes of the digital audio signal's FFT.
     */
    public static void update(GLSConfig properties, double[] magnitudes) {
        // Fetch properties; declare method variables and objects
        SystemMode mode = properties.getMode();
        double[] channelDefinitions = properties.getChannelDefinitions();
        AudioFormat audioFormat = properties.getTargetFormat();
        double frequency = 0.0;
        int index = 0;
        int counter;

        // Update pin states for automatic mode
        if (mode == SystemMode.automatic || mode == SystemMode.manual) {
            // Update GPIO pin states
            if (delayCounter >= channelDelays || channelDelays <= 0) {
                for (int i = 0; i < 8; i++) {
                    // Reset counter
                    counter = 0;

                    // Loop through frequency bands
                    while (frequency < channelDefinitions[i]) {
                        // Increment counter if the value at index is greater than the threshold
                        if (magnitudes[index] > thresholds[i]) {
                            counter++;
                        }

                        // Determine next frequency
                        frequency = audioFormat.getSampleRate() * ++index / magnitudes.length;
                    }

                    // Determine state of the channel
                    if (counter >= properties.getTriggerCount()) {
                        high(i);
                    } else {
                        low(i);
                    }
                }

                // Reset delay counter
                delayCounter = 0;
            } else {
                delayCounter++;
            }
        }
    }

    /**
     * Sets the shutdown states for each pin so that when the GPIO Controller
     * is shut down, the state of each pin will settle at the correct state.
     *
     * @param state The state at which the pins should shut down to.
     */
    public static void setShutdownOptions(boolean state) {
        for (GpioPinDigitalOutput singlePin : pins) {
            singlePin.setShutdownOptions(true, state ? PinState.HIGH : PinState.LOW);
        }
    }

    /**
     * This method should be called when the system is shutting down. This
     * ensures that the GPIO pins are shut down to the correct state, and
     * properly passes GPIO control back to the Raspberry Pi.
     */
    public static void shutdown() {
        gpioController.shutdown();
    }

    /**
     * Checks whether or not the requested channel is a valid channel.
     * <p>
     * Valid channels: 0 - 7, for a total of 8 channels (the relay has 8 channels)
     *
     * @param channel The requested channel to manipulate.
     * @return Whether or not the requested channel is valid.
     */
    private static boolean channelIsValid(int channel) {
        return 0 <= channel && channel <= 7;
    }

    /**
     * Toggles the state of the given channel.
     *
     * @param channel The channel to modify.
     */
    public static void toggleChannel(int channel) {
        // Check for valid channel designation
        if (!channelIsValid(channel)) {
            JenningsUtil.println("Invalid GPIO toggleChannel() attempt for non-existent channel: " + channel);
            return;
        }

        // Execute pin operation
        pins[channel].toggle();
    }

    /**
     * Sets the given channel to logic "1".
     *
     * @param channel The channel to modify.
     */
    public static void high(int channel) {
        // Check for valid channel designation
        if (!channelIsValid(channel)) {
            JenningsUtil.println("Invalid GPIO high() attempt for non-existent channel: " + channel);
            return;
        }

        // Check if pin is already high()
        if (pins[channel].isHigh()) {
            return;
        }

        // Execute pin operation
        pins[channel].high();
    }

    /**
     * Sets the given channel to logic "0".
     *
     * @param channel The channel to modify.
     */
    public static void low(int channel) {
        // Check for valid channel designation
        if (!channelIsValid(channel)) {
            JenningsUtil.println("Invalid GPIO low() attempt for non-existent channel: " + channel);
            return;
        }

        // Check if pin is already low()
        if (pins[channel].isLow()) {
            return;
        }

        // Execute pin operation
        pins[channel].low();
    }

    /**
     * Sets the state of the given channel.
     *
     * @param channel The channel to modify.
     * @param state   The state to set the channel.
     */
    public static void setPinState(int channel, boolean state) {
        // Check for valid channel designation
        if (!channelIsValid(channel)) {
            JenningsUtil.println("Invalid GPIO setPinState() attempt for non-existent channel: " + channel);
            return;
        }

        // Check if pin is already set to requested state
        if (pins[channel].isHigh() && state || pins[channel].isLow() && !state) {
            return;
        }

        // Execute pin operation
        pins[channel].setState(state);
    }

    /**
     * Sets each channel to a logic "0".
     */
    public static void reset() {
        // Set all channels to low
        for (GpioPinDigitalOutput singlePin : pins) {
            if (singlePin.isHigh()) {
                singlePin.low();
            }
        }
    }

    /**
     * Sets each channel to a logic "1".
     */
    public static void full() {
        // Set all channels to high
        for (GpioPinDigitalOutput singlePin : pins) {
            if (singlePin.isLow()) {
                singlePin.high();
            }
        }
    }
}