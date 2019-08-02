/*
 * Created by Steven Jennings on 29 October 2017.
 *
 * This is the software for my Senior Project in Electrical Engineering
 * Technology. I call it the Generic Light Show, a system that can accept
 * any music file as input, and procedurally generate a light show based on
 * the contents of the music file.
 *
 * Note, in order to run the .jar program containing this program, the Pi4J
 * library may need an extra argument for the program to execute:
 * Dpi4j.linking=dynamic
 *
 * This would make the full command:
 * java -Dpi4j.linking=dynamic -jar <filename>.jar <args>
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

/**
 * The main class for the Generic Light Show.
 * <p>
 * Possible modes:
 * automatic (Automatic Threshold)
 * manual (Manual Threshold)
 * custom (Custom sequencing)
 *
 * @author Steven Jennings
 * @version 0.0.0
 */
public class Main {

    /**
     * An unreachable constructor, which forces this class to be
     * not instantiable.
     */
    private Main() {
        // Do nothing. This class should not be instantiable
    }

    /**
     * The main method for the Generic Light Show.
     * <p>
     * The user selects a music file of choice. (Only .wav is confirmed to work)
     * <p>
     * The system analyzes the chosen music file in preparation for execution.
     * <p>
     * Then the system executes the light show, complete with music playback.
     *
     * @param args The arguments of the system. This determines the mode at which
     *             to run the system. If the mode is Automatic Threshold or
     *             Manual Threshold, then the user must supply two additional
     *             arguments: The threshold sensitivity/threshold value
     *             (for Automatic/Manual modes, respectively), as well as a
     *             time scalar for the delay between GPIO pin updates.
     */
    public static void main(String[] args) {
        boolean debug = true; // Debug control variable

        // Run the light show based on config file if no arguments were provided
        if (args.length == 0) {
            runDefault(debug);
        } else { // Otherwise, parse the arguments for a more controlled show
            runUserDefined(args, debug);
        }
    }

    /**
     * Runs the default light show, completely based on a config file.
     *
     * @param dbg Debug flag.
     */
    private static void runDefault(boolean dbg) {
        try {
            // Declare system variables and objects
            GenericLightShow lightShow;

            // Instantiate GenericLightShow
            lightShow = new GenericLightShow();

            // Execute light show
            lightShow.run();
        } catch (Error | Exception e) {
            System.err.println("Arguments are invalid. Syntax goes as follows:");
            System.err.println("java -Dpi4j.linking=dynamic -jar <filename>.jar <mode> <threshold sensitivity> <update frequency> <channel definitions>");
            System.err.println("Note: Custom sequencing mode doesn't require any extra arguments.");
            if (dbg) {
                e.printStackTrace();
            }
        }
    }

    /**
     * Runs the light show based on arguments passed into the program.
     *
     * @param userArguments The user's arguments, from the terminal.
     * @param dbg           Debug flag.
     */
    private static void runUserDefined(String[] userArguments, boolean dbg) {
        try {
            // Declare system variables and objects
            SystemMode mode;
            GenericLightShow lightShow;

            // Fetch designated mode
            switch (userArguments[0].toLowerCase()) {
                case "automatic":
                    mode = SystemMode.automatic;
                    break;
                case "manual":
                    mode = SystemMode.manual;
                    break;
                case "custom":
                    mode = SystemMode.custom;
                    break;
                default:
                    throw new IllegalArgumentException("Arguments are invalid.");
            }

            // Instantiate GenericLightShow
            lightShow = new GenericLightShow();

            // Set fields
            lightShow.properties.setMode(mode); // Mode
            switch (mode) { // Threshold information, based on mode
                case automatic:
                    lightShow.properties.setThresholdSensitivity(Double.parseDouble(userArguments[1]));
                    lightShow.properties.setUpdateFrequency(Integer.parseInt(userArguments[2]));
                    break;
                case manual:
                    double manualThreshold = Double.parseDouble(userArguments[1]);
                    lightShow.properties.setThresholds(new double[]{
                            manualThreshold,
                            manualThreshold,
                            manualThreshold,
                            manualThreshold,
                            manualThreshold,
                            manualThreshold,
                            manualThreshold,
                            manualThreshold
                    });
                    lightShow.properties.setUpdateFrequency(Integer.parseInt(userArguments[2]));
                    break;
                case custom:
                    // Do nothing
                    break;
            }

            // Execute light show
            lightShow.run();
        } catch (Error | Exception e) {
            System.err.println("Arguments are invalid. Syntax goes as follows:");
            System.err.println("java -Dpi4j.linking=dynamic -jar <filename>.jar <mode> <threshold sensitivity> <update frequency> <channel definitions>");
            System.err.println("Note: Custom sequencing mode doesn't require any extra arguments.");
            if (dbg) {
                e.printStackTrace();
            }
        }
    }

}