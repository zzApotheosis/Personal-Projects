/*
 * Created by Steven Jennings on 06 November 2017.
 *
 * The sequencer file's syntax goes as follows:
 * <time> <a>,<b>,<c>,<d>,<e>,<f>,<g>,<h>
 * where <a>, <b>, etc. are the defined states of their respective channels, and
 * <time> is the time between the target action and the previous action.
 *
 * <a> = Channel 0
 * <b> = Channel 1
 * <c> = Channel 2
 * <d> = Channel 3
 * <e> = Channel 4
 * <f> = Channel 5
 * <g> = Channel 6
 * <h> = Channel 7
 *
 * The possible states go as follows:
 * "1" = Channel High
 * "0" = Channel Low
 * "T" or "t" = Channel Toggle
 * Every other character = Do nothing (Best represented by "X" or "x")
 *
 * Example sequencer:
 * 0.0    1,0,0,0,0,0,0,0
 * 200.0  X,1,X,X,X,X,X,X
 * 400.0  1,X,0,X,1,X,0,X
 * 1000.0 X,1,X,0,X,1,X,0
 * 1500.0 1,1,1,1,1,1,1,1
 * 133.7  T,X,T,X,T,X,T,X
 * 500.0  T,T,T,T,T,T,T,T
 * 234.5  X,T,X,T,X,T,X,T
 *
 * In this example, these actions take place:
 * At the beginning of playback, set channel 0 to on.
 * After 200 milliseconds (ms), set channel 1 to on
 * After 400 ms, set channels 0 and 4 to on, 2 and 6 to off
 * After 1000 ms, set channels 1 and 5 to on, 3 and 7 to off
 * After 1500 ms, set every channel to on
 * After 133.7 ms, toggle channels 0/2/4/6
 * After 500 ms, toggle all channels
 * After 234.5 ms, toggle channels 1/3/5/7
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

import util.JenningsUtil;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.LinkedList;
import java.util.Scanner;
import java.util.StringTokenizer;

/**
 * @author Steven Jennings
 * @version 0.0.0
 */
public class SequenceController {

    // Class fields
    private static File sequencer;
    /*
     * The list of actions for the Sequence Controller to take for the
     * Generic Light Show. This is determined upon calling interpretCustomFile().
     */
    private static LinkedList<Double> timeline;
    private static LinkedList<String> actionList;
    private static int actionCount;
    private static double markedTime;
    private static boolean active;

    /**
     * Initializes the mostly-static Sequence Controller.
     */
    public static void initialize() {
        sequencer = JenningsUtil.selectFile("Select Custom Sequencer");
        if (sequencer == null) { // Check for null sequencer
            JenningsUtil.println("No sequencer was selected. Exiting Generic Light Show.");
            System.exit(-1);
        }
        timeline = new LinkedList<>();
        actionList = new LinkedList<>();
        actionCount = 0;
        active = false;
    }

    /**
     * Interprets the selected sequencer file so that the Sequence Controller
     * knows when to control the GPIO channels, and also which pins to control.
     */
    public static void interpretCustomFile() {
        // Declare method variables and objects
        Scanner scanner = null;

        // Initialize scanner
        try {
            scanner = new Scanner(sequencer);
        } catch (FileNotFoundException e) {
            System.err.println("ERROR: Sequencer interpreter failed." +
                    "\nExiting system.");
            System.exit(-1);
        }

        // Interpret sequencer, assuming properly formatted sequencer
        while (scanner.hasNext()) {
            timeline.add(Double.parseDouble(scanner.next()) * Math.pow(10.0, 6.0)); // Time data, converted from milli to nano
            actionList.add(scanner.next().trim()); // Channel data
            scanner.nextLine(); // Skip the rest of the line
            actionCount++;
        }
    }

    /**
     * Begins the custom sequencer.
     */
    public static void start() {
        // Set the class as active
        active = true;

        // Create temporary variables
        int index = 0;
        boolean actionAvailable;

        // Capture the start time
        markedTime = System.nanoTime();

        // Run the custom sequencer
        while (index < actionCount && active) {
            // Check for the next action to take
            actionAvailable = checkForNextAction(index);
            if (actionAvailable) {
                updateSequence(index);
                index++;
            }
        }

        // Once finished with the sequence, go to inactive state
        active = false;
    }

    /**
     * Checks for whether or not the next index in the action list has been
     * reached.
     *
     * @param i The index of the action line to evaluate.
     * @return Whether or not the action was completed.
     */
    private static boolean checkForNextAction(int i) {
        if (System.nanoTime() - markedTime > timeline.get(i)) {
            return true;
        } else {
            return false;
        }
    }

    /**
     * Updates the GPIO pins to their appropriate values based on the current
     * index/line in the custom sequencer file.
     *
     * @param i The index of the action line to evaluate.
     */
    private static void updateSequence(int i) {
        // Create StringTokenizer for selected action
        StringTokenizer stringTokenizer = new StringTokenizer(actionList.get(i), ",");

        // Update the reference time
        markedTime = System.nanoTime();

        // Update GPIO pins based on actionList
        int index = 0;
        while (stringTokenizer.hasMoreTokens()) {
            // Determine action to take
            switch (stringTokenizer.nextToken().toLowerCase()) {
                case "0":
                    GPIOController.low(index);
                    break;
                case "1":
                    GPIOController.high(index);
                    break;
                case "t":
                    GPIOController.toggleChannel(index);
                    break;
            }

            // Increment index
            index++;
        }
    }

    /**
     * Checks whether or not this class is currently active.
     *
     * @return The state of the Sequence Controller.
     */
    public static boolean isActive() {
        return active;
    }

    /**
     * Stops all activities within this class.
     */
    public static void stop() {
        active = false;
    }

}