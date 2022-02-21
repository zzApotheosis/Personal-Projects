/*
 * Created by Steven Jennings on 25 August 2017.
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

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import util.JenningsUtil;
import util.controllers.GPIOController;
import util.controllers.SequenceController;
import util.handlers.AnalysisHandler;
import util.handlers.AudioHandler;

import java.io.*;

/**
 * @author Steven Jennings
 * @version 0.0.0
 */
public class GenericLightShow {

    // ClASS FIELDS ************************************************************

    private File audioFile; // The music file, chosen by the user
    public GLSConfig properties; // Various properties of the project/system

    /*
     * NOTE: The properties field MUST be globally accessible to
     * classes (like Main) with instances of the
     * Generic Light Show class (this class).
     */

    // CLASS CONSTRUCTORS ******************************************************

    /**
     * Constructor initializes class data.
     */
    public GenericLightShow() {
        initialize();
    }

    /**
     * For use with constructors only. Values are fetched from JSON configuration
     * file using getConfig().
     */
    private void initialize() {
        // Initialize basic fields
        this.audioFile = null; // Must be fetched with selectAudioFile()
        getConfig(); // Fetch system configuration
    }

    // CONFIGURATION-RELATED METHODS *******************************************

    /**
     * Fetches the values stored in the configuration file and sets the
     * given properties for the light show.
     */
    private void getConfig() {
        // Declare method variables and objects
        boolean isValid;
        Gson gson = new Gson();

        // Fetch system configuration
        try (Reader reader = new FileReader("GLS_config.json")) {
            this.properties = gson.fromJson(reader, GLSConfig.class);
            validateConfig();
        } catch (IOException e) {
            System.err.println(e.getMessage());
            System.err.println("Unable to read config file. Generating default config...");
            writeDefaultConfig();
            try {
                Thread.sleep(1000);
            } catch (InterruptedException e1) {
                e1.printStackTrace();
            }
            getConfig(); // Try again
            return;
        }

        // Announce successful config read
        System.out.println("\nSuccessfully fetched system configuration.\n");
    }

    /**
     * If a new configuration file needs to be generated, this method will
     * write a fresh, default config file.
     */
    private void writeDefaultConfig() {
        // Declare method variables and objects
        GLSConfig defaultConfig = new GLSConfig();
        Gson gson = new GsonBuilder().setPrettyPrinting().create();

        // Create new JSON config file
        try (Writer writer = new FileWriter("GLS_config.json")) {
            gson.toJson(defaultConfig, writer);
        } catch (IOException e) {
            System.err.println(e.getMessage());
            System.err.println("ERROR: Unable to create default config file.");
            System.err.println("ERROR: Exiting program.");
            System.exit(-1);
        }
    }

    /**
     * Updates the config file based on the current state of the system
     * properties.
     */
    private void updateConfig() {
        // Declare method variables and objects
        Gson gson = new GsonBuilder().setPrettyPrinting().create();

        // Update JSON config file
        try (Writer writer = new FileWriter("GLS_config.json")) {
            gson.toJson(this.properties, writer);
        } catch (IOException e) {
            System.err.println(e.getMessage());
            System.err.println("ERROR: Unable to update config file.");
        }
    }

    /**
     * Makes sure that the values in the configuration are valid.
     */
    private void validateConfig() {
        // Check Threshold smoothness
        this.properties.setThresholdSmoothness(JenningsUtil.clamp(
                this.properties.getThresholdSmoothness(), 0.0, 1.0));

        // Check buffer size
        this.properties.setBufferSize((int) JenningsUtil.clamp(
                this.properties.getBufferSize(), 0, Integer.MAX_VALUE));

        // Check trigger count
        this.properties.setTriggerCount((int) JenningsUtil.clamp(
                this.properties.getTriggerCount(), 1, Integer.MAX_VALUE));

        // Check buffer size
        if (!JenningsUtil.isPowerOf2(this.properties.getBufferSize())) {
            this.properties.setBufferSize((int) Math.pow(2.0, 14.0)); // Default to 16384
        }
    }

    // CLASS METHODS ***********************************************************

    /**
     * Runs the light show based on the mode it is currently in.
     */
    public void run() {
        // Explain the project/system
        explain();

        // Select the system's input audio file
        this.audioFile = JenningsUtil.selectFile("Select Music File");

        // Stop system if no music file was selected
        if (this.audioFile == null) {
            JenningsUtil.println("No music file was selected. Exiting Generic Light Show.");
            System.exit(-1);
        }

        // Store file information in system properties
        this.properties.setFileName(JenningsUtil.getFileName(this.audioFile));

        // Set shutdown options for the GPIO pins
        // This forces the pins to always turn off at the end of the light show
        GPIOController.setShutdownOptions(false);

        // Analyze the given audio file
        analyze();

        // Update configuration file to match current system configuration
        updateConfig();

        // Execute the Generic Light Show
        execute();

        // Shutdown GPIO Controller
        shutdown();

        // Print copyright information
        copyright();
    }

    /**
     * Explains the program and project/system.
     */
    private void explain() {
        System.out.println("Welcome to the Generic Light Show!");
        System.out.println("\nThis project is the work of Steven Jennings, Tuan Ton, and Dereje Abebe.");
        System.out.println("This system analyzes a selected audio file, and procedurally generates" +
                "\na light show based on the contents of the selected music file!");
        System.out.println("Only .wav files have been tested.");
        System.out.println("We hope you enjoy it!");
        System.out.println("\n------------------------------------------------------------------\n");
    }

    /**
     * Prints the copyright information of this project/system.
     */
    private void copyright() {
        System.out.println(
                "\n   Copyright 2017 Steven Jennings\n" +
                        "\n" +
                        "   Licensed under the Apache License, Version 2.0 (the \"License\");\n" +
                        "   you may not use this file except in compliance with the License.\n" +
                        "   You may obtain a copy of the License at\n" +
                        "\n" +
                        "       http://www.apache.org/licenses/LICENSE-2.0\n" +
                        "\n" +
                        "   Unless required by applicable law or agreed to in writing, software\n" +
                        "   distributed under the License is distributed on an \"AS IS\" BASIS,\n" +
                        "   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.\n" +
                        "   See the License for the specific language governing permissions and\n" +
                        "   limitations under the License.\n");
    }

    /**
     * Performs a pre-play analysis on the selected music file. This is mainly
     * to determine the FFT thresholds for each channel before playback.
     * The music file needs to be selected first or an <code>Exception</code>
     * will be thrown.
     *
     * @see NullPointerException
     */
    private void analyze() {
        // Announce analysis begin
        JenningsUtil.println("Analyzing file...");

        // Execute analysis
        AnalysisHandler.analyze(this.properties, this.audioFile);

        // Announce analysis end
        JenningsUtil.println("File analysis complete.");
    }

    /**
     * Executes the light show. Before this method is called, the music file
     * needs to be selected and analyzed. Otherwise, an <code>Exception</code>
     * will be thrown.
     *
     * @see NullPointerException
     */
    private void execute() {
        // Initialize the GPIO Handler
        GPIOController.initialize(this.properties);

        // Display basic information about the system before execution
        JenningsUtil.println(); // Blank line
        JenningsUtil.println("Executing Generic Light Show.");
        JenningsUtil.println("System Mode: " + (this.properties.getMode() == SystemMode.automatic ? "Automatic" : (this.properties.getMode() == SystemMode.manual ? "Manual" : "Custom Sequencing")));
        if (this.properties.getMode() == SystemMode.automatic || this.properties.getMode() == SystemMode.manual) {
            JenningsUtil.println("Channel Thresholds:");
            double[] thresholds = this.properties.getThresholds();
            for (int i = 0; i < 8; i++) {
                JenningsUtil.println(i + ": " + thresholds[i]);
            }
            JenningsUtil.println("Threshold Sensitivity: " + this.properties.getThresholdSensitivity());
            JenningsUtil.println("Threshold Trigger Count: " + this.properties.getTriggerCount());
            if (this.properties.getMode() == SystemMode.automatic) {
                JenningsUtil.println("Threshold Smoothing: " + this.properties.getThresholdSmoothness());
            }
            JenningsUtil.println("Channel Update Frequency: " + this.properties.getUpdateFrequency());
        }
        JenningsUtil.println("Audio Buffer Size: " + this.properties.getBufferSize());

        // For custom sequencing mode, initialize Sequence Controller. Also select sequencer file
        if (this.properties.getMode() == SystemMode.custom) {
            SequenceController.initialize();
            SequenceController.interpretCustomFile();
        }

        // Create separate thread to execute system
        Thread t1 = new Thread(() -> {
            // Execute Generic Light Show
            AudioHandler.initiateLightShow(this.properties, this.audioFile);
        });

        // Execute thread
        t1.start();

        // Wait for thread to finish
        while (t1.isAlive()) {
            try {
                Thread.sleep(100);
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }

        // Announce end of Generic Light Show execution
        JenningsUtil.println("Audio playback and Generic Light Show finished.");
    }

    /**
     * Passes along the signal to shutdown the GPIO Controller.
     */
    private void shutdown() {
        // Shutdown GPIO Controller
        GPIOController.shutdown();
    }

    // SETTERS AND GETTERS *****************************************************

    /**
     * Sets the music file.
     *
     * @param file New music file.
     */
    public void setAudioFile(File file) {
        this.audioFile = file;
    }

    /**
     * Gets the selected music file.
     *
     * @return The selected music file. It is null if no file has been selected.
     */
    public File getAudioFile() {
        return this.audioFile;
    }
}