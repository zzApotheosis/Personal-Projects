/*
 * Created by Steven Jennings on 13 October 2017.
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
 */

package testing;

import com.pi4j.io.gpio.*;

/**
 * @author Steven Jennings
 * @version 0.0.0
 */
public class TestGPIO {

    /**
     * The main method. Used to test the 8 channels of the SainSmart
     * Solid State Relay.
     *
     * @param args Unused.
     * @throws InterruptedException For any unexpected problems, which
     *                              probably won't happen.
     */
    public static void main(String[] args) throws InterruptedException {
        // Create GPIO Controller
        final GpioController gpioController = GpioFactory.getInstance();

        // Provision GPIO pins as output pins
        final GpioPinDigitalOutput[] pin = new GpioPinDigitalOutput[]{
                gpioController.provisionDigitalOutputPin(RaspiPin.GPIO_00, "Channel 0", PinState.LOW),
                gpioController.provisionDigitalOutputPin(RaspiPin.GPIO_01, "Channel 1", PinState.LOW),
                gpioController.provisionDigitalOutputPin(RaspiPin.GPIO_02, "Channel 2", PinState.LOW),
                gpioController.provisionDigitalOutputPin(RaspiPin.GPIO_03, "Channel 3", PinState.LOW),
                gpioController.provisionDigitalOutputPin(RaspiPin.GPIO_04, "Channel 4", PinState.LOW),
                gpioController.provisionDigitalOutputPin(RaspiPin.GPIO_05, "Channel 5", PinState.LOW),
                gpioController.provisionDigitalOutputPin(RaspiPin.GPIO_06, "Channel 6", PinState.LOW),
                gpioController.provisionDigitalOutputPin(RaspiPin.GPIO_07, "Channel 7", PinState.LOW)
        };

        // Set shutdown states (executed when gpioController.shutdown() is called)
        for (GpioPinDigitalOutput singlePin : pin) {
            singlePin.setShutdownOptions(true, PinState.LOW);
        }

        // Execute test
        System.out.println("Executing GPIO test!\n");
        for (int i = 0; i < 200; i++) {
            for (GpioPinDigitalOutput singlePin : pin) {
                System.out.println("Pin: " + singlePin.getPin());
                singlePin.toggle();
                Thread.sleep(200);
            }
        }

        // Shutdown GPIO Controller
        gpioController.shutdown();
    }

}