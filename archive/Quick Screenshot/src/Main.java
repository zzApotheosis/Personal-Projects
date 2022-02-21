/*
 * Created by Steven Jennings on 20 June 2019.
 *
 * Used template code from: https://stackoverflow.com/a/37903591
 */

import javax.imageio.ImageIO;
import java.awt.*;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.Date;

/**
 * @author Steven Jennings
 * @version 0.0.0
 */
public class Main {

    public static void main(String[] args) {
        delay(1000);
        captureAllDisplays();
    }

    private static void delay(int time) {
        try {
            Thread.sleep(time);
        } catch (InterruptedException e) {
            System.err.println(e);
        }
    }

    private static void captureAllDisplays() {
        GraphicsEnvironment graphicsEnvironment = GraphicsEnvironment.getLocalGraphicsEnvironment(); // Get OS's graphics environment
        GraphicsDevice[] graphicsDevices = graphicsEnvironment.getScreenDevices(); // Get all displays used in graphics environment
        String timeStamp = new SimpleDateFormat("yyyy-MM-dd-HH-mm-ss").format(new Date()); // Get current date and time for file name
        Robot robot;
        BufferedImage image;
        String imageFormat = "png";
        String imageDirectory = "." + File.separator + "Quick Screenshot" + File.separator;

        // Check if output directory exists, if not, create it
        if (!new File(imageDirectory).exists()) new File(imageDirectory).mkdir();

        for (GraphicsDevice graphicsDevice : graphicsDevices) {
            // DisplayMode displayMode = graphicsDevice.getDisplayMode();
            Rectangle bounds = graphicsDevice.getDefaultConfiguration().getBounds();
            System.out.println(graphicsDevice.getIDstring().replace("\\", ""));
            System.out.println("Min: (" + bounds.getMinX() + ", " + bounds.getMinY() + ");" +
                    "\nMax: (" + bounds.getMaxX() + ", " + bounds.getMaxY() + ")");
            System.out.println("Dimensions: " + bounds.getWidth() + "x" + bounds.getHeight());
            System.out.println(); // Separate blocks by screen capture

            try {
                robot = new Robot();
                image = robot.createScreenCapture(new Rectangle((int) bounds.getMinX(),
                        (int) bounds.getMinY(), (int) bounds.getWidth(), (int) bounds.getHeight()));
                ImageIO.write(image, imageFormat, new File(imageDirectory + timeStamp + "_" + graphicsDevice.getIDstring().replace("\\", "") + "." + imageFormat));
            } catch (AWTException | IOException e) {
                e.printStackTrace();
            }
        }
    }

}
