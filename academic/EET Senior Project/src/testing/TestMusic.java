/*
 * Created by Steven Jennings on 28 October 2017.
 */

package testing;

import org.apache.commons.math3.complex.Complex;

import javax.sound.sampled.*;
import javax.swing.*;
import java.io.File;
import java.io.IOException;

/**
 * @author Steven Jennings
 * @version 0.0.0
 */
public class TestMusic {

    private volatile static int test = 0;

    private TestMusic() {
        // Class is not instantiable
    }

    public synchronized static void main(String[] args) {
        File file = showFileChooser();

        Thread t1 = new Thread(() -> {
            System.out.println("Now playing song!");
            playSong(file);
        });

        t1.start();

        while (t1.isAlive()) {
            try {
                Thread.sleep(100);
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }

        System.out.println("Song finished!");
    }

    private static File showFileChooser() {
        File out = null;
        JFileChooser fc = new JFileChooser();
        fc.setDialogTitle("Choose Music File");
        int returnValue = fc.showOpenDialog(null);
        if (returnValue == JFileChooser.APPROVE_OPTION) {
            out = fc.getSelectedFile();
        } else {
            System.exit(0);
        }
        return out;
    }

    private static void playSong(File songFile) {
        try {
            AudioInputStream in;
            in = AudioSystem.getAudioInputStream(songFile);
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
            rawPlay(decodedFormat, din);
            in.close();
        } catch (Exception e) {
            System.err.println("Song playback error!");
            e.printStackTrace();
        }
    }

    private static void rawPlay(AudioFormat targetFormat, AudioInputStream din) throws IOException, LineUnavailableException {
        byte[] data = new byte[4096];
        SourceDataLine line = getLine(targetFormat);
        if (line != null) {
            // Start playback
            line.start();
            int nBytesRead = 0, nBytesWritten = 0;
            while (nBytesRead != -1) {
                nBytesRead = din.read(data, 0, data.length);
                if (nBytesRead != -1) {
                    nBytesWritten = line.write(data, 0, nBytesRead);
                }
            }

            // Stop playback
            line.drain();
            line.stop();
            line.close();
            din.close();
        }
    }

    private static SourceDataLine getLine(AudioFormat audioFormat) throws LineUnavailableException {
        SourceDataLine res;
        DataLine.Info info = new DataLine.Info(SourceDataLine.class, audioFormat);
        res = (SourceDataLine) AudioSystem.getLine(info);
        res.open(audioFormat);
        return res;
    }

    private static double[] toDouble(byte[] in) {
        double[] out = new double[in.length];

        for (int i = 0; i < in.length; i++) {
            out[i] = in[i];
        }

        return out;
    }

    private static double[] calcMagnitudes(Complex[] in) {
        double[] out = new double[in.length];

        for (int i = 0; i < in.length; i++) {
            out[i] = Math.sqrt(Math.pow(in[i].getReal(), 2.0) + Math.pow(in[i].getImaginary(), 2.0));
        }

        return out;
    }

}