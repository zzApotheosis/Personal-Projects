/*
 * Created by Steven Jennings on 21 June 2019.
 */

import zzapo.JenningsUtil;

import javax.crypto.KeyGenerator;
import javax.crypto.SecretKey;
import java.awt.*;
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.StringSelection;
import java.security.NoSuchAlgorithmException;
import java.util.Base64;
import java.util.Random;
import java.util.Scanner;

/**
 * @author Steven Jennings
 * @version 0.0.0
 */
class DataHandler {
    private String input;
    private String offset;
    private int seqLen;
    private int shift;
    private char[] charset;
    private String output;

    public DataHandler() {
        this.input = "";
        this.offset = "";
        this.seqLen = 0;
        this.shift = 0;
        this.charset = null;
        this.output = "";
    }

    public void run(Type type) {
        switch (type) {
            case STANDARD:
                info();
                fetchStandardInput();
                standardProcess();
                break;
            case EXTRA_SECURE:
                extraSecureProcess();
                break;
            case ERROR:
                JenningsUtil.println("ERROR: Exiting program. Check your arguments.");
                System.exit(-1);
        }
        display();
        copyToClipboard();
        enterToContinue();
    }

    private void info() {
        JenningsUtil.println("This program generates a relatively secure set" +
                " of pseudo-random characters for use in cryptographic" +
                " applications.");
        JenningsUtil.println();
        JenningsUtil.println("WARNING: The level of security and cryptography" +
                " is not as powerful as it could be. This program is intended" +
                " to be used to scramble data in such a way that can be" +
                " replicated later.");
        JenningsUtil.println();
        JenningsUtil.println("Use at your own risk.");
        JenningsUtil.println();
    }

    // TODO: Verify input data integrity. Bastards better not throw me summa dem InputMismatchExceptions and shit.
    private void fetchStandardInput() {
        Scanner scanner = new Scanner(System.in);
        JenningsUtil.print("Enter seed: ");
        this.input = scanner.nextLine().trim();
        JenningsUtil.print("Offset: ");
        this.offset = scanner.nextLine().trim();
        JenningsUtil.print("Sequence length: ");
        this.seqLen = scanner.nextInt();
        JenningsUtil.print("Shift: ");
        this.shift = scanner.nextInt();
        JenningsUtil.println();
        JenningsUtil.println("Enter character set as a 4-bit binary number, " +
                "where each bit represents a boolean value for LOWERCASE " +
                "LETTERS, UPPERCASE LETTERS, NUMBERS, and SYMBOLS in that" +
                " order. A value of 0000 will exit the program.");
        JenningsUtil.println();
        JenningsUtil.print("Enter desired character set: ");
        this.charset = JenningsUtil.selectCharSet(scanner.next());
    }

    private void standardProcess() {
        // Create StringBuilder object
        StringBuilder stringBuilder = new StringBuilder();
        Random random = new Random();

        // Set seed
        random.setSeed(JenningsUtil.cryptoStringToLong(this.input + this.offset));

        // Shift results
        for (int i = 0; i < this.shift; i++) {
            random.nextInt();
        }

        // Append data
        for (int i = 0; i < this.seqLen; i++) {
            stringBuilder.append(this.charset[random.nextInt(this.charset.length)]);
        }

        // Set output
        this.output = stringBuilder.toString();
    }

    private void display() {
        // Display results
        JenningsUtil.println();
        JenningsUtil.println("Output: " + this.output);
        JenningsUtil.println();
    }

    private void fetchExtraSecureInput() {
        Scanner scanner = new Scanner(System.in);
        JenningsUtil.print("Sequence length: ");
        this.seqLen = scanner.nextInt();
        JenningsUtil.print("Shift: ");
        this.shift = scanner.nextInt();
        JenningsUtil.println();
        JenningsUtil.println("Enter character set as a 4-bit binary number, " +
                "where each bit represents a boolean value for LOWERCASE " +
                "LETTERS, UPPERCASE LETTERS, NUMBERS, and SYMBOLS in that" +
                " order. A value of 0000 will exit the program.");
        JenningsUtil.println();
        JenningsUtil.print("Enter desired character set: ");
        this.charset = JenningsUtil.selectCharSet(scanner.next());
    }

    private void extraSecureProcess() {
        try {
            KeyGenerator keyGen = KeyGenerator.getInstance("AES");
            keyGen.init(256);
            SecretKey secretKey = keyGen.generateKey();
            this.output = Base64.getEncoder().encodeToString(secretKey.getEncoded());
        } catch (NoSuchAlgorithmException e) {
            e.printStackTrace();
        }
    }

    private void copyToClipboard() {
        Clipboard clipboard = Toolkit.getDefaultToolkit().getSystemClipboard();
        StringSelection data = new StringSelection(this.output);
        clipboard.setContents(data, data);
        JenningsUtil.println("The output has been copied to the clipboard.");
        JenningsUtil.println();
    }

    private void enterToContinue() {
        JenningsUtil.println("Press enter to exit the program.");
        boolean cont;
        Scanner scanner;
        do {
            scanner = new Scanner(System.in);
            cont = !scanner.nextLine().equals("");
        } while (cont);
    }

    public void setInput(String in) {
        this.input = in;
    }

    public void setOutput(String in) {
        this.output = in;
    }

    public String getInput() {
        return this.input;
    }

    public String getOutput() {
        return this.output;
    }
}
