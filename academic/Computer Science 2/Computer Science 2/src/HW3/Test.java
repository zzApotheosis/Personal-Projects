package HW3;

import java.io.*;
import java.util.Scanner;

/**
 * Created on 10/19/16.
 */
public class Test {
//    public static void main(String[] args) throws IOException {
//        String s = readFile("/home/zzapotheosis/Desktop/CS2 HW3/tale2.txt");
//
//        System.out.println(s);
//
//    }

    private static String readFile(String file) throws IOException {
        BufferedReader reader = new BufferedReader(new FileReader(file));
        String line = null;
        StringBuilder stringBuilder = new StringBuilder();
        String ls = System.getProperty("line.separator");

        try {
            while ((line = reader.readLine()) != null) {
                stringBuilder.append(line);
                stringBuilder.append(ls);
            }

            return stringBuilder.toString();
        } finally {
            reader.close();
        }
    }
}
