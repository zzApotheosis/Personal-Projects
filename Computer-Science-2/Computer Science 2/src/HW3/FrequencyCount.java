package HW3;

import java.io.IOException;

/******************************************************************************
 *  Compilation:  javac FrequencyCount.java
 *  Execution:    java FrequencyCount < file.txt
 *  Dependencies: StdIn.java Counter.java
 *  Data files:   http://www.cs.princeton.edu/introcs/43sort/mobydicks.txt
 *                http://www.cs.princeton.edu/introcs/43sort/Leipzig1M.txt
 *                http://www.cs.princeton.edu/introcs/43sort/Leipzig100K.txt
 *                http://www.cs.princeton.edu/introcs/43sort/Leipzig200KM.txt
 *
 *  Read in a sequence of words, separated by whitespace.
 *  Compute the number of times each word appears, and print,
 *  sorted by frequency.
 *
 *  % java FrequencyCount < mobydick.txt
 *  the: 13527
 *  of: 6374
 *  and: 5857
 *  a: 4457
 *  to: 4388
 *  in: 3773
 *  that: 2666
 *  his: 2404
 *  I: 1742
 *  with: 1615
 *  ...
 *
 ******************************************************************************/

public class FrequencyCount {

    public static void main(String[] args) throws IOException {

        // read in the words as an array
        String s = StdIn.readAll();
        String[] words = s.split("\\s+");

        // sort the words
        Merge.sort(words, 0, words.length - 1);

        // tabulate frequencies of each word
        Counter[] zipf = new Counter[words.length];
        int m = 0;                                        // number of distinct words
        for (int i = 0; i < words.length; i++) {
            if (i == 0 || !words[i].equals(words[i-1]))   // short-circuiting OR
                zipf[m++] = new Counter(words[i], words.length);
            zipf[m-1].increment();
        }

        // sort by frequency and print
        Merge.sort(zipf, 0, m - 1);                           // sorting a subarray
        for (int j = m-1; j >= 0; j--) {
            StdOut.println(zipf[j]);
        }
    }
}