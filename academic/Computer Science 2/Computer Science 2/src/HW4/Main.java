package HW4;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;
import java.util.StringTokenizer;

/**
 * Created by zzapotheosis on 11/2/16.
 */
public class Main {

    public static void main(String[] args) {

        StopWatch time = new StopWatch();

        String fileName = "C:/Users/zzapo/Desktop/dma.txt";
        File file = new File(fileName);
        Scanner sc = null;
        String dma = "";

        BST tree = new BST();

        try {
            sc = new Scanner(file);
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }

        while (sc.hasNext()) {
            dma += sc.next() + " ";
        }

        StringTokenizer st = new StringTokenizer(dma, ", ");

        tree.resetBalanceCounter();
        tree.resetInsertCounter();
        int temp;
        while (st.hasMoreTokens()) {
            temp = Integer.parseInt(st.nextToken());
            tree.insert(temp, st.nextToken(), st.nextToken());
        }

        System.out.println("Time elapsed: " + time.elapsedTime());

        System.out.println("Number of tree rebalance calls: " + tree.balanceCounter);
        System.out.println("Number of node insertion calls: " + tree.insertCounter);

        System.out.println("Preorder");
        tree.inorder();
    }
}