/*
 * Created by Steven Jennings (zzApotheosis) on 02 November 2016.
 */

package HW4;

class Node {
    Node left, right;
    int dma;
    String region;
    String state;
    int height;

    Node() {
        this.left = null;
        this.right = null;
        this.dma = 0;
        this.region = "";
        this.state = "";
    }

    Node(int x, String r, String s) {
        this.left = null;
        this.right = null;
        this.dma = x;
        this.region = r;
        this.state = s;
    }
}