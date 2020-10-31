/*
 * Created by Steven Jennings (zzApotheosis) on 27 September 2016.
 *
 * The majority of this code was found online at URL:
 * http://www.programcreek.com/2012/11/leetcode-solution-merge-sort-linkedlist-in-java/
 *
 * I only made minor changes. I moved the main method to the top because that's where I prefer to put it.
 * I made the Node its own java file.
 *
 * I'll come back to this and have it merge sort the dma.txt file instead of random integers.
 */

package HW2;

public class LinkedListSort {

    public static void main(String[] args) {
        Node n1 = new Node(2);
        Node n2 = new Node(3);
        Node n3 = new Node(4);
        Node n4 = new Node(3);
        Node n5 = new Node(4);
        Node n6 = new Node(5);

        n1.next = n2;
        n2.next = n3;
        n3.next = n4;
        n4.next = n5;
        n5.next = n6;

        printList(n1);

        n1 = mergeSortList(n1);

        printList(n1);
    }

    // merge sort
    public static Node mergeSortList(Node head) {

        if (head == null || head.next == null) {
            return head;
        }

        // count total number of elements
        int count = 0;
        Node p = head;
        while (p != null) {
            count++;
            p = p.next;
        }

        // break up to two list
        int middle = count / 2;

        Node l = head, r = null;
        Node p2 = head;
        int countHalf = 0;
        while (p2 != null) {
            countHalf++;
            Node next = p2.next;

            if (countHalf == middle) {
                p2.next = null;
                r = next;
            }
            p2 = next;
        }

        // now we have two parts l and r, recursively sort them
        Node h1 = mergeSortList(l);
        Node h2 = mergeSortList(r);

        // merge together
        Node merged = merge(h1, h2);

        return merged;
    }

    public static Node merge(Node l, Node r) {
        Node p1 = l;
        Node p2 = r;

        Node fakeHead = new Node(100);
        Node pNew = fakeHead;

        while (p1 != null || p2 != null) {

            if (p1 == null) {
                pNew.next = new Node(p2.data);
                p2 = p2.next;
                pNew = pNew.next;
            } else if (p2 == null) {
                pNew.next = new Node(p1.data);
                p1 = p1.next;
                pNew = pNew.next;
            } else {
                if (p1.data < p2.data) {
                    // if(fakeHead)
                    pNew.next = new Node(p1.data);
                    p1 = p1.next;
                    pNew = pNew.next;
                } else if (p1.data == p2.data) {
                    pNew.next = new Node(p1.data);
                    pNew.next.next = new Node(p1.data);
                    pNew = pNew.next.next;
                    p1 = p1.next;
                    p2 = p2.next;

                } else {
                    pNew.next = new Node(p2.data);
                    p2 = p2.next;
                    pNew = pNew.next;
                }
            }
        }

        // printList(fakeHead.next);
        return fakeHead.next;
    }


    public static void printList(Node x) {
        if (x != null) {
            System.out.print(x.data + " ");
            while (x.next != null) {
                System.out.print(x.next.data + " ");
                x = x.next;
            }
            System.out.println();
        }

    }
}