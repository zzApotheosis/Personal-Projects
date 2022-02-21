/*
 * Created by Steven Jennings (zzApotheosis) on 02 November 2016.
 */

package HW4;

class BST {
    private Node root;

    public boolean isEmpty() {
        return root == null;
    }

    public void clear() {
        root = null;
    }

    public void insert(int dma, String region, String state) {
        root = insert(dma, region, state, root);
    }

    private int height(Node t) {
        return t == null ? -1 : t.height;
    }

    private int max(int lhs, int rhs) {
        return lhs > rhs ? lhs : rhs;
    }

    public int insertCounter;
    public int balanceCounter;

    public void resetInsertCounter() {
        insertCounter = 0;
    }

    public void resetBalanceCounter() {
        balanceCounter = 0;
    }

    private Node insert(int x, String r, String s, Node t) {
        insertCounter++;
        if (t == null) {
            t = new Node(x, r, s);
        } else {
            if (r.compareTo(t.region) < 0) {
                balanceCounter++;
                t.left = insert(x, r, s, t.left);
                if (height(t.left) - height(t.right) == 2) {
                    if (r.compareTo(t.region) < 0) {
                        t = rotateWithLeftChild(t);
                    } else {
                        t = doubleWithLeftChild(t);
                    }
                }
            } else if (r.compareTo(t.region) > 0) {
                balanceCounter++;
                t.right = insert(x, r, s, t.right);
                if (height(t.right) - height(t.left) == 2) {
                    if (r.compareTo(t.region) > 0) {
                        t = rotateWithRightChild(t);
                    } else {
                        t = doubleWithRightChild(t);
                    }
                }
            } else
                ;  // Duplicate; do nothing
        }

        t.height = max(height(t.left), height(t.right)) + 1;
        return t;
    }

    private Node rotateWithLeftChild(Node k2) {
        Node k1 = k2.left;
        k2.left = k1.right;
        k1.right = k2;
        k2.height = max(height(k2.left), height(k2.right)) + 1;
        k1.height = max(height(k1.left), k2.height) + 1;
        return k1;
    }

    private Node rotateWithRightChild(Node k1) {
        Node k2 = k1.right;
        k1.right = k2.left;
        k2.left = k1;
        k1.height = max(height(k1.left), height(k1.right)) + 1;
        k2.height = max(height(k2.right), k1.height) + 1;
        return k2;
    }

    private Node doubleWithLeftChild(Node k3) {
        k3.left = rotateWithRightChild(k3.left);
        return rotateWithLeftChild(k3);
    }

    private Node doubleWithRightChild(Node k1) {
        k1.right = rotateWithLeftChild(k1.right);
        return rotateWithRightChild(k1);
    }

    public int countNodes() {
        return countNodes(root);
    }

    private int countNodes(Node r) {
        if (r == null) {
            return 0;
        }
        else {
            int l = 1;
            l += countNodes(r.left);
            l += countNodes(r.right);
            return l;
        }
    }

    /* Functions to search for an element */
    public boolean search(int val) {
        return search(root, val);
    }

    private boolean search(Node r, int val) {
        boolean found = false;
        while ((r != null) && !found) {
            int rval = r.dma;
            if (val < rval) {
                r = r.left;
            }
            else if (val > rval) {
                r = r.right;
            }
            else {
                found = true;
                break;
            }
            found = search(r, val);
        }
        return found;
    }

    /* Function for inorder traversal */
    public void inorder() {
        inorder(root);
    }

    private void inorder(Node r) {
        if (r != null) {
            inorder(r.left);
            System.out.print(r.dma + " ");
            inorder(r.right);
        }
    }

    /* Function for preorder traversal */
    public void preorder() {
        preorder(root);
    }

    private void preorder(Node r) {
        if (r != null) {
            System.out.print(r.region + " ");
            preorder(r.left);
            preorder(r.right);
        }
    }

    /* Function for postorder traversal */
    public void postorder() {
        postorder(root);
    }

    private void postorder(Node r) {
        if (r != null) {
            postorder(r.left);
            postorder(r.right);
            System.out.print(r.dma + " ");
        }
    }
}