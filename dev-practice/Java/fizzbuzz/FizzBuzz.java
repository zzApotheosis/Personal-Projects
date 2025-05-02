class FizzBuzz {
    public static void main(String[] args) {
        String msg = "";
        for (int i = 1; i <= 100; i++) {
            msg = "";
            if (i % 3 == 0) {
                msg += "fizz";
            }
            if (i % 5 == 0) {
                msg += "buzz";
            }
            if (msg.length() == 0) {
                msg += String.valueOf(i);
            }
            System.out.println(msg);
        }
    }
}
