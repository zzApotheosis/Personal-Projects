package providedCode;

/** ********************************************************************
    CoinFlipRace.java
    Demonstrates the existence of separate data space in multiple
    instantiations of a programmer-defined class.

    Adapted from
    FlipRace.java       Author: Lewis and Loftus
                        Java Software Solutions, 3rd ed
   ********************************************************************
*/

public class CoinFlipRace
{
   //-----------------------------------------------------------------
   //  Flips two coins until one of them comes up heads a set number
   //  of times in a row.
   //-----------------------------------------------------------------
   public static void main (String[] args)
   {
      final int GOAL = 3;           // Number of heads to count
      int count1 = 0, count2 = 0;   // Counters for Coin1 and Coin2
      int tosses = 0;               // Total number of pairs of tosses
      String phrase;                // # of tosses as a string

      // Create two separate coin objects
      Coin coin1 = new Coin();
      Coin coin2 = new Coin();

      while (count1 < GOAL && count2 < GOAL)
      {
         tosses++;
         coin1.flip();
         coin2.flip();

         // Print the flip results. This uses Coin's toString method
         // that is invoked when converting the objects to strings
         System.out.println("Toss " + tosses + "\t" +
                            "Coin 1: " + coin1 + "   " +
                            "Coin 2: " + coin2);

         // If the coin came up heads, increment the respective counter;
         // otherwise, reset the respective counter
         count1 = (coin1.getFace() == coin1.HEADS) ? count1+1 : 0;
         count2 = (coin2.getFace() == coin2.HEADS) ? count2+1 : 0;
      }

      // Determine the winner
      phrase = " in " + tosses + " tosses!";
      if (count1 < GOAL)
         System.out.println ("Coin 2 wins" + phrase);
      else
         if (count2 < GOAL)
            System.out.println ("Coin 1 wins" + phrase);
         else
            System.out.println ("It's a TIE" + phrase);
   }
}
