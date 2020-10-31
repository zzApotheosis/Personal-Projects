package providedCode;

/** ******************************************************************
    CoinCountFlips.java
    
    Adapted and Class name changed from CountFlips.java to group
    Coin-related classes.     Author: Lewis and Loftus
                                      Java Software Solutions, 3rd ed
    
    Demonstrates the use of a programmer-defined class.
   
!!! Challenge: how can you speed up this program?
!!! Hint: study how and when counting is done. :)

    ********************************************************************
*/

public class CoinCountFlips
{  
   //-----------------------------------------------------------------
   //  Flips a coin multiple times and counts the number of heads
   //  and tails that were flipped.
   //-----------------------------------------------------------------

   public static void main (String[] args)
   {  
      final String NL = "\n";       // New line output indicator
      final int NUM_FLIPS = 1000;   // Number of times to flip coin
      int heads = 0, tails = 0;     // Counts number of tosses
      Coin myCoin = new Coin();     // instantiate the Coin object
      
      // Count heads and tails
      for (int count = 1; count <= NUM_FLIPS; count++)
      {  
         myCoin.flip();
         if (myCoin.getFace() == myCoin.HEADS) heads++;
         else                                  tails++;
      }
     
      // Display results
      System.out.println ("Number flips: " + NUM_FLIPS + NL +
                          "Number of heads: " + heads + NL +
                          "Number of tails: " + tails);   
   }
}
