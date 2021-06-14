package providedCode;

/** ******************************************************************
    CoinCountFlips.java
    
    Adapted and Class name changed from CountFlips.java to group
    Coin-related classes.     Author: Lewis and Loftus
                                      Java Software Solutions, 3rd ed
    
    Demonstrates:
    1.   The use of a programmer-defined class.
    2.   Timing of counting tails or calculating the # of tails
   
    ********************************************************************
*/

public class CoinCountFlipsWithTiming {  

   static Toolkit tools = new Toolkit();
   static final String NL = "\n";         // New line output indicator
   static final String NUMBER = "#,##0";  // Mask for output  
   static final int    WIDTH  = 12;       // Width of output field
   
   //-----------------------------------------------------------------
   //  Flips a coin multiple times and counts the number of heads
   //  and tails that were flipped.
   //-----------------------------------------------------------------

   public static void main (String[] args) {  
      final double NUM_MILLIONS = 500;    
                                       // Number of millions of times to flip coin
      long numFlips = (long) (NUM_MILLIONS * 1000000);   // # of times to flip
      long heads = 0;                  // Counts number of heads
      long tails = 0;                  // Counts number of tails
      Coin myCoin = new Coin();        // Instantiate the Coin object
      
      String timeStart = "";           // Start time of the loop
      String timeEnd   = "";           // End   time of the loop
      
      // *** Count both heads and tails

      timeStart = tools.getCurrentTime();
      for (int count = 1; count <= numFlips; count++) {  
         myCoin.flip();
         if (myCoin.getFace() == myCoin.HEADS) { heads += 1; }
         else                                  { tails += 1; }
      } // End for
      timeEnd = tools.getCurrentTime();
      
      // Display results
      displayResults("Count heads and tails",
                     numFlips, heads, tails, timeStart, timeEnd);

      // *** Count just heads

      heads = 0;     // Reset counters
      tails = 0;
      timeStart = tools.getCurrentTime();
      for (int count = 1; count <= numFlips; count++) {  
         myCoin.flip();
         if (myCoin.getFace() == myCoin.HEADS) { heads += 1; }
      } // End for
      tails = numFlips - heads;
      timeEnd = tools.getCurrentTime();
      
      // Display results
      displayResults("Count just heads",
                     numFlips, heads, tails, timeStart, timeEnd);
   } // End main
   
   //************************************************************************
   
   public static void displayResults(String whatType,
                                     long numberOfFlips,
                                     long numberOfHeads,
                                     long numberOfTails,
                                     String startTime,
                                     String endTime) {
      
      double percentHeads = 0.0;    // % of tosses that are heads
      double percentTails = 0.0;    // % of tosses that are tails
      
      if (numberOfFlips > 0) {
         percentHeads = (double) numberOfHeads / numberOfFlips;
         percentTails = (double) numberOfTails / numberOfFlips;
      } // End if
      
      System.out.println(NL + NL + whatType + NL +
         "Number flips: "    + tools.rightPad(numberOfFlips, WIDTH, NUMBER) + NL +
         "Number of heads: " + tools.leftPad(numberOfHeads, WIDTH, NUMBER) + 
                   " (" + tools.leftPad(percentHeads,6,"0.000%") + ")" + NL  +
         "Number of tails: " + tools.leftPad(numberOfTails, WIDTH, NUMBER) + 
                   " (" + tools.leftPad(percentTails,6,"0.000%") + ")" + NL  +
         "Start/end times: " + startTime + " / " + endTime);   
    } // End displayResults
} // End class
