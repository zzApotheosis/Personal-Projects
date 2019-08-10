package providedCode;

/** ******************************************************************
    DieSnakeEyes.java - Roll dice using the Die class and count
                        the number of snake eyes rolled
                       
    Demonstrates the use of a class with overloaded constructors. 

    Adapted from:
    SnakeEyes.java      Authors: Lewis and Loftus 
                        Java Software Solutions, 3rd ed
                        
    This program was modified to count any one pair of die tosses.
    See constants ROLL1 and ROLL2. 
    To count snake eyes, set   ROLL1 = 1 and ROLL2 = 1
    To count box cars,   set   ROLL1 = 6 and ROLL2 = 6
    
    Note that the variable that counts tosses of (ROLL1,ROLL2) is
    still called 'snakeEyes' per the original program
   ********************************************************************
*/

public class DieSnakeEyes
{  
   //  Create two die objects, then roll both dice a set number of
   //  times, counting the number of (ROLL1,ROLL2) tosses that occur.

   public static void main (String[] args)
   {  
      final String NL = "\n";  // New line indicator for output
      final int ROLLS = 1000;  // # of times to roll the dice
      final int ROLL1 = 1;     // Roll to search for on num1
      final int ROLL2 = 1;     // Roll to search for on num2
      String rollPhrase = "";  // Phrase when ROLL1 == ROLL2
      
      int counter = 0;         // Counts the number of desired roll
      int num1, num2;          // The faces of the two die thrown
      Die die1 = new Die( 6);  // Create one die
      Die die2 = new Die(20);  // Create another die
      
      // Roll the dice the set number of times. If both numbers come
      // up 1, we have a snake eyes, so count it
      for (int rollIt = 1; rollIt <= ROLLS; rollIt++)
      {  
         num1 = die1.roll();
         num2 = die2.roll();
         // Check for the desired roll
         if ( (num1 == ROLL1 && num2 == ROLL2) ||
              (num1 == ROLL2 && num2 == ROLL1) )
            counter++;
      }
      
      // Display the results
      if (ROLL1 != ROLL2) {
         rollPhrase = ") and (" + ROLL2 + "," + ROLL1;
      } // End if
      System.out.println(
            "Number of rolls: " + ROLLS + NL +
            "Number of rolls of (" + ROLL1 + "," + ROLL2 + 
            rollPhrase + "): " + 
            counter + NL + 
            "Percent: " + 100.0 * counter/ROLLS + "%");
   }  
}
