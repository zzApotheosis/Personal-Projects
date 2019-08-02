package providedCode;

/** ******************************************************************
    Coin.java    Author: Lewis/Loftus, Java Software Solutions, 3rd ed
    Represent a coin with two sides that can be flipped.
    *******************************************************************
*/

public class Coin
{
   public final int HEADS = 0;
   public final int TAILS = 1;
   private int face;             // Face showing, 0=heads or 1=tails
   
   //-----------------------------------------------------------------
   //  Default Constructor: set up the coin by flipping it.
   //-----------------------------------------------------------------
   public Coin()
   {  
      flip(); 
   }
   //-----------------------------------------------------------------
   //  Flips the coin by randomly choosing a face.
   //  The Math.random returns a double number between 0 and 1, so
   //  multiplying it by 2 and converting it to integer gives 0 or 1.
   //-----------------------------------------------------------------
   public void flip()
   {  
      face = (int) (Math.random() * 2); 
   }
   //-----------------------------------------------------------------
   //  Returns the current face of the coin as 0 or 1
   //-----------------------------------------------------------------
   public int getFace()
   {  
      return face;  
   }
   //-----------------------------------------------------------------
   //  Returns the current face of the coin as a string.
   //  This uses the toString method that is built in to the class
   //-----------------------------------------------------------------
   public String toString()
   {  
      String faceName;
      if (face == HEADS) { faceName = "Heads"; }
      else               { faceName = "Tails"; }
      return faceName;  
   }
}