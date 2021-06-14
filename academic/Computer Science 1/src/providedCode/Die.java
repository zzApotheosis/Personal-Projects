package providedCode;

/** ********************************************************************
    Die.java     
    Adapted from Lewis/Loftus, Java Software Solutions, 3rd ed
    Represents one die (singular of dice) with faces showing values
    between 1 and the number of faces on the die.
    ********************************************************************
*/

public class Die
{
   private final boolean INITIALIZE_ROLL = true;   // True : roll to initialize,
                                                   // False: initialize to 1
   private final int DEFAULT_FACES = 6;   // Default # of faces 
   private final int MIN_FACES = 4;       // Minimum # of faces on the die
   private int numFaces;                  // Number of faces (sides) on the die
   private int faceValue;                 // Current value showing on the die
   
   //-----------------------------------------------------------------
   //  Defaults to a DEFAULT_FACES-sided die, initially showing 1.
   //-----------------------------------------------------------------
   public Die()
   {  
      numFaces = DEFAULT_FACES;   
      if (INITIALIZE_ROLL) roll();
      else                 faceValue = 1;  
   }
   
   //-----------------------------------------------------------------
   //  Explicitly sets the size of the die. Defaults to a size of
   //  DEFAULT_FACES if the parameter is invalid.  Initial face is 1. 
   //-----------------------------------------------------------------
   public Die(int faces)
   {  
      // Instead of the if-else below, you can code the single line
      //    faces = Math.max(numFaces,MIN_FACES);
      if (faces < MIN_FACES)  numFaces = DEFAULT_FACES;
          else                numFaces = faces;
      if (INITIALIZE_ROLL) roll();
      else                 faceValue = 1;  
   }
   
   //-----------------------------------------------------------------
   //  Rolls the die and returns the result.
   //  The Math.random returns a double number between 0 and 1, so
   //  multiplying it by 2, converting it to integer and adding 1 
   // gives a number between 1 and the number of faces.
   //-----------------------------------------------------------------
   public int roll()
   {  
      faceValue = (int) (Math.random() * numFaces) + 1;
      return faceValue;  
   }
   
   //-----------------------------------------------------------------
   //  Returns the current die value.
   //-----------------------------------------------------------------
   public int getFaceValue()
   {   
      return faceValue;  
   }
}