package providedCode;// ExceptionDonutsLong - Caclulate # donuts per glass of milk - long version
// Adapted from file ExceptD.java in the book "Java: An Introduction 
// to Computer Science & Programming" by Walter Savitch 

import java.io.*;          // To catch I/O Exceptions
import java.util.Scanner; 
import java.text.DecimalFormat;

public class ExceptionDonutsLong
{
  // ***************************************************************

  public static void main (String[] args) throws Exception
  {  
     int donutCount, milkGlasses;  // # donuts and glasses of milk
     double donutsPerGlass;        // = donutCount / milkGlasses
     String inputString;           // Get user input
     DecimalFormat fmt = new DecimalFormat("0.0");
     
     try
     { 
       System.out.print("Enter number of donuts: ");
       donutCount = readLineInt("donuts");
       if (donutCount < 0) 
       { 
          System.out.println ("Number of donuts must be 0 or more");
          System.out.println ("Ending the program.");
          System.exit(0);
       }
                 
       System.out.print("Enter number of glasses of milk: ");
       milkGlasses = readLineInt("glasses of milk");
       if (milkGlasses < 1)
       {
          throw new Exception("Exception: there's no milk!");
       }
         
       donutsPerGlass = donutCount/(double) milkGlasses;
       System.out.println(
           donutCount + " donuts with " + 
           milkGlasses + " glasses of milk gives you\n" +
           fmt.format(donutsPerGlass) + " donuts for each glass of milk.");                     
     } // End try
     catch(Exception e)
     {  
         System.out.println(e.getMessage());
         System.out.println("Go get some milk. This program is DONE!");
         System.exit(0);
     }
  } // End main 
  
  // ***************************************************************

  // readLineInt - read a line and convert it to an integer
  
  public static int readLineInt(String prompt) throws Exception
  { 
     String inputString = ""; // Input from the user
     int number = -9999;      // Assign a value to avoid a compiler warning
     boolean done = false;    // Set to true when a valid number is inputted
     
     while (!done)
     { 
       try
       { 
         inputString = readLine();
         inputString = inputString.trim();  //removes blanks
         number = Integer.parseInt(inputString);
         done = true;
       }
       catch (NumberFormatException e)
       {  
         System.out.print(
               "Your input is not correct.\n" +
               "It must be a whole number written like 42.  Please try again.\n" +
               "Enter a whole number of " + prompt + ": ");
       }         
      }  // End while (!done)
      return number;
  } // End readLineInt
  
  // ***************************************************************

  // readLine - read characters until an end of line character is reached
  
  public static String readLine() throws Exception
  {  
     final char EOL = '\n';   // End of Line character
     char nextChar;           // Next character read
     String result = "";      // Build the resulting string one char at a time
     boolean done = false;    // Set to true when end is reached
     
     while (!done)
     {  
        nextChar = readChar();
        if (nextChar == EOL) done = true;
        else                 result = result + nextChar;  
     }
     return result;
  } // End readLine
  
  // ***************************************************************

  // readChar - read a character from the keyboard
  
  public static char readChar() throws Exception
  {  
     int charAsInt = -1;  // Assign a value to avoid a compiler warning
     
     try
     {  
        charAsInt = System.in.read();
     }
     catch(IOException e)
     {  
        System.out.println(e.getMessage() + "\nEnding Program.");
        System.exit(0);
     }
     return (char) charAsInt;
  } // End readChar
} // End class
