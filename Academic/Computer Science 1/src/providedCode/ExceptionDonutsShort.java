package providedCode;// ExceptionDonutsShort - Caclulate # donuts per glass of milk - Short version
// Adapted from file ExceptD.java in the book "Java: An Introduction 
// to Computer Science & Programming" by Walter Savitch 

import java.io.*;          // To catch I/O Exceptions
import java.util.Scanner; 
import java.text.DecimalFormat;

public class ExceptionDonutsShort
{
  // ***************************************************************

  public static void main (String[] args) throws Exception
  {  
     int donutCount = 0;      // # of donuts input from the user
     int milkGlasses = 0;     // # of glasses of milk input from the user
     double donutsPerGlass;   // = donutCount / milkGlasses
     String inputString;      // To get user input
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

       donutsPerGlass = (double) donutCount / milkGlasses;
       System.out.println(
           donutCount + " donuts with " + 
           milkGlasses + " glasses of milk gives you\n" +
           fmt.format(donutsPerGlass) + " donuts for each glass of milk.");                     
     } // End try
     catch(Exception e)
     {  
         System.out.println(e.getMessage());
         if (milkGlasses == 0) System.out.println("What? You want me to divide by 0?!!!??!");
         else                  System.out.println("Ain't no such thing as negative glasses.");
 
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
     Scanner keyboard = new Scanner(System.in);
     
     while (!done)
     { 
       try
       { 
         inputString = keyboard.nextLine();
         inputString = inputString.trim();  // Removes blanks at the start and end
         number = Integer.parseInt(inputString);
         done = true;
       }
       catch (NumberFormatException e)
       {  
         System.out.print(
               "Your input, " + inputString + ", is not correct.\n" +
               "It must be a whole number written like 42.  Please try again.\n" +
               "Enter a whole number of " + prompt + ": ");
       }         
      }  // end while (!done)
      return number;
  } // End readLineInt
} // End class
