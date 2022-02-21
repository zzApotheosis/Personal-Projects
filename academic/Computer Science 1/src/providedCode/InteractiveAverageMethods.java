package providedCode; /**
   Interactive Average Program with Methods
   Accompanies Programming Assignment #5
   
   Using methods, this program asks the user to input two real numbers, 
   calculates the average of these numbers, and prints the results
   
   Methods used:
   
    explain() - explain the program to the user
    getNum() -  get numbers from the user
    calcAvg() - calculate the average of the numbers
    outputResults()- output the numbers and their average
    
    Version of your Integrated Development Environment (IDE), computer and OS
    New vocabulary word and its meaning
    Inspirational quote � not religious or political � along with the source and
    the person�s year of birth [and death] written as, for example, (1912 � 1987)
    or, if the person is still alive, (b. 1949)
*/

import java.util.Scanner;  // For console input

public class InteractiveAverageMethods { 

   // Note that the Scanner an Toolkit objects are preceeded by 'static' and
   // appear before the main program. This gives them a global scope.

   static Scanner console = new Scanner(System.in);
   static Toolkit tools   = new Toolkit();
   static final int NUMBER_WIDTH = 8;  // # of spaces to display numbers
   
   public static void main (String [] args) { 
      double num1 = 0.0;     // First input value
      double num2 = 0.0;     // Second input value
      double average = 0.0;  // Average of the input values
      
      // Explain the program to the user
      explain();
      
      // Input the numbers 
      num1 = getOneNumber(1);
      num2 = getOneNumber(2);
      
      // Determine the average of the two numbers
      average = calcAvg(num1, num2);
      
      // Output the results
      outputResults(num1, num2, average); 
      
      System.exit(0);
   } // End main
   
   // **************************************************************
  
   // Methods section
   
   // Explain the program to the user
   public static void explain() { 
      System.out.println(
            "This program averages two real numbers " +
            "entered by the user.\n" +
            "The output is the two numbers and their average.\n" +
            "Note: methods are used.");
   } // End explain
   
   // ***************************************************************
   
   // Return the number input by the user
   public static double getOneNumber(int whichNumber) { 
      double num;   // Number input by the user
      System.out.print("Enter input value #" + whichNumber + ": ");
      num = console.nextDouble();
      return num;
   } // end getOneNumber
   
   // ***************************************************************
  
   // Return the average of two numbers, a and b
   public static double calcAvg(double a, double b) { 
      return (a + b) / 2.0;
   } // End calcAvg
   
   // ***************************************************************
   
   // Output the values of numbers first and second and their average
   public static void outputResults(double first, 
                                     double second, 
                                     double average) { 
      System.out.print(
         "The average of " + 
         tools.leftPad(first, NUMBER_WIDTH, "0.0") +
         " and " + tools.leftPad(second, NUMBER_WIDTH, "0.0") +
         " is " + tools.leftPad(average, NUMBER_WIDTH, "0.00"));

   } // End outputResults
} // End class