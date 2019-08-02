package providedCode; /**
 *  InteractiveAverageClass - acquire and calculate averages
 *  
 *  As of this writing, two numbers are averaged. They are stored in
 *  two private instance variables, num1 and num2
 *  
 *  Methods:
 *     InteractiveAverageClass() - Default constructor initializes 
 *                   num1 and num2 to 0
 *     InteractiveAverageClass(double value1, double value2) - 
 *                   Constructor that initializes num1 to value1, 
 *                   num2 to value2    
 *     getNumbers() - prompts the user to input two numbers into 
 *                   num1 and num2
 *     getOneNumber() - prompts the user to enter one number
 *     calcAvg() - returns the average of num1 and num2
 *     outputNumbers()- outputs (with a message) the values of 
 *                   num1 and num2
 */

import java.util.Scanner;

public class InteractiveAverageClass {  

   private double num1;   // Instance variable, first  number to average 
   private double num2;   // Instance variable, second number to average
   Scanner console = new Scanner(System.in); // Establish the keyboard
   
   // **************************************************************
   
   // Default (no arg) constructor - initialize num1 and num2 to 0
   public InteractiveAverageClass() { 
      num1 = 0; 
      num2 = 0;
   } // End InteractiveAverageClass()
   
   // **************************************************************
   
   // Constructor (2-arg) - initialize num1 to value1 and num2 to value2
   public InteractiveAverageClass(double value1, double value2) { 
     num1 = value1;
     num2 = value2;
   } // End InteractiveAverageClass(double value1, double value2
   
   // **************************************************************
   
   /* Get values of num1 and num2 from the user
    * Note the use of this method calling another method with parameters
    */
   public void getNumbers() { 
     num1 = getOneNumber("first");
     num2 = getOneNumber("second");
   }

   // **************************************************************
   
   // Get a single number
   public double getOneNumber(String which) {
     double oneNumber;
     System.out.print("Enter your " + which + " number: ");
     oneNumber = console.nextDouble();
     return oneNumber;
   } // End getNumbers

   // **************************************************************
   
   // Return the average of the numbers
   public double calcAverage() { 
     return (num1 + num2) / 2.0;
   } // End calcAverage
   
   // **************************************************************
   
   // Output the values of the numbers with a message
   public void outputNumbers() {
     System.out.print("The numbers are " + num1 + " and " + num2);
   } // End outputNumbers
} // End class
