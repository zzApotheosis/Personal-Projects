package providedCode;// Interactive Average Program
// This program asks the user to input two real numbers,
// calculates the average of these numbers, and
// prints the results
// your name
// Program #1, CS 1050, MW or TR as appropriate
// Type the version of Java used and platform used (computer and operating system)

import java.util.*;

public class InteractiveAverage
{
  static Scanner console = new Scanner(System.in);

  public static void main (String [] args) throws Exception
  {
  	 double num1, num2;   // input values
    double average;      // average of input values
	
    // Explain the program to the user
    System.out.println("This program averages two real numbers.");
	   
    // Input the two numbers 
    System.out.print("Input your first number: "); 
    num1 = console.nextDouble();
	  
    System.out.print("Input your second number: ");
    num2 = console.nextDouble();
	  
    // Determine the average of the two numbers
    average = (num1 + num2)/2;
	  
    // Output the results
    System.out.print("The average of " + num1);
    System.out.println(" and " + num2 + " is " + average);
    System.out.println("your name");
  }
}
