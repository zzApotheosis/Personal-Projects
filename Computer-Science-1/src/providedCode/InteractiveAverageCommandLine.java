package providedCode;// Interactive Average Program using parameters from the command line
// This program asks the user to input two real numbers,
// calculates the average of these numbers, and
// prints the results. Alternatively, one or both of the numbers can
// come from the command line. If there's only one number, it is assigned 
// to num1. If two numbers, to num1 and num2, respectively
// your name
// Program #1, CS 1050, MW or TR as appropriate
// Type the version of Java used and platform used (computer and operating system)

import java.util.*;

public class InteractiveAverageCommandLine {
  static Scanner console = new Scanner(System.in);

  public static void main (String [] args) {
  	 double num1, num2;     // input values
    double average;        // average of input values
	 int len = args.length; // # of numbers in args (ignore 3 or more elements)

    // Explain the program to the user
    System.out.println("This program averages two real numbers.");
	   
    // Initialize num1 and num2 based on the # of parameters on the command line
    
    if (len >= 1) {
      num1 = Double.parseDouble(args[0]);
    } else {
       // Input the two numbers 
       System.out.print("Input your first number: "); 
       num1 = console.nextDouble();
    } // End if
	  
    if (len >= 2) {
      num2 = Double.parseDouble(args[1]);
    } else {
       if (len >= 1) {
         System.out.println("Your first number is " + num1 + "\n");
    } // End if
       System.out.print("Input your second number: ");
       num2 = console.nextDouble();
    } // End if
    	  
    // Determine the average of the two numbers
    average = (num1 + num2)/2;
	  
    // Output the results
    System.out.print("The average of " + num1);
    System.out.println(" and " + num2 + " is " + average);
    System.out.println("David Kramer");
  }
}
