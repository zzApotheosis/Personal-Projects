package providedCode; /**
    File: Main_04_Template.java - a starting point for Assignment 4

	 Read a file of numbers, calculate sums and averages
	 
	 Input	File Main_04_Input.txt has numbers of type double for gross pay,
            savings rate and IRA investment rate, one set per line
	 
	 Process	Read the data values, calculate savings and IRA investment amounts,
            and sum them
	 
	 Output	A file with the original data values, one line for each input line as
            shown in the assignment specification. After the detail lines are
            displayed, display the summary information as listed in the spec.
               
            The output file is named Main_04_Output.txt
            
    Note 	Without your added code, the program will display the number
		      of numbers in the input file.
*/

import java.util.Scanner;  // Access the Scanner class
import java.io.*;          // Access PrintWriter and related classes

//### Rename your class and file name to Main_04
public class Main_04_Template {

   public static void main(String[] args) throws IOException {
   
      // Declare variables
   	// Define your file names on the next two lines as needed.
   
      final String INPUT_FILE  = "Main_04_Input.txt";
      final String OUTPUT_FILE = "Main_04_Output.txt";
      
      int numInputLines = 0;     // Number of lines in the input file
      int numValidLines = 0;     // Number of valid lines in the input file
      double grossPay = 0.0;     // Input file's gross pay
      double savingsRate = 0.0;  // Input file's savings rate
      double iraRate = 0.0;      // Input file's IRA investment rate
      double sumGrossPay = 0.0;  // Sum of all valid gross pay amounts
      double sumSavings = 0.0;   // Sum of all valid savings amounts
      double sumIra = 0.0;       // Sum of all valid IRA investment amounts
      String line = "";          // Output one line to two or more output areas
   //### Add variables to calculate the averages
   	
   	// Access the input/output files
   
      File inputDataFile = new File(INPUT_FILE);
      Scanner inputFile  = new Scanner(inputDataFile);
      
      FileWriter outputDataFile = new FileWriter(OUTPUT_FILE);
      PrintWriter outputFile = new PrintWriter(outputDataFile);
      
      // *****   Begin program execution   *****
      
   	// Read the input file and sum the numbers. 
   	
      while (inputFile.hasNext()) {
         numInputLines++;
         grossPay = inputFile.nextDouble();
         savingsRate = inputFile.nextDouble();
         iraRate = inputFile.nextDouble();
      		//### Add code here to:
            // 1. Determine whether the input data is valid
            // 2. If so:
            //    (a) Calculate savings and IRA investment amounts
            //    (b) Add those amounts and gross pay to running totals
      		//    (c) Write the spec'd information to the output file
      		//    (d) Write the same information to the console
      		//		   (use System.out.println). This is called "Echoing the input"
            // 3. If not:
            //    (a) Write just the three input values in the correct columns
            //    (b) Write the same information to the console
      } // End while
   
      /* 
         //### Here, the while loop has ended, meaning we've read the entire file
   	   Add code here to output in the format shown in the specification:
   	   1. The total number of input lines read (included in this template)
         2. The total number of valid input lines
         3. The sums of the gross pay, savings amount and IRA amount for valid
            lines only
         4. The averages of the gross pay, savings amount and IRA amount for
            valid lines only
   	   5. Close the input file
   	   6. Close the output file
      */
   
      line =  "The number of input lines is " + numInputLines
            + "\n\nNote: Remove these print statements and replace them"
            + "\nwith the required otuput statements.";
      outputFile.println(line);
      System.out.println(line);
   
      outputFile.close();
      System.exit(0);	
   } // End main
} // End class