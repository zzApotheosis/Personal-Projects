package providedCode;// Define the class with the payroll program parameters

import java.util.Scanner;     // To read the parameter file
import java.io.*;             // To acces File operations

public class EmployeeParameters {
   static private String EMPLOYEE_PARAMETERS = "resources/EmployeeParameters.txt";
                                          // Name of the parameter file
   static private String NL = "\r\n";     // New line for println
   static private String PERCENT = "%";   // Phrase to display percent
   public int maxEmployees;		   // Maximum number of employees 
   public double savingsRate;		   // Savings rate, %
   public double iraRate;           // IRA investment rate, %
   public double federalWithholdingRate;  // Federal withholding tax rate, %
   public double stateWithholdingRate;    // State withholding tax rate, %

   //**************************************************************************

   // Default constructor - Set all private variables to 0
   
   public EmployeeParameters() {
      maxEmployees = 0;
      savingsRate  = 0.0;
      iraRate = 0.0;
      federalWithholdingRate = 0.0;
      stateWithholdingRate = 0.0;
   } // End default constructor
   
   //**************************************************************************

   // getEmployeeParameters - read the parameter file and load the employee
   //    parameters from the parameter file. The order of the varaiables is the
   //    same order the numbers appear in the parameter file. See the comments
   //    in the private declarations above for a description of each variable.

   public void getEmployeeParameters() throws IOException {
   
      File parameterFile = new File(EMPLOYEE_PARAMETERS);
      Scanner parameter  = new Scanner(parameterFile);
      
      maxEmployees = parameter.nextInt();
      savingsRate  = parameter.nextDouble();
      iraRate = parameter.nextDouble();
      federalWithholdingRate = parameter.nextDouble();
      stateWithholdingRate = parameter.nextDouble();
   } // End getEmployeeParameters
   
   //**************************************************************************

   // displayEmployeeParameters - display the parameters on the console.

   public void displayEmployeeParameters() {
      System.out.println(
         NL + "Maximum # of employees: " + maxEmployees +
         NL + "Savings rate: " + savingsRate + PERCENT +
         NL + "IRA investment rate: " + iraRate + PERCENT +
         NL + "Federal withholding rate: " + federalWithholdingRate + PERCENT +
         NL + "State withholding rate: "  + stateWithholdingRate + PERCENT);
   } // End displayParameters
   
} // End class
