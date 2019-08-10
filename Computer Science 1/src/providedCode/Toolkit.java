package providedCode; /** File Toolkit.java - a class with general tools, version 06, 2015-07-21

   This class is a "pure" class and does not have a main program. It requires
   the Employee.java class as there are some sort routines in this Toolkit
   that refer to the Employee class
   
   The constructors are not documented in the Input-Process-Output format,
   but the user methods defined are
   
   Methods available in this toolkit 
         (alphabetically listed and coded below after the Constructors)
   
   getClassInfo   Get the main program's package name (or the empty string),
                  class name and path to the class
                  
                  Note:  getClassInfo uses several private methods, most of 
                  which begin with "getClassInfo". The exception is the Dummy
                  class that is defined at the end of the getClassInfo methods.
                  GetClassInfo is listed in alphabetical order, but the 
                  supporting private methods and the Dummy class appear at the
                  end of this file
   
   getCurrentDate No parameters. Get current date as a string in MM/DD/YYYY
                  format

   getCurrentDate One parameter. Get the current date as a string in the
                  supplied format

   getCurrentDateTime   No parameters. Uses the default calls for current date
                  and time

   getCurrentTime No parameters. Get current time as a string in HH:MM:SS format

   getCurrentTime One parameter. Get the current time as a string in the 
                  supplied format

   giveMeAnS      Returns an "s" as a String if the number supplied is not 1;
                  "" if the number is one. used for pluralization

   giveMeAPlural  Returns a suffix as a String if the number supplied is not 1;
                  "" if the number is one. used for pluralization

   leftPad        Convert a number to a string and pad it on the left
                     Two parameter lists (overloaded)

   outputLine     Output a single line to a screen, file or both

   padNumber      Convert a number to a string and pad it on the left, right
                  or both

   padString      Pad a string on the left, right or both
                     Two parameter lists (overloaded)

   partiallyFillArray   Fill an integer array up to and including its length

   rightPad       Convert a number to a string and pad it on the right
                     Two parameter lists (overloaded)

   roundNumber    Round a number to N places

   selectionSort  Sort an array of integers

   selectionSortWithIndex  Sort an array of integers along with their indexes

   selectionSortStringWithNumbers
                  Sort an array of strings along with some numbers

   
   A template for documenting a method is at the end of this file. When creating
   a new method:
   
   1. Insert the name and description in the alphabetical list above
   2. Insert the method header in the appropriate alphabetical location in the
      definitions of the methods below
   3. Copy the template documentation to the method area and write the method
   
   @Author  Dr. Patricia Tucker (some of the padding methods) and David Kramer
*/
                                    // Import for the...
import java.io.*;                   //    PrintWriter file class
import java.text.DecimalFormat;     //    Padding methods
import java.util.Scanner;           //    partiallyFillArray method
import java.text.SimpleDateFormat;  //    getCurrentTime method
import java.util.Calendar;          //    getCurrentTime method
import java.util.Date;              //    getCurrentDate method

public class Toolkit {

   // Global constants
   
   // These are made public to allow callers to access the names

   // Output mode constants are used in the outputLine method. They
   // determine where the output is to be displayd or written.
   
   final public char OUTPUT_MODE_SCREEN  = 'S';   // Output to screen
   final public char OUTPUT_MODE_FILE    = 'F';   // Output to a file
   final public char OUTPUT_MODE_BOTH    = 'B';   // Output to both
   final public char OUTPUT_MODE_DEFAULT = OUTPUT_MODE_SCREEN;
                                                  // Default output mode
   final public char OUTPUT_MODE_INVALID = '?';   // Invalid output mode
  
   // Global private variables
   
   // Delimeter between output line count and the line and an indicator
   // whether the line count should be included. These constants and variables
   // are used in the outputLine method
   
   final private String OUTPUT_COUNT_DELIMETER = ")  ";  // For printed line #s
   final private char OUTPUT_SHOW_COUNT_YES = 'Y';   // Show output line count
   final private char OUTPUT_SHOW_COUNT_NO  = 'N';   // Don't show line count
   final private char OUTPUT_SHOW_COUNT     = OUTPUT_SHOW_COUNT_YES;
   
   private char outputMode;               // Output mode for outputLine()
                                          // ... Set by a constructor
   private PrintWriter outputFile = null; // Set by a Constr'r for outputLine()
   private int outputLineCounter = 0;     // Counts lines done in outputLine()
   
   // *******************************************************************
   // ************************** Constructors  **************************
   // *******************************************************************

   // *******************************************************************

   /** Default Constructor
   */
   
   public Toolkit() {
      setOutputMode(OUTPUT_MODE_DEFAULT); // Force the default setting
   } // End default, no-arg constructor
   
   // *******************************************************************
   
   // Constructor to set the outputMode to a specific value, 
   //    defaulting to the screen if the value is not valid
   
   public Toolkit(char outMode) {
      setOutputMode(outMode);     // Establish the outputMode for this instance
   } // End constructor with one argument

   // *******************************************************************
   
   /** Constructor to:
       1. Set the outputMode to a specific value, defaulting to the screen
       2. Set the outputFile to the desired file
   */
   public Toolkit(char outMode, FileWriter outputFileName) {
      outputFile = new PrintWriter(outputFileName);
      setOutputMode(outMode);     // Establish the outputMode for this instance
   } // End constructor with two arguments
     
   // *******************************************************************
   // ********************** End of Constructors  ***********************
   // *******************************************************************
   // ********************** Setters and Getters  ***********************
   // *******************************************************************

   // *******************************************************************
   
    /**
     * getClassInfo is a public method that when called will build and return 
     * a string array containing the main running class data.
     * @return String[] containing:
     * classInformation[0] = class package name,
     * classInformation[1] = class name,
     * classInformation[2] = class path.
     */
    public static String[] getClassInfo() {
        
        String stackTraceInfoString;
        String[] parsedStackTrace;
        String[] classInfoArray = new String[3];
        DummyClass dummyClass = new DummyClass();
        /* Get the stack trace string containing the package and class name. */
        stackTraceInfoString = getClassInfoStackTrace();
        /* Parse the stack trace string to split the package and class name. */
        parsedStackTrace = getClassInfoStackTraceParse(stackTraceInfoString);
        /* Store the stack trace info into classInfoArray. */
        for(int i = 0; i < parsedStackTrace.length; i++) {
            
            classInfoArray[i] = parsedStackTrace[i];
        }
        /* Get the class path from a dummy class created above and store it. */
        classInfoArray[2] = getClassInfoClassPath(dummyClass);
        
        return classInfoArray;
    }
   
   // *******************************************************************

   /** Method setOutputMode
       A Constructor above or a user can change the outputMode at any time
   */
   
   public void setOutputMode(char outMode) {
   
      // Assume the parameter is valid, but set the outputMode to the
      // default if it isn't
      
      outputMode = outMode;
      switch (outMode) {
         case OUTPUT_MODE_FILE  : break;  // Nothing else to do
         case OUTPUT_MODE_BOTH  : break;  // Nothing else to do
         case OUTPUT_MODE_SCREEN: break;  // Nothing else to do
         default                : outputMode = OUTPUT_MODE_DEFAULT;
      } // End switch
   } // End setOutputMode
   
   // *******************************************************************

   /** Method getOutputMode
       Return the current outputMode
   */
   
   public char getOutputMode() {
      return outputMode;
   } // End getOutputMode
   
   // *******************************************************************
   // ***************** Class Methods (alphabetically) ******************
   // *******************************************************************
   
   // *******************************************************************

   /** getCurrentDate - no arguments
       Get the current date in default format

       Input
         @param   None. Use the string MM/dd/yyyy as a default
         
       Process    Call getCurrentDate() with the default format string
         
       Output
         @return  The current date in the default format
   */

   public static String getCurrentDate() {
       return getCurrentDate("MM/dd/yyyy");
   } // End getCurrentDate, no arguments
   
   // *******************************************************************

   /** getCurrentDate - one argument
       Get the current date in the format supplied by the caller

       Input
         @param   The string to use to format. Example: "yyyy/MM/dd"
         
       Process    Use the Date class to format the current date
         
       Output
         @return  The current date in the requested format
    */

   public static String getCurrentDate(String theFormat) {
       SimpleDateFormat dateFmt = new SimpleDateFormat(theFormat);
       Date theDate = new Date();
       return dateFmt.format(theDate);
   } // End getCurrentDate, one argument
   
   // *******************************************************************

   /** getCurrentDateTime - no arguments
       Get the current date and time in date/time default formats

       Input
         @param   None
         
       Process    Call getCurrentDate() and getCurrentTime() using defaults

       Output
         @return  The current date and time in the default formats
   */

   // *******************************************************************

   public static String getCurrentDateTime() {
       return getCurrentDate() + " " + getCurrentTime();
   } // End getCurrentDateTime
   
   // *******************************************************************

   /** getCurrentTime - no arguments
       Get the current time in default HH:MM:SS format

       Input
         @param   None. Use the default format
         
       Process    Use getCurrentTime with the default format
         
       Output
         @return  The current date in the default format
  */
   
   public static String getCurrentTime() {
     	return getCurrentTime("HH:mm:ss");
    } // End getCurrentTime

   // *******************************************************************

   /** getCurrentTime - one argument
       Get the current time in requested format

       Input
         @param   A string with the desired time format
         
       Process    Use the Calendar class to get and format the current time
         
       Output
         @return  The current date in the default format
  */
   
   public static String getCurrentTime(String theFormat) {
      Calendar cal = Calendar.getInstance();
     	cal.getTime();
     	SimpleDateFormat sdf = new SimpleDateFormat(theFormat);
     	return sdf.format(cal.getTime());
    } // End getCurrentTime

   // *******************************************************************

   /** giveMeAnS - pluralizes a word with an "s", if necessary
       Return an "s" if the number supplied is not 1; null string otherwise.
       Used to add an "s" to a word that might or might not be pluralized
       based on the quantity. No attempt is made to determine whether the
       pluralization should yield an "-es" at the end of the word

       Spec Reference   None

       Input
         @param   quantity, an integer representing a quantity
         
       Process    Use 'giveMeAPlural' to do the exceedingly heavy work
         
       Output
         @return  "s" if quantity != 1
                  "", if quantity  = 1
         
       Note       None.
   */
   
   public static String giveMeAnS(int quantity) {
      return giveMeAPlural(quantity, "s");
   } // End giveMeAnS

   // *******************************************************************

   /** giveMeAPlural - pluralizes a word with a suffix, if necessary
       Return the string parameter if the number supplied is not 1; 
       null string otherwise.
       
       Used to add an "es" or any other suffix to a word that might or 
       might not be pluralized based on the quantity. No attempt is made
       to determine whether the pluralization should yield an "-es" at 
       the end of the word.

       Spec Reference   None

       Input
         @param   quantity, an integer representing a quantity
         @param   suffix, the string to append if quantity != 1
         
       Process    1. If the quantity = 1, set the return string to the empty
                     string;
                  2. Otherwise, set the return string to the string parameter
         
       Output
         @return  suffix, if quantity != 1
                  "", if quantity  = 1
         
       Note       None.
   */
   
   public static String giveMeAPlural(int quantity, String suffix) {
   
      String s = "";      // Assume quantity = 1
      if (quantity != 1) { s = suffix; }
      return s;
   } // End giveMeAPlural

   // *******************************************************************

   /** leftPad, 4 parameters
       Convert a number to a string and pad it on the left

       Spec Reference   Introduced with Assignment 5

       Input
         @param   number   A double number to be formatted
         @param   width    The width of the formatted number after padding
         @param   mask     The DecimalFormat mask to use
         @param   padding  The string to use for padding, usually a space
         
       Process    1. Convert the number to a string
                  2. Use padString to finish the job
         
       Output
         @return  strPad   The formatted number, padded if necessary
         
       Notes      Unless the calling method uses DecimalFormat elsewhere
                  in its program, the calling program does not need to import
                  the DecimalFormat class as that is done here
                  
                  The string resulting from leftPad could be longer than the 
                  width specified if its length once converted to a string
                  exceeds the width. For example, the value 123456789.1 with
                  a width of 8 formatted with a mask of "#0.00" returns the
                  String value   123456789.10    which exceeds 8 characters

      Examples    The tilde (~) character is used to denote a space.
                  Assume double variable 'value'  equals 45.6789,
                         double variable 'dollar' equals 1.2, and
                         double variable 'doll2'  equals 1234.56
                  Note the rounding that is done where appropriate
      
                  leftPad call                              Result
                  ----------------------------------------  -------------------
                  leftPad(value,  8, "0", " ")              ~~~~~~46
                  leftPad(value,  8, "0.0, " ")             ~~~~45.7
                  leftPad(value,  8, "0.00, " ")            ~~~45.68
                  leftPad(dollar, 8, "0.00", " ")           ~~~~1.20
                  leftPad(dollar, 8, "0.00", "*")           ****1.20
                  leftPad(doll2, 12, "#,##0.00", " ")       ~~~~1,234.56
                  leftPad(doll2, 12, "#,##0.00", "$")       $$$$1,234.56
                  leftPad(doll2, 12, "#,##0", " ")          ~~~~~~~1,235
   */

   public static String leftPad (double number, 
                                 int width, 
                                 String mask, 
                                 String padding) {
      final String NO_RIGHT_PAD = "";  // No padding on the right
      String strPad;                   // String to be returned
   
      //  1. Convert number to a String and
      //  2. Use the string padding method to finish the job

      DecimalFormat fmt = new DecimalFormat(mask);
      strPad = padString(fmt.format(number), width, padding, NO_RIGHT_PAD);
      return strPad;
   } // end leftPad, 4 parameters

   // *******************************************************************

   /** leftPad, 3 parameters that assume a blank pad on the left
       Convert a number to a string and pad it on the left

       Spec Reference   Introduced with Assignment 5

       Input
         @param   number   A double number to be formatted
         @param   width    The width of the formatted number after padding
         @param   mask     The DecimalFormat mask to use
         
       Process    1. Call the 4-parameter leftPad with blank padding
         
       Output
         @return  strPad   The formatted number, padded if necessary
         
       Notes      See notes for leftPad, 4 paramters
       
      Examples    The tilde (~) character is used to denote a space.
                  Assume double variable 'value'  equals 45.6789,
                         double variable 'dollar' equals 1.2, and
                         double variable 'doll2'  equals 1234.56
                  Note the rounding that is done where appropriate
      
                  leftPad call                              Result
                  ----------------------------------------  -------------------
                  leftPad(value,  8, "0")                   ~~~~~~46
                  leftPad(value,  8, "0.0)                  ~~~~45.7
                  leftPad(value,  8, "0.00)                 ~~~45.68
                  leftPad(dollar, 8, "0.00")                ~~~~1.20
                  leftPad(dollar, 8, "0")                   ~~~~~~~1
                  leftPad(doll2, 12, "#,##0.00")            ~~~~1,234.56
                  leftPad(doll2, 12, "#,##0")               ~~~~~~~1,235
   */

   public static String leftPad (double number, int width, String mask) {
      String strPad;                   // String to be returned
      strPad = leftPad(number, width, mask, " ");
      return strPad;
   } // end leftPad, 3 parameters

   // *******************************************************************

   /** outputLine
       Output a line of text to the screen, a file, or both
            
       Spec Reference   Done in class

       Input
         @param   The line to output
         
                  Uses private variables
                  outputMode  to determine where to send the output
                  outputFile  to print to a file
         
       Process    Use the outputMode to determine where the output goes
                  If the output is to a file and the file is not defined,
                     give a message
         
       Output
         @return  None
         
       Note       The Constructors are set up so no matter which one is used
                  to instantiate the class, the outputMode should be valid, so
                  we don't check if outputMode is valid
   */     

   public void outputLine(String theLine) {
   
      String outputMe = theLine; // Use a local variable for outputting
      
      // Increment the line counter. Note that if the outputFile is invalid, 
      // the output line count is decremented, so incrementing it here leaves
      // the net count unchanged by the end of the method
      
      // If the output line counter is requested for display, attach it to the
      // line being printed as a prefix
      
      outputLineCounter++;
      if (OUTPUT_SHOW_COUNT == OUTPUT_SHOW_COUNT_YES) {
         outputMe = outputLineCounter + OUTPUT_COUNT_DELIMETER +  outputMe;
      } // End if
      
      if (outputMode == OUTPUT_MODE_SCREEN || outputMode == OUTPUT_MODE_BOTH) {
         System.out.println(outputMe);
      } // End if
      if (outputMode == OUTPUT_MODE_FILE || outputMode == OUTPUT_MODE_BOTH)
      {
         if (outputFile != null) { // i.e., a file has been defined 
            outputFile.println(outputMe);
         }
         else {
            System.out.println("File not set for line: "  + outputMe);
            outputLineCounter--;    // Adjust the # of valid lines printed
         } // End if (outputFile != null) 
      } // End if (outputMode == OUTPUT_MODE_FILE ...
   } // End outputLine

   // *******************************************************************

   /** padNumber
       Format and pad a number on the left and/or right side
   
       Spec Reference   Introduced along with Assignment 7

       Input
         @param   number   A double number to be formatted
         @param   width    The width of the resulting formatted number
         @param   mask     The DecimalFormat mask to use
         @param   padLeft  The string to use for padding on the left;  null OK
         @param   padRight The string to use for padding on the right; null OK
         
       Process    1. Convert the number to a string
                  2. If the width is too small, give it the default length
                  3. Pad the string on the left and/or right
         
       Output
         @return  strPad   The formatted number, padded if necessary
 
       Note       To pad on only one side, say the left, use the empty
                  string to pad on the right

                  If both the left and right padding strings are NOT null,
                  the resulting string will end up longer than 'width'
                  
                  Also see notes with method leftPad
   */    
    
   public static String padNumber(double number, 
                                  int width, 
                                  String mask,
                                  String padLeft,
                                  String padRight) {
      String strPad;       // String to be returned, possibly padded
   
      //  Convert number to a String and use the string padding method to finish
      
      DecimalFormat fmt = new DecimalFormat(mask);
      strPad = padString(fmt.format(number), width, padLeft, padRight);
      return strPad;
   } // end padNumber()
   
   // *******************************************************************

   /** padString, 4 parameters
       Pad a string on the left and/or right side
   
       Input
         @param   str      A string to be padded
         @param   width    The width of the resulting formatted number
         @param   padLeft  The string to use for padding on the left
         @param   padRight The string to use for padding on the right
         
       Process    1. If the width is too small, give it the default length
                  2. Pad the string on the left and/or right
         
       Output
         @return  strPad   The formatted string, padded if necessary
 
       Note       To pad on only one side, say the left, use the empty
                  string to pad on the right
                  
                  If BOTH the left and right padding strings are NOT null,
                  the resulting string will end up longer than 'width'

                  Also see notes with method leftPad

      Examples    The tilde (~) character is used to denote a space.
                  Assume string variable 'name' has the name "John"
      
                  padString call                            Result
                  ----------------------------------------  -------------------
                  padString(name, 8, "", " ")               John~~~~
                  padString(name, 8, " ", "")               ~~~~John
                  padString(name, 8, " ", " ")              ~~~~John~~~~
                  padString(name, 8, "", "*")               John****
   */    
    
   public static String padString(String str, 
                                  int width, 
                                  String padLeft,
                                  String padRight) {
                                    
      String strPad = str;         // String to be returned, starts as the param
      int charsToPad;              // The number of charactes to pad
   
      // Using the length of the String str, calculate the number of characters
      // to pad on the left. Note the number could be <= 0
      charsToPad = width - strPad.length();
   
      // Pad str by spaces on the left and/or right so that the
      // resulting length of strPad is 'width' characters
      for (int i = 0;  i < charsToPad;  i++) {
         strPad = padLeft + strPad + padRight;
      } // End for
      return strPad;
   } // end padString, 4 parameters

   // *******************************************************************

   /** padString, 2 parameters
       Pad a string on the right side with blanks
   
       Input
         @param   str      A string to be padded
         @param   width    The width of the resulting formatted number
         
       Process    Use padString with 4 parameters, an empty string for the
                  right pad parameter and a blank for the left pad
                  parameter, so the string is left-padded with blanks
         
       Output
         @return  strPad   The formatted string, padded if necessary
         
      Examples    The tilde (~) character is used to denote a space.
                  Assume string variable 'name' has the name "John"
      
                  padString call                            Result
                  ----------------------------------------  -------------------
                  padString(name,  8)                        John~~~~
                  padString(name, 12)                        John~~~~~~~~
   */    
    
   public static String padString(String str, int width) {
                                    
      String strPad;         // String to be returned
      strPad = padString(str, width, "", " ");
      return strPad;
   } // end padString, 2 parameters

   // *******************************************************************

   /** partiallyFillArray
       Fill an integer array up to and including its declared length

       Spec Reference   Introduced in Assignment 10

       Input
         @param   inp   Scanner file to read
         @param   array Integer array to fill. The array length has been set
                        in a declaration statement prior to this call
         
       Process    1. Get the length of the array to use as a read limit
                  2. Read each line from the file and store only those elements
                     that fit into the array
                  3. Calculate the total number of input records read
         
       Output
         @return  nRead, the number of input records read which is <= the
                  length of the array
         
       Note       We calculate the number of rows left over after the array is
                  filled, but don't do anything with it as of now. For example,
                  if there are 20 elements in the array and 25 numbers in the
                  input file, the left overs will be 5 rows
   */
         
   public static int partiallyFillArray(Scanner inp, int[] array) {
   
      int len = array.length;    // Declared length of the array
      int nRead = 0;             // # of rows read, which is <= len
      int leftOver;              // # of rows not read into the array
      
      // Read the data and store elements in the array only if there's
      // enough room for them
      
      while (inp.hasNext()) {
         // If we haven't reached the limit of the arrays, store the data from
         // next row into the arrays
         if (nRead < len) {
            array[nRead] = inp.nextInt();
         }
         else { // There's more data, but we're ignoring it
            inp.nextLine();     // Flush unstored data
         } // End if
         nRead++;       // Indicate a record has been read 
      } // End while
      
      // Here, we finished reading the file. If nRead is more than the length
      // of the arrays, we have leftovers and must set the # of rows read to
      // the length of the array. If nRead <= the length of the arrays, we use
      // nRead as the number of rows in the arrays. The 'min' method does
      // the trick!
      
      leftOver = Math.max(nRead-len,0);   // Number of numbers not in the array
      nRead = Math.min(nRead,len);        // Must be done AFTER leftOver calc 
      
      return nRead;
   } // End partiallyFillArray
   
   // *******************************************************************

   /** rightPad, 4 parameters
       Pad a string on the right
   
       Spec Reference   Introduced along with Assignment 7

       Input
         @param   number   A double number to be formatted
         @param   width    The width of the formatted number after padding
         @param   mask     The DecimalFormat mask to use
         @param   padding  The string to use for padding, usually a space
         
       Process    1. Convert the number to a string
                  2. Use padString to finish the job
         
       Output
         @return  strPad   The formatted number, padded if necessary
 
       Note       See the notes with the leftPad method, 4 parameters

      Examples    The tilde (~) character is used to denote a space.
                  Assume double variable 'value'  equals 45.6789,
                         double variable 'dollar' equals 1.2, and
                         double variable 'doll2'  equals 1234.56
                  Note the rounding that is done where appropriate
      
                  rightPad call                              Result
                  ----------------------------------------  -------------------
                  rightPad(value,  8, "0", " ")             46~~~~~~
                  rightPad(value,  8, "0.0, " ")            45.7~~~~
                  rightPad(value,  8, "0.00, " ")           45.68~~~
                  rightPad(dollar, 8, "0.00", " ")          1.20~~~~
                  rightPad(dollar, 8, "0.00", "*")          1.20****
                  rightPad(doll2, 12, "#,##0.00", " ")      1,234.56~~~~
                  rightPad(doll2, 12, "#,##0.00", "$")      1,234.56$$$$
                  rightPad(doll2, 12, "#,##0", " ")         1,235~~~~~~~
   */    
    
   public static String rightPad(double number, 
                                 int width, 
                                 String mask,
                                 String padding) {
      final String NO_LEFT_PAD = "";  // No padding on the left
      String strPad;                  // String to be returned

      DecimalFormat fmt = new DecimalFormat(mask);
      strPad = padString(fmt.format(number), width, NO_LEFT_PAD, padding);
      return strPad;
   } // end rightPad

   // *******************************************************************

   /** rightPad, 3 parameters
       Pad a string on the right
   
       Spec Reference   Introduced along with Assignment 7

       Input
         @param   number   A double number to be formatted
         @param   width    The width of the formatted number after padding
         @param   mask     The DecimalFormat mask to use
         
       Process    Call rightPad with 4 parameters, padding with a blank
         
       Output
         @return  strPad   The formatted number, padded if necessary
 
       Note       See the notes with the leftPad method, 4 parameters

      Examples    The tilde (~) character is used to denote a space.
                  Assume double variable 'value'  equals 45.6789,
                         double variable 'dollar' equals 1.2, and
                         double variable 'doll2'  equals 1234.56
                  Note the rounding that is done where appropriate
      
                  rightPad call                              Result
                  ----------------------------------------  -------------------
                  rightPad(value,  8, "0")                  46~~~~~~
                  rightPad(value,  8, "0.0)                 45.7~~~~
                  rightPad(value,  8, "0.00)                45.68~~~
                  rightPad(dollar, 8, "0.00")               1.20~~~~
                  rightPad(doll2, 12, "#,##0.00")           1,234.56~~~~
                  rightPad(doll2, 12, "#,##0")              1,235~~~~~~~

   */    
    
   public static String rightPad(double number, int width, String mask) {
      String strPad;                  // String to be returned
      strPad = rightPad(number, width, mask, " ");
      return strPad;
   } // end rightPad

    // *******************************************************************
   
  /** roundNumber
      Round a number to N places  
            
       Spec Reference   None. Just a useful method!

       Input
         @param   valueToRound   A double number to round off
         @param   placesToRound  The number of places to round it to
         
       Process    1. Determine the power of 10 to use to divide the number
                  2. Multiply the number to round by the power of ten and 
                     round it off to the nearest decimal
                  3. Divide by that same power of ten to get the rounding
         
       Output
         @return  The number rounded to the requested number of places
         
       Note
  */
   
   public static double roundNumber(double valueToRound, int placesToRound) {

      double roundMe;     // The number to round, starts as valueToRound
      double powerOfTen;  // To calculate 10^placesToRound 
      
      roundMe = valueToRound;                      // Ex: round 1.256, 2 places
      powerOfTen = Math.pow(10,placesToRound);     // 10 ^ 2 = 100
      roundMe = roundMe * powerOfTen;              // 1.256 * 100 = 125.6
      roundMe = Math.round(roundMe);               // 125.6 rounded = 126
      
      return roundMe / powerOfTen;                 // 126 / 100 = 1.26
   } // End roundNumber
   
   // *******************************************************************
   
   /** selectionSort
       Sort an integer array from smallest to largest
            
       Spec Reference   Introduced with Assignment 10

       Input
         @param   array An integer array to sort. It is already filled
         @param   len   Number of elements in the array to sort, that might
                        be less than the declared length of the array
         
       Process    1. Start at the first element and assume it is the smallest 
                     value. Call it the "current element"
                  2. Search the rest of the array and if you find a smaller
                     value, note it
                  3. At the end of the loop, swap the locations of the smallest
                     value with the current element of the array
                  4. Move to the next element of the array, call it the "current
                     element" and repeat steps 2 and 3 until we reach the next
                     to the last element. Then, the largest element will be in
                     the last location
         
       Output
         @return  None
         
       Note       The specification for this method was changed from the
                  original assignment by adding a parameter of the number of
                  elements to process in the sort. This allows for sorting a
                  partially-filled array
   */
   
   public static void selectionSort(int[] array, int len) {

    int k, i;		// Indexes into the array to find the minimum value
    int minIndex; // Index of the minimum value in the array
    int temp; 		// Temporary storage while swapping array elements

    /*      
      Each pass determines the location, minIndex, of the smallest value
      in the array elements:   Ary[k],  Ary[k+1], ... Ary[len - 1].
      Then the values in  Ary[k]  and  Ary[minIndex] are swapped.
    */
    
    for (k = 0;  k < len - 1;  k++) {  // Note we stop at length - 1
       //  Find the location, minIndex, of the smallest value in row k
       minIndex = k;  // Assume the minimum value is at location k
       for (i = k + 1;  i < len;  i++) {
           if (array[i] < array[minIndex]) 
               minIndex = i;
       } // End inner for

       //  Swap elements in the minIndex and k positions of the array
       temp = array[minIndex];
       array[minIndex] = array[k];
       array[k] = temp;
    } // End outer for
   } // End selectioSort, just the array, no index

   // *******************************************************************
   
   /** selectionSortWithIndex
       Sort an integer array from smallest to largest and keep track of
       the original indexes
            
       Spec Reference   Introduced with Assignment 10

       Input
         @param   array An integer array to sort. It is already filled
         @param   index An array that contains the indexes of the original
                        values which are typically index[i] = i + 1 to number
                        them for humans to read easily. That is,
                           index[0] = 1 = the first  element
                           index[1] = 2 = the second element, and so on
         @param   len   Number of elements in the array to sort, that might
                        be less than the length of the array
         
       Process    1. Start at the first element and assume it is the smallest
                     value. Call it the "current element"
                  2. Search the rest of the array and if you find a smaller
                     value, note it
                  3. At the end of the loop, swap the locations of the smallest
                     value with the current element of the array
                  4. Move to the next element of the array, call it the "current
                     element" and repeat steps 2 and 3 until we reach the next
                     to the last element. Then, the largest element will be in
                     the last location
         
       Output
         @return  None
         
       Note       The specification for this method was changed from the
                  original assignment by adding a parameter of the number of
                  elements to process in the sort. This allows for sorting a 
                  partially-filled array
   */
   
   public static void selectionSortWithIndex(int[] array, 
                                             int[] index, 
                                             int len) {
   
    int k, i;		// Indexes into the array to find the minimum value
    int minIndex; // Index of the minimum value in the array
    int temp; 		// Temporary storage while swapping array elements

    /*      
      Each pass determines the location, minIndex, of the smallest value
      in the array elements:   Ary[k],  Ary[k+1], ... Ary[len - 1].
      Then the values in  Ary[k]  and  Ary[minIndex] are swapped.
    */
    
    for (k = 0;  k < len - 1;  k++) {  // Note we stop at length - 1
    
       //  Find the location, minIndex, of the smallest value in row k
       minIndex = k;  // Assume the minimum value is at location k
       for (i = k + 1;  i < len;  i++) {
           if (array[i] < array[minIndex]) 
               minIndex = i;
       } // End inner for

       //  Swap elements in the minIndex and k positions of the array
       temp = array[minIndex];
       array[minIndex] = array[k];
       array[k] = temp;
       
       // Swap the indexes, too
       temp = index[minIndex];
       index[minIndex] = index[k];
       index[k] = temp;

    } // End outer for
   }   // End selectionSortWithIndex

   // *******************************************************************

   /**
      selectionSortStringWithNumbers
      Sort a string or double array in ascending order. Keep its data, the 
      2-dimensional array 'numbers' in the same row as the names as the names
      get moved around. This method does NOT use indexes but swaps the String
      and the numeric entries
   
      The parameters are:
      
      @param   ary - a String array
      @param   numbers - a 2-dimensional array of real numbers
      @param   len - the number of possibly partially-filled elements in the 
               array (i.e., it might be < ary.length)
      @param   column - the column # in the numbers array to sort by, or -1 if 
               we're sorting by String. For example, if column = 2, it means
               we are sorting by column #2 which is the 3rd column
   */
   
   public static void selectionSortStringWithNumbers(String[] ary,
                                                     double[][] numbers,
                                                     int len,
                                                     int column) {

    /*
      The next two lines of code establish an array that can contain an
      entire row of the two-dimensional 'numbers' array. The first line
      gets the number of columns in one row. We make two assumptions here:
      
      1. The 'numbers' array is not empty (there is at least one entry). 
      2. The number of columns in the 'numbers' array is the same for all rows
         (i.e., a rectangular matrix). We use this idea in the first line of
         code that follows to access the first entry to get the # of column
         in any row of the matrix
    */

    int nColumns = numbers[0].length;  // # of columns in the 2-dimens'l array
    double[] oneRow = new double[nColumns];  // Will hold one row during a swap
    
    boolean isSortByString; // True if sort by String, false if by a # column
    int k, i;		   // Indexes into the array to assist with the sort
    int minIndex;    // Index of the minimum value in the array
    int aryLen;      // Length of the parameter array
    String strTemp;  // Temporary storage for string swapping

    // Set the flag for the entire sort based on whether we are sorting by
    // a String (parameter 'column' = -1) or we are sorting by one of the
    // columns in the double table (parameter 'column' = column # to sort by)
    
    isSortByString = (column < 0); // True means sort by String, false by numbers
      
    // Each pass determines the location, minIndex, of the largest value
    
    for (k = 0;  k < len - 1;  k++)
    {    
       //  Find the location, minIndex, of the smallest value in row k
       minIndex = k;  // Assume the minimum value is at location k
       for (i = k + 1;  i < len;  i++) {
           if (isSortByString) {
               if (ary[i].compareTo(ary[minIndex]) < 0) minIndex = i;
           } else { // We are sorting by a specific column named 'column'
               if (numbers[i][column] < numbers[minIndex][column]) minIndex = i;
           } // End if (isSortString)
       } // End for (i = k + 1;  i < len;  i++)

       // Swap elements in the minIndex and k positions of the arrays
       // First swap the names, the the columns

       strTemp = ary[minIndex];
       ary[minIndex] = ary[k];
       ary[k] = strTemp;

       // Now swap the column entries
       
       /* Swap all the columns of two rows. We swap entire rows in one fell
          swoop. This is much faster and takes a lot less code than swapping
          column by column in a for loop.
          Note that not all programming languages support this kind of 
          matrix operation.
        */
       oneRow = numbers[minIndex];        // Copy row minIndex
       numbers[minIndex] = numbers[k];    // Copy row k to minIndex
       numbers[k] = oneRow;               // Copy original row minIndex to row k
     } // End for (k = 0;  k < len - 1;  k++)
   } // End selectionSortStringWithNumbers
   
   // *******************************************************************

   /**
      selectionSortArrayOfClass
      Sort an array of the Employee class in ascending order.
      The parameters are:
      
      @param   empl - an array of the Employee class. If the array is empty,
                      the method simply returns with no action taken
      @param   len - the number of possibly partially-filled elements in the 
               Employee array (i.e., it might be < empl.length)
      @param   sortType - a string that indicates the type of sort:
                  "Name"      - sort by name
                  "Gross Pay" - sort by gross pay
      @return  0, sort was completed successfully
               1, the array to sort has no elements
               2, the sortType is invalid
   */
   
   public static int selectionSortArrayOfClass(
               Employee[] empl,
               int len,
               String sortType) {

    final String SORT_BY_NAME  = "NAME";
    final String SORT_BY_GROSS = "GROSS PAY";
    
    Employee tempEmpl = new Employee();   // Holds an entry being swapped
    
    boolean isSortByName;     // True if sort by name
    boolean isSortByGross;    // True if sort by gross pay
    int k, i;		   // Indexes into the array to assist with the sort
    int minIndex;    // Index of the minimum value in the array
    String howSort;  // Upper case of the sort type
    
    // Begin execution. If the employee array has no entries, there is nothing
    // to sort, so simply return
    
    if (empl.length == 0) {
      return 1;
    } // End if
    
    // Set the flag for the entire sort based on whether we are sorting by
    // a name (sortType = "Name") or we are sorting by the gross pay
    // (sortType = "Gross Pay"). If the the sortType is not one of the two
    // possibilities, the method returns with no action taken.
    
    howSort = sortType.toUpperCase().trim(); // Deal only with upper case
    isSortByName  = (howSort.equals(SORT_BY_NAME));   // True means sort by name
    isSortByGross = (howSort.equals(SORT_BY_GROSS));  // True means sort by gross
    
    // If neither of the sort types is true, the user used an unrecognized sort
    // type, so just return
    
    if (!(isSortByName || isSortByGross)) {
      return 2;
    } // End if
    
    // At this point, the empl array has at least one entry, and we are
    // sorting by name or by gross pay.
    // Each pass determines the location, minIndex, of the smallest value
    
    for (k = 0;  k < len - 1;  k++)
    {    
       //  Find the location, minIndex, of the smallest value in row k
       minIndex = k;  // Assume the minimum value is at location k
       
       // We check once for each pass of control variable k to see whether
       // we are sorting by name or gross pay. This approach is in contrast
       // to one inner for loop (control variable i) in which we ask each
       // time through the loop what type of sort we are doing.
       // This approach saves a whole bunch of times we ask what type
       // of sort we're doing at the expense of a slightly larger program.

       if (isSortByName) {
          for (i = k + 1;  i < len;  i++) {
               if (empl[i].name.compareTo(empl[minIndex].name) < 0) minIndex = i;
          } // End for (i = k + 1;  i < len;  i++)
       } // End 'if' sorting by name
       else { // We are sorting by gross pay
          for (i = k + 1;  i < len;  i++) {
               if (empl[i].grossPay < empl[minIndex].grossPay) minIndex = i;
          } // End for (i = k + 1;  i < len;  i++)
       } // End 'else' sorting by gross pay
       
       // Swap elements in the minIndex and k positions of the arrays

       tempEmpl = empl[minIndex];
       empl[minIndex] = empl[k];
       empl[k] = tempEmpl;
     } // End for (k = 0;  k < len - 1;  k++)
     
     return 0;    // Indicate the sort ended OK
   } // End selectionSortArrayOfClass
   
   // **************************************************************************
    /**
     * getClassInfoStackTraceParse will take the stack trace string and determine if the 
     * string has a "." delimeter to split a package and class name.  It will 
     * then store that information into a string array to be returned.
     * @param stackTraceInfo - String containing package name and class name.
     * @return String[] containing:
     * parsedStackTraceInfo[0] = package name,
     * parsedStackTraceInfo[1] = class name.
     */
    private static String[] getClassInfoStackTraceParse(String stackTraceInfo) {
        
        String packageName;
        String className;
        
        String[] parsedStackTraceInfo = new String[2];
        /* If the stacktrace contains a delimeter, split the data into two 
           separate strings. */
        if(stackTraceInfo.contains(".")){
            /* Get first string(package name). */
            packageName = stackTraceInfo.substring(
                    0, stackTraceInfo.indexOf("."));
            /* Get second string(class name). */
            className = stackTraceInfo.substring(
                    stackTraceInfo.indexOf(".") + 1, stackTraceInfo.length());
            /* Store the data into the string array. */
            parsedStackTraceInfo[0] = packageName;
            parsedStackTraceInfo[1] = className;
        /* If there is no delimeter, store the stackTraceInfo as the class name
           only. */
        } else {
            
            packageName = "Not Defined";
            className = stackTraceInfo;
            
            parsedStackTraceInfo[0] = packageName;
            parsedStackTraceInfo[1] = className;
        }
        
        return parsedStackTraceInfo;
    }
    /**
     * getClassInfoStackTrace will return a string containing the requested stack trace
     * information.  In this case, that will be the class package and class 
     * name.
     * @return String containing the requested stack trace information.
     */
    private static String getClassInfoStackTrace() {
        /* Get the stack trace for the current thread. */
        StackTraceElement[] stElements = Thread.currentThread().getStackTrace();
        /* Loop through the stack trace array and locate the array containing 
           the correct data. */
        for(int i = 1; i < stElements.length; i++){

            StackTraceElement ste = stElements[i];
            /* !!!!!!  This much match THIS CLASS NAME !!!!!!
             * !!!!!!  i.e. - .equals(This_Class_Name.class.getname()) !!!!!! */
            if(!ste.getClassName().equals(Toolkit.class.getName()) && 
                    ste.getClassName().indexOf("java.lang.Thread") != 0) {
                
                return ste.getClassName();
            }
        }
        return null;
    }
    /**
     * getClassInfoClassPath will determine the class path based on a dummy class that is
     * created in this class and passed through as a parameter.  It will then 
     * remove the forward or back slash from the path, if the slash precedes 
     * the path, i.e. - /C:path/ or \D:path\ -> C:path/ or D:path\.
     * @param dummyClass - is a class created in this class that is only created
     * in order to determine the path of this class.
     * @return String that contains the path of this class.
     */
    private static String getClassInfoClassPath(Object dummyClass) {
        
        String classPathString;
        /* Get the path to this class. */
        classPathString = dummyClass.getClass().getProtectionDomain().
                                getCodeSource().getLocation().getPath();
        /* If the path contains a preceding slash, remove it. */
        if(classPathString.startsWith("/") || 
                classPathString.startsWith("\\")) {
            
            classPathString = 
                    classPathString.substring(1, classPathString.length());
        }

        return classPathString;
    }
    /**
     * DummyClass is created for the following reason.  Due to the fact that a 
     * "non-static variable ... cannot be referenced from a static context", we
     * have to create a static class to pass through as a parameter to the 
     * getClassPath method in order to get the path of this class.
     */
    static class DummyClass {
        /* This class will not do anything. But for a class that does nothing, 
           it plays a very important role. */
    }

   // *******************************************************************
   // **************** Template for Method Documentation ****************
   // *******************************************************************

   // Replace <bracketed items>

   /** <Method Name>
       <Method Purpose>
            
       Spec Reference

       Input
         @param
         
       Process    1.
                  2.
                  3.
         
       Output
         @return
         
       Note       (Special comments about the method: usage, behavior, etc.)
   */

} // End class