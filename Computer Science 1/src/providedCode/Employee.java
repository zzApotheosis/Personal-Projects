// Employee class - data values for each employee

package providedCode;

public class Employee {
    public String name;             // Name of the employee
    public double hoursWorked;      // Hours worked in the payroll period
    public double payRate;          // Hourly pay rate
    public double grossPay;         // Gross pay based on the number of hours worked
    public double adjustedGrossPay; // Gross pay less amount for the IRA
    public double netPay;           // Gross pay less taxes
    public double savingsAmount;    // Amount of gross pay for savings
    public double iraAmount;        // Amount of gross pay for the IRA
    public double taxAmount;        // Amount of tax based on gross pay and tax rates
    public double wealth;           // Savings Amount + IRA Amount
} // End Employee