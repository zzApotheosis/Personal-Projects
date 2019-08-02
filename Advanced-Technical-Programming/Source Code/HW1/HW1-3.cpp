// Created by Steven Jennings
// Date: 09 June 2016
//
// This next task "prompts the user for a voltage and a resistance."
// Using this information, the program calculates the current through the resistor,
// and calculates the power dissipated by the resistor.

#include "stdafx.h"


int main()
{
	printf("This program accepts two inputs from the user,\n");
	printf("a voltage (volts), and a resistance (ohms).\n");
	printf("The program then calculates the current (amperes) through,\n");
	printf("and the power (watts) dissipated by the resistor.\n");
	
	double voltage;
	double resistance;
	
	printf("\n");
	printf("Enter a voltage.\n");
	scanf("%lf", &voltage);
	printf("Enter a resistance.\n");
	scanf("%lf", &resistance);

	double current = voltage / resistance;
	double power = voltage * voltage / resistance;

	printf("\n");
	printf("Voltage: %lf Volts\n", voltage);
	printf("Current: %lf Amps\n", current);
	printf("Resistance: %lf Ohms\n", resistance);
	printf("Power: %lf Watts\n", power);
	getchar();
	getchar();
}

