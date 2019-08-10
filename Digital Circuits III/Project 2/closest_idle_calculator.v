/*
 * Title: Closest Idle Floor Calculator
 * This calculates the closest floor to idle at in case
 * there are no current floor requests.
 *
 * Author: Steven Jennings
 * Date: 2 May 2017
 * Copyright 2017 Steven Jennings
 */

module closest_idle_calculator(out, location, clk);
	// Define I/O
	output reg [3:0] out;
	input [3:0] location;
	input clk;
	
	// Define internal wires and registers
	reg [3:0] F1diff;
	reg [3:0] F10diff;
	reg [3:0] lowestDifference;
	reg [3:0] closestFloor;
	
	// Define floors
	parameter
	F1 = 4'd1,
	F10 = 4'd10;
	
	// Calculate each floor difference (limited to top floor and bottom floor)
	always begin
		// Calculate distance to F1
		if (F1 < location)
			F1diff <= location - F1;
		else
			F1diff <= F1 - location;
		
		// Calculate distance to F10
		if (F10 < location)
			F10diff <= location - F10;
		else
			F10diff <= F10 - location;
	end
	
	// Determine closest floor
	// This must operate sequentially or it may fail. DO NOT USE "<="
	always@(posedge clk) begin
		if (F1diff <= F10diff) // If the differences are identical, default to bottom floor
			closestFloor = F1;
		else
			closestFloor = F10;
	end
	
	// Set output to closest floor (to idle at)
	always begin
		out = closestFloor;
	end
endmodule