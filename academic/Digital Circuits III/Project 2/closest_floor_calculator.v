/*
 * Title: Closest Floor Calculator
 * This module analyzes the location of the elevator and determines
 * which of the given requested floors is the closest.
 *
 * Author: Steven Jennings
 * Date: 27 April 2017
 * Copyright 2017 Steven Jennings
 */

module closest_floor_calculator(out, location, requests, clk);
	// Define I/O
	output reg [3:0] out;
	input [3:0] location;
	input [0:9] requests;
	input clk;
	
	// Define internal wires and registers
	reg [3:0] F1diff;
	reg [3:0] F2diff;
	reg [3:0] F3diff;
	reg [3:0] F4diff;
	reg [3:0] F5diff;
	reg [3:0] F6diff;
	reg [3:0] F7diff;
	reg [3:0] F8diff;
	reg [3:0] F9diff;
	reg [3:0] F10diff;
	reg [3:0] lowestDifference;
	reg [3:0] closestFloor;
	
	// Define floors
	parameter
	F1 = 4'd1,
	F2 = 4'd2,
	F3 = 4'd3,
	F4 = 4'd4,
	F5 = 4'd5,
	F6 = 4'd6,
	F7 = 4'd7,
	F8 = 4'd8,
	F9 = 4'd9,
	F10 = 4'd10;
	
	// Calculate each floor difference
	always begin
		// Distance to F1
		if (requests[0]) begin
			if (F1 < location) begin
				F1diff <= location - F1;
			end else begin
				F1diff <= F1 - location;
			end
		end else begin
			F1diff <= 4'b1111;
		end
		
		// Distance to F2
		if (requests[1]) begin
			if (F2 < location) begin
				F2diff <= location - F2;
			end else begin
				F2diff <= F2 - location;
			end
		end else begin
			F2diff <= 4'b1111;
		end
		
		// Distance to F3
		if (requests[2]) begin
			if (F3 < location) begin
				F3diff <= location - F3;
			end else begin
				F3diff <= F3 - location;
			end
		end else begin
			F3diff <= 4'b1111;
		end
		
		// Distance to F4
		if (requests[3]) begin
			if (F4 < location) begin
				F4diff <= location - F4;
			end else begin
				F4diff <= F4 - location;
			end
		end else begin
			F4diff <= 4'b1111;
		end
		
		// Distance to F5
		if (requests[4]) begin
			if (F5 < location) begin
				F5diff <= location - F5;
			end else begin
				F5diff <= F5 - location;
			end
		end else begin
			F5diff <= 4'b1111;
		end
		
		// Distance to F6
		if (requests[5]) begin
			if (F6 < location) begin
				F6diff <= location - F6;
			end else begin
				F6diff <= F6 - location;
			end
		end else begin
			F6diff <= 4'b1111;
		end
		
		// Distance to F7
		if (requests[6]) begin
			if (F7 < location) begin
				F7diff <= location - F7;
			end else begin
				F7diff <= F7 - location;
			end
		end else begin
			F7diff <= 4'b1111;
		end
		
		// Distance to F8
		if (requests[7]) begin
			if (F8 < location) begin
				F8diff <= location - F8;
			end else begin
				F8diff <= F8 - location;
			end
		end else begin
			F8diff <= 4'b1111;
		end
		
		// Distance to F9
		if (requests[8]) begin
			if (F9 < location) begin
				F9diff <= location - F9;
			end else begin
				F9diff <= F9 - location;
			end
		end else begin
			F9diff <= 4'b1111;
		end
		
		// Distance to F10
		if (requests[9]) begin
			if (F10 < location) begin
				F10diff <= location - F10;
			end else begin
				F10diff <= F10 - location;
			end
		end else begin
			F10diff <= 4'b1111;
		end
	end
	
	// Determine closest floor
	// This must operate sequentially or it may fail. DO NOT USE "<="
	always@(posedge clk) begin
		closestFloor = F10;
		lowestDifference = F10diff;
		if (F9diff < lowestDifference) begin
			closestFloor = F9;
			lowestDifference = F9diff;
		end
		
		if (F8diff < lowestDifference) begin
			closestFloor = F8;
			lowestDifference = F8diff;
		end
		
		if (F7diff < lowestDifference) begin
			closestFloor = F7;
			lowestDifference = F7diff;
		end
		
		if (F6diff < lowestDifference) begin
			closestFloor = F6;
			lowestDifference = F6diff;
		end
		
		if (F5diff < lowestDifference) begin
			closestFloor = F5;
			lowestDifference = F5diff;
		end
		
		if (F4diff < lowestDifference) begin
			closestFloor = F4;
			lowestDifference = F4diff;
		end
		
		if (F3diff < lowestDifference) begin
			closestFloor = F3;
			lowestDifference = F3diff;
		end
		
		if (F2diff < lowestDifference) begin
			closestFloor = F2;
			lowestDifference = F2diff;
		end
		
		if (F1diff < lowestDifference) begin
			closestFloor = F1;
			lowestDifference = F1diff;
		end
	end
	
	// Set output to closest floor
	always begin
		out = closestFloor;
	end
endmodule