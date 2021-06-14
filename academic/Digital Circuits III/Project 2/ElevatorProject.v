/*
 * Title: Elevator Project
 * This is a fully functional elevator implementation in Verilog.
 * Disclaimer - does not include passenger destinations. It only includes
 * service requests.
 *
 * Author: Steven Jennings
 * Date: 27 April 2017
 * Copyright 2017 Steven Jennings. All rights reserved.
 *
 * Pin layout:
 * floorButtons[0]    <-> PIN_AE12
 * floorButtons[1]    <-> PIN_AD10
 * floorButtons[2]    <-> PIN_AC9
 * floorButtons[3]    <-> PIN_AE11
 * floorButtons[4]    <-> PIN_AD12
 * floorButtons[5]    <-> PIN_AD11
 * floorButtons[6]    <-> PIN_AF10
 * floorButtons[7]    <-> PIN_AF9
 * floorButtons[8]    <-> PIN_AC12
 * floorButtons[9]    <-> PIN_AB12
 * floorLights[0]     <-> PIN_Y21
 * floorLights[1]     <-> PIN_W21
 * floorLights[2]     <-> PIN_W20
 * floorLights[3]     <-> PIN_Y19
 * floorLights[4]     <-> PIN_W19
 * floorLights[5]     <-> PIN_W17
 * floorLights[6]     <-> PIN_V18
 * floorLights[7]     <-> PIN_V17
 * floorLights[8]     <-> PIN_W16
 * floorLights[9]     <-> PIN_V16
 * floorNumberOut[13] <-> PIN_AJ29
 * floorNumberOut[12] <-> PIN_AH29
 * floorNumberOut[11] <-> PIN_AH30
 * floorNumberOut[10] <-> PIN_AG30
 * floorNumberOut[9]  <-> PIN_AF29
 * floorNumberOut[8]  <-> PIN_AF30
 * floorNumberOut[7]  <-> PIN_AD27
 * floorNumberOut[6]  <-> PIN_AE26
 * floorNumberOut[5]  <-> PIN_AE27
 * floorNumberOut[4]  <-> PIN_AE28
 * floorNumberOut[3]  <-> PIN_AG27
 * floorNumberOut[2]  <-> PIN_AF28
 * floorNumberOut[1]  <-> PIN_AG28
 * floorNumberOut[0]  <-> PIN_AH28
 * sysclk             <-> PIN_AF14
 */

module ElevatorProject(floorNumberOut, floorLights, floorButtons, sysclk);
	// Define I/O
	output wire [13:0] floorNumberOut; // Output for two seven-segment displays
	output wire [0:9] floorLights; // Output for each floor LED
	input [0:9] floorButtons; // Input buttons for each floor
	input sysclk; // Input system clock
	
	// Define internal variables
	reg [1:0] state; // Variable to represent current state
	integer count; // Variable to hold count, driven by clockdivider
	wire clk; // 20Hz clock from clockdivider
	reg [0:9] floorRequest; // Variable to represent floor requests
	reg [3:0] currentFloor; // Variable to represent current floor of elevator
	reg [3:0] destinationFloor; // Variable to represent the destination floor of the elevator
	wire [3:0] closestFloor; // Variable to represent the closest requested floor
	wire [3:0] closestIdleFloor; // Variable to represent the closest floor to idle
	
	// Define states
	parameter
	S0 = 2'b00, // Idle
	S1 = 2'b01, // Moving
	S2 = 2'b10, // Temporary Latch Reset
	S3 = 2'b11; // Servicing
	
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
	
	// Instantiate 20Hz clock divider
	clockdivider c1(
	.out (clk),
	.in (sysclk)
	);
	// Instantiate dual seven-segment display module
	dual_seven_seg s1(
	.out (floorNumberOut),
	.in (currentFloor)
	);
	// Instantiate closest floor calculator
	closest_floor_calculator cfc(
	.out (closestFloor),
	.location (currentFloor),
	.requests (floorRequest),
	.clk (clk)
	);
	// Instantiate closest idle floor calculator
	closest_idle_calculator cfic(
	.out (closestIdleFloor),
	.location (currentFloor),
	.clk (clk)
	);
	
	// Initialize some variables
	initial begin
		state <= S0;
		count <= 0;
		currentFloor <= F1;
		destinationFloor <= F1;
		floorRequest <= 0;
	end
	
	// Output floor lights. Easiest line of code in this whole project.
	assign floorLights = floorRequest;
	
	// Control button inputs (floorButtons) and floor requests (floorRequest)
	always@(posedge sysclk) begin
		// Set latches
		if (state == S0 || state == S1 || state == S3) begin
			if (floorButtons[0])
				floorRequest[0] = 1;
			if (floorButtons[1])
				floorRequest[1] = 1;
			if (floorButtons[2])
				floorRequest[2] = 1;
			if (floorButtons[3])
				floorRequest[3] = 1;
			if (floorButtons[4])
				floorRequest[4] = 1;
			if (floorButtons[5])
				floorRequest[5] = 1;
			if (floorButtons[6])
				floorRequest[6] = 1;
			if (floorButtons[7])
				floorRequest[7] = 1;
			if (floorButtons[8])
				floorRequest[8] = 1;
			if (floorButtons[9])
				floorRequest[9] = 1;
		end
		
		// Reset latches
		if (state == S2) begin
			if (currentFloor == F1)
				floorRequest[0] = 0;
			if (currentFloor == F2)
				floorRequest[1] = 0;
			if (currentFloor == F3)
				floorRequest[2] = 0;
			if (currentFloor == F4)
				floorRequest[3] = 0;
			if (currentFloor == F5)
				floorRequest[4] = 0;
			if (currentFloor == F6)
				floorRequest[5] = 0;
			if (currentFloor == F7)
				floorRequest[6] = 0;
			if (currentFloor == F8)
				floorRequest[7] = 0;
			if (currentFloor == F9)
				floorRequest[8] = 0;
			if (currentFloor == F10)
				floorRequest[9] = 0;
		end
	end
	
	// Main code. This is the code that controls the flow of the state machine.
	// Reminder: clk runs at 20Hz
	always@(posedge clk) begin
		case (state)
			// State 0: Elevator is idle, waiting for button press. When button is pressed,
			// this code calculates the destination floor, which should be the closest one.
			// There is a tiny delay that allows the closest floor calculator to operate in time.
			S0: begin
				if (|floorRequest == 1) begin // Go to state 1 if any request is made
					if (count >= 1) begin // Tiny delay
						destinationFloor <= closestFloor;
						state <= S1;
						count <= 0;
					end else begin
						count <= count + 1;
					end
				end else if (currentFloor != F1 && currentFloor != F10) begin // Calculate closest floor to idle at
					if (count >= 1) begin // Tiny delay
						destinationFloor <= closestIdleFloor;
						state <= S1;
						count <= 0;
					end else begin
						count <= count + 1;
					end
				end
			end
			
			// State 1: Elevator is moving to destination floor.
			S1: begin
				if (destinationFloor < currentFloor) begin // Move down if destination is below
					if (count >= 40) begin // Two seconds between floors
						currentFloor <= currentFloor - 1;
						count <= 0;
					end else begin
						count <= count + 1;
					end
				end else if (destinationFloor > currentFloor) begin // Move up if destination is above
					if (count >= 40) begin // Two seconds between floors
						currentFloor <= currentFloor + 1;
						count <= 0;
					end else begin
						count <= count + 1;
					end
				end else if (destinationFloor == currentFloor) begin // Go to state 2 if destination is reached
					state <= S2;
					count <= 0;
				end
			end
			
			// State 2: Very short state to reset appropriate latches. Since the latch resets run on the
			// system clock, this state should only need a single cycle from the 20Hz clock.
			S2: begin
				state <= S3;
				count <= 0; // Technically not needed; Implemented anyway
			end
			
			// State 3: Elevator is servicing destination floor.
			S3: begin
				if (count >= 80) begin // Service for four seconds
					state <= S0;
					count <= 0;
				end else begin
					count <= count + 1;
				end
			end
			
			// Default: In case something weird happens, set state to idle and count to 0.
			default: begin
				state <= S0;
				count <= 0;
			end
		endcase
	end
endmodule
