/*
 * Title: Traffic Light Project
 * This is a fully functional traffic light system, complete with crosswalk buttons.
 *
 * Authors: Steven Jennings, Scott Allen
 * Date: 18 April 2017
 * Copyright (C) 2017 Steven Jennings
 */

module New_TrafficLight_Project(
red_cr, yellow_cr, green_cr, arrow_cr, //Cross Street lights
red_mn, yellow_mn, green_mn, arrow_mn, //Main Street lights
crossOut, //Crosswalk timer
straight_cr, turn_cr, turn_mn, btn_cr, btn_mn, //Inputs
sysclk);
	
	//Declare I/O
	output reg red_cr, yellow_cr, green_cr, arrow_cr; //Cross Street lights
	output reg red_mn, yellow_mn, green_mn, arrow_mn; //Main Street lights
	output reg [13:0] crossOut; //Crosswalk timer outputs to two 7-seg displays in a 14-bit register.
	
	input straight_cr, turn_cr, turn_mn, btn_cr, btn_mn; //Street sensors, two crosswalk buttons
	input sysclk; //System clock
	
	reg [3:0] state; //State register to represet state machine diagram.
	reg done; //Conditional register for crosswalk timers.
	reg [3:0] num; //4-bit number for crosswalk counter. Max number is 15.
	reg btn_cr_reg, btn_mn_reg; //Latch-like registers for crosswalk buttons.
	reg clk; //Register to hold the output of the clock divider.
	
	integer count, counter; //count for state machine flow. counter for clock divider.
	
	//Declare states
	parameter
	S0 = 4'b0000,
	S1 = 4'b0001,
	S2 = 4'b0010,
	S3 = 4'b0011,
	S4 = 4'b0100,
	S5 = 4'b0101,
	S6 = 4'b0110,
	S7 = 4'b0111,
	S8 = 4'b1000,
	S9 = 4'b1001,
	S10 = 4'b1010,
	S11 = 4'b1011,
	S12 = 4'b1100,
	S13 = 4'b1101,
	S14 = 4'b1110,
	S15 = 4'b1111;
	
	//Initialize variables
	initial begin
		count = 0;
		counter = 0;
		btn_cr_reg = 0;
		btn_mn_reg = 0;
		state = S0;
		done = 1;
	end
	
	//Clock divider
	always@(posedge sysclk) begin
		counter <= counter + 1;
		if (counter >= 50000000) begin
			clk <= 1'b1; //Pulse module output. This creates a 1Hz clock with a super low duty cycle.
			counter <= 0; //Reset counter
		end else begin
			clk <= 1'b0; //Clock is zero otherwise.
		end
	end
	
	//Crosswalk timer
	always@(posedge clk) begin
		if ((btn_cr_reg || btn_mn_reg) && (state == S1 || state == S6)) begin //This if condition keeps an eye on the crosswalk registers. If either of them are set, and an appropriate state has been reached, begin the timer.
			done = 0;
		end
		if (~done) begin //Begin countdown when the done condition has been set to 0.
			case (num)
				4'b1111 : crossOut <= 14'b10011110100100;
				4'b1110 : crossOut <= 14'b10011111001100;
				4'b1101 : crossOut <= 14'b10011110000110;
				4'b1100 : crossOut <= 14'b10011110010010;
				4'b1011 : crossOut <= 14'b10011111001111;
				4'b1010 : crossOut <= 14'b10011110000001;
				4'b1001 : crossOut <= 14'b11111110000100;
				4'b1000 : crossOut <= 14'b11111110000000;
				4'b0111 : crossOut <= 14'b11111110001111;
				4'b0110 : crossOut <= 14'b11111110100000;
				4'b0101 : crossOut <= 14'b11111110100100;
				4'b0100 : crossOut <= 14'b11111111001100;
				4'b0011 : crossOut <= 14'b11111110000110;
				4'b0010 : crossOut <= 14'b11111110010010;
				4'b0001 : crossOut <= 14'b11111111001111;
				4'b0000 : crossOut <= 14'b11111110000001;
				default : crossOut <= 14'b11111110000001;
			endcase
			num = num - 1;
			if (num == 0) begin //Counter is finished. Reset values.
				done = 1;
				num = 15;
			end
		end else begin
			crossOut <= 14'b11111110000001;
		end
	end
	
	//This controls the crosswalk button registers.
	always@(posedge sysclk) begin
		if (~btn_cr) begin
			btn_cr_reg <= 1;
		end
		if (~btn_mn) begin
			btn_mn_reg <= 1;
		end
		if (btn_cr_reg && ~done) begin //Reset cross street register once the timer has started.
			btn_cr_reg <= 0;
		end
		if (btn_mn_reg && ~done) begin //Reset main street register once the timer has started.
			btn_mn_reg <= 0;
		end
	end
	
	//The main code. This controls the flow of the state machine. It operates on the clock divider's 1Hz pulse.
	always@(posedge clk)
	begin
		case (state)
			//State 0: Main street has the green light. State flow only yields to certain sensors.
			S0: if (count >= 15 && (straight_cr || turn_cr || btn_mn_reg))
					begin
						state <= S3;
						count <= 0;
					end else if (btn_cr_reg && done)
					begin
						state <= S1;
						count <= 0;
					end else
					begin
						count <= count + 1;
					end
					
			//State 1: Allow pedestrians to cross over cross street (with main street green).
			S1: if (count < 15)
					begin
						count <= count + 1;
					end else
					begin
						state <= S2;
					end
			
			//State 2: Temporary reset state for crosswalk register (Peds crossing over cross street).
			S2: 	begin
						count <= 0; //Reset the count.
						if (~(straight_cr || turn_cr)) //If nobody's waiting at cross street, remain in state 0, otherwise, go to State 3.
						begin
							state <= S0;
						end else if (done)
						begin
							state <= S3;
						end
					end
					
			//State 3: Main street gets yellow light for 4 seconds.
			S3: if (count < 4)
					begin
						count <= count + 1;
					end else
					begin
						state <= S4;
						count <= 0;
					end
			
			//State 4: All lights are red here for 1 second. Certain conditions will determine what happens next.
			S4: if (turn_cr)
					begin
						state <= S7; //People turning left at cross street get priority next.
					end else if (btn_mn_reg)
					begin
						state <= S6; //If nobody is turning, allow peds to cross.
					end else
					begin
						state <= S5; //If nobody is turning and nobody wants to cross, give the green light to cross street straight drivers.
					end
			
			//State 5: Cross street has green light for 10 seconds.
			S5: if (count < 10)
					begin
						count <= count + 1;
					end else if (done) //Done condition to make sure the crosswalk timer is done if it was running. If it wasn't running, then this condition is always true.
					begin
						state <= S8; //After 10 seconds, go to State 8.
						count <= 0;
					end
			
			//State 6: Pedestrians may begin crossing. According to the handout, the crosswalk must last for 15 seconds, but
			//the cross street may only be green for 10 seconds. This state lasts for 5 seconds to start the timer and keep the light red.
			S6: if (count < 5)
					begin
						count <= count + 1;
					end else
					begin
						state <= S5;
						count <= 0;
					end
			
			//State 7: Cross street left turners get the arrow for 6 seconds.
			S7: if (count < 6)
					begin
						count <= count + 1;
					end else if (count >= 6 && btn_mn_reg) //Allow pedestrians to cross next if they're waiting.
					begin
						state <= S6;
						count <= 0;
					end else if (count >= 6 && ~btn_mn_reg) //If no pedestrians, then go straight to the green light for cross street.
					begin
						state <= S5;
						count <= 0;
					end
			
			//State 8: Cross street gets the yellow light for 4 seconds.
			S8: if (count < 4)
					begin
						count <= count + 1;
					end else
					begin
						state <= S9;
						count <= 0;
					end
			
			//State 9: All lights are red here for one second.
			S9:	begin
						if (turn_mn)
						begin
							state <= S10; //If people are waiting to turn at main street. Let them.
						end else
						begin
							state <= S0; //Otherwise, main street gets the green light and the cycle repeats.
						end
					end
			
			//State 10: Main street gets the left green arrow for 6 seconds.
			S10: if (count < 6)
					begin
						count <= count + 1;
					end else
					begin
						state <= S0;
						count <= 0;
					end
			
			//Default: 
			default: begin state <= S0; count <= 0; end //If something weird happens, just set the state to 0.
		endcase
	end
	
	//The output driver. This code mainly drives the actual traffic lights.
	always@(posedge sysclk) begin
		case (state)
			S0:	begin
						//Control Main Street Lights
						red_mn = 1'b0;
						yellow_mn = 1'b0;
						green_mn = 1'b1;
						arrow_mn = 1'b0;
						
						//Control Cross Street Lights
						red_cr = 1'b1;
						yellow_cr = 1'b0;
						green_cr = 1'b0;
						arrow_cr = 1'b0;
					end
			S1:	begin
						//Control Main Street Lights
						red_mn = 1'b0;
						yellow_mn = 1'b0;
						green_mn = 1'b1;
						arrow_mn = 1'b0;
						
						//Control Cross Street Lights
						red_cr = 1'b1;
						yellow_cr = 1'b0;
						green_cr = 1'b0;
						arrow_cr = 1'b0;
					end
			S2:	begin
						//Control Main Street Lights
						red_mn = 1'b0;
						yellow_mn = 1'b0;
						green_mn = 1'b1;
						arrow_mn = 1'b0;
						
						//Control Cross Street Lights
						red_cr = 1'b1;
						yellow_cr = 1'b0;
						green_cr = 1'b0;
						arrow_cr = 1'b0;
					end
			S3:	begin
						//Control Main Street Lights
						red_mn = 1'b0;
						yellow_mn = 1'b1;
						green_mn = 1'b0;
						arrow_mn = 1'b0;
						
						//Control Cross Street Lights
						red_cr = 1'b1;
						yellow_cr = 1'b0;
						green_cr = 1'b0;
						arrow_cr = 1'b0;
					end
			S4:	begin
						//Control Main Street Lights
						red_mn = 1'b1;
						yellow_mn = 1'b0;
						green_mn = 1'b0;
						arrow_mn = 1'b0;
						
						//Control Cross Street Lights
						red_cr = 1'b1;
						yellow_cr = 1'b0;
						green_cr = 1'b0;
						arrow_cr = 1'b0;
					end
			S5:	begin
						//Control Main Street Lights
						red_mn = 1'b1;
						yellow_mn = 1'b0;
						green_mn = 1'b0;
						arrow_mn = 1'b0;
						
						//Control Cross Street Lights
						red_cr = 1'b0;
						yellow_cr = 1'b0;
						green_cr = 1'b1;
						arrow_cr = 1'b0;
					end
			S6:	begin
						//Control Main Street Lights
						red_mn = 1'b1;
						yellow_mn = 1'b0;
						green_mn = 1'b0;
						arrow_mn = 1'b0;
						
						//Control Cross Street Lights
						red_cr = 1'b1;
						yellow_cr = 1'b0;
						green_cr = 1'b0;
						arrow_cr = 1'b0;
					end
			S7:	begin
						//Control Main Street Lights
						red_mn = 1'b1;
						yellow_mn = 1'b0;
						green_mn = 1'b0;
						arrow_mn = 1'b0;
						
						//Control Cross Street Lights
						red_cr = 1'b1;
						yellow_cr = 1'b0;
						green_cr = 1'b0;
						arrow_cr = 1'b1;
					end
			S8:	begin
						//Control Main Street Lights
						red_mn = 1'b1;
						yellow_mn = 1'b0;
						green_mn = 1'b0;
						arrow_mn = 1'b0;
						
						//Control Cross Street Lights
						red_cr = 1'b0;
						yellow_cr = 1'b1;
						green_cr = 1'b0;
						arrow_cr = 1'b0;
					end
			S9:	begin
						//Control Main Street Lights
						red_mn = 1'b1;
						yellow_mn = 1'b0;
						green_mn = 1'b0;
						arrow_mn = 1'b0;
						
						//Control Cross Street Lights
						red_cr = 1'b1;
						yellow_cr = 1'b0;
						green_cr = 1'b0;
						arrow_cr = 1'b0;
					end
			S10:	begin
						//Control Main Street Lights
						red_mn = 1'b1;
						yellow_mn = 1'b0;
						green_mn = 1'b0;
						arrow_mn = 1'b1;
						
						//Control Cross Street Lights
						red_cr = 1'b1;
						yellow_cr = 1'b0;
						green_cr = 1'b0;
						arrow_cr = 1'b0;
					end
			default: //All lights red by default
					begin
						//Control Main Street Lights
						red_mn = 1'b1;
						yellow_mn = 1'b0;
						green_mn = 1'b0;
						arrow_mn = 1'b0;
						
						//Control Cross Street Lights
						red_cr = 1'b1;
						yellow_cr = 1'b0;
						green_cr = 1'b0;
						arrow_cr = 1'b0;
					end
		endcase
	end
	
endmodule
