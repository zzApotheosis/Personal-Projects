/*
 * Title: Dual Seven Segment Display
 * This module accepts a 4-bit input, and properly converts
 * the input to a 14-bit dual seven segment display.
 *
 * Author: Steven Jennings
 * Date: 27 April 2017
 * Copyright 2017 Steven Jennings
 */
 
module dual_seven_seg(out, in);
	// Define I/O
	output reg [13:0] out; // 14-bit output for two seven segment displays
	input [3:0] in; // 4-bit input for up to 16 possible binary inputs
	
	always begin
		case (in)
			4'b1111 : out <= 14'b10011110100100;
			4'b1110 : out <= 14'b10011111001100;
			4'b1101 : out <= 14'b10011110000110;
			4'b1100 : out <= 14'b10011110010010;
			4'b1011 : out <= 14'b10011111001111;
			4'b1010 : out <= 14'b10011110000001;
			4'b1001 : out <= 14'b11111110000100;
			4'b1000 : out <= 14'b11111110000000;
			4'b0111 : out <= 14'b11111110001111;
			4'b0110 : out <= 14'b11111110100000;
			4'b0101 : out <= 14'b11111110100100;
			4'b0100 : out <= 14'b11111111001100;
			4'b0011 : out <= 14'b11111110000110;
			4'b0010 : out <= 14'b11111110010010;
			4'b0001 : out <= 14'b11111111001111;
			4'b0000 : out <= 14'b11111110000001;
			default : out <= 14'b00000000000000;
		endcase
	end
endmodule