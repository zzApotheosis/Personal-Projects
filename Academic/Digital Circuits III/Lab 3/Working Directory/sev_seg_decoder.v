/*
 * Title: Seven Segment Decoder
 * This is the design for a seven segment decoder
 * 
 * Authors: Steven Jennings, Scott Allen
 * Date: 9 March 2017
 * Copyright (C) 2017 Steven Jennings
 */
 
module sev_seg_decoder(led_out, bin_in);
	//I/O & wiring
	output [6:0] led_out;
	input [3:0] bin_in;
		
	//led_out[0] = a'b'c'd + a'bc'd' + ab'cd + abc'd
	assign led_out[0] = (~bin_in[3] & ~bin_in[2] & ~bin_in[1] & bin_in[0]) | (~bin_in[3] & bin_in[2] & ~bin_in[1] & ~bin_in[0])
	| (bin_in[3] & ~bin_in[2] & bin_in[1] & bin_in[0]) | (bin_in[3] & bin_in[2] & ~bin_in[1] & bin_in[0]);
	
	//led_out[1] = bcd' + acd + abd' + a'bc'd
	assign led_out[1] = (bin_in[2] & bin_in[1] & ~bin_in[0]) | (bin_in[3] & bin_in[1] & bin_in[0]) | (bin_in[3] & bin_in[2] & ~bin_in[0])
	| (~bin_in[3] & bin_in[2] & ~bin_in[1] & bin_in[0]);
	
	//led_out[2] = abd' + abc + a'b'cd'
	assign led_out[2] = (bin_in[3] & bin_in[2] & ~bin_in[0]) | (bin_in[3] & bin_in[2] & bin_in[1])
	| (~bin_in[3] & ~bin_in[2] & bin_in[1] & ~bin_in[0]);
	
	//led_out[3] = b'c'd + bcd + a'bc'd' + ab'cd'
	assign led_out[3] = (~bin_in[2] & ~bin_in[1] & bin_in[0]) | (bin_in[2] & bin_in[1] & bin_in[0])
	| (~bin_in[3] & bin_in[2] & ~bin_in[1] & ~bin_in[0]) | (bin_in[3] & ~bin_in[2] & bin_in[1] & ~bin_in[0]);
	
	//led_out[4] = a'd + b'c'd + a'bc'
	assign led_out[4] = (~bin_in[3] & bin_in[0]) | (~bin_in[2] & ~bin_in[1] & bin_in[0]) | (~bin_in[3] & bin_in[2] & ~bin_in[1]);
	
	//led_out[5] = a'b'd + a'b'c + a'cd +abc'd
	assign led_out[5] = (~bin_in[3] & ~bin_in[2] & bin_in[0]) | (~bin_in[3] & ~bin_in[2] & bin_in[1])
	| (~bin_in[3] & bin_in[1] & bin_in[0]) | (bin_in[3] & bin_in[2] & ~bin_in[1] & bin_in[0]);
	
	//led_out[6] = a'b'c' + a'bcd + abc'd'
	assign led_out[6] = (~bin_in[3] & ~bin_in[2] & ~bin_in[1]) | (~bin_in[3] & bin_in[2] & bin_in[1] & bin_in[0])
	| (bin_in[3] & bin_in[2] & ~bin_in[1] & ~bin_in[0]);
endmodule

module stimulus_seven_seg;
	reg[3:0] bin_in = 0000;
	wire[6:0] led_out;
	reg clk;
	
	//Instantiating the seven segment decoder
	sev_seg_decoder s1(led_out, bin_in);
	
	initial
		clk = 1'b0;
	always
		#5 clk = ~clk; //Toggle clk every 5 time units
	
	always @(posedge clk)
		bin_in = bin_in + 1;
	
	initial
	begin
		$monitor("At time ", $time, " binary input = %b and hex output = %h\n", bin_in, led_out);
			#160 $stop;
	end
endmodule
