/*
 * Title: Creating Behavioral Module and Test Bench
 * This is a behavioral model design for an eight bit counter.
 *
 * Authors: Steven Jennings, Scott Allen
 * Date: 15 March 2017
 * Copyright (C) 2017 Steven Jennings
 */

module eight_bit_counter(count_out, enable, clock, clear);
	// I/O declaration
	output [7:0] count_out;
	reg [7:0] temp;
	input enable, clock, clear;
	wire w6, w5, w4, w3, w2, w1, w0;
	
	assign w0 = count_out[0] & enable;
	assign w1 = count_out[1] & w0;
	assign w2 = count_out[2] & w1;
	assign w3 = count_out[3] & w2;
	assign w4 = count_out[4] & w3;
	assign w5 = count_out[5] & w4;
	assign w6 = count_out[6] & w5;
	
	// Instantiate T Flip Flops
	t_ff t0(count_out[0], enable, clock, clear);
	t_ff t1(count_out[1], w0, clock, clear);
	t_ff t2(count_out[2], w1, clock, clear);
	t_ff t3(count_out[3], w2, clock, clear);
	t_ff t4(count_out[4], w3, clock, clear);
	t_ff t5(count_out[5], w4, clock, clear);
	t_ff t6(count_out[6], w5, clock, clear);
	t_ff t7(count_out[7], w6, clock, clear);

endmodule

module t_ff(q_out, t_in, clock, clear);
	// I/O declaration
	output reg q_out;
	input t_in, clock, clear;
	
	// Initialize q = 0
	initial begin
		q_out = 1'b0;
	end
	
	// Set output to 0 if clear = 1
	// Toggle output if T = 1
	always@(posedge clock, negedge clear) begin
		if (~clear) begin
			q_out <= 1'b0;
		end
		else if (t_in == 1) begin
			q_out <= q_out ^ t_in;
		end
	end
endmodule

/*module stimulus_counter;
	reg clear, enable, clock;
	wire [7:0] count_out;
	
	//Instantiate the eight-bit synchronous counter
	eight_bit_counter s1(count_out, enable, clock, clear);
	
	initial begin
		$monitor("At time ", $time, " clear = %b, and count = %h\n", clear, enable, count_out);
		#2700 $finish;
	end
	
	//Initialize the inputs clear and enable
	initial begin
		clear = 0;
		enable = 0;
		#10 clear = 1;
		#20 enable = 1;
		#2600 clear = 0;
	end
	
	//Create clock
	initial begin
		clock = 1'b0;
	end
	
	always begin
		#5 clock = ~clock;
	end
endmodule*/
