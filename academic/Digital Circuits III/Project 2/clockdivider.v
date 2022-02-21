/*
 * Title: Clock Divider
 * This transforms a 50MHz clock to a 20Hz clock.
 * 
 * Author: Steven Jennings
 * Date: 27 April 2017
 * Copyright 2017 Steven Jennings
 */

module clockdivider(out, in);
	// Define I/O
	output reg out;
	input in;
	
	// Define count variable to handle clock division
	integer count;
	
	// Initialize variables
	initial begin
		count <= 0;
		out <= 0;
	end
	
	always@(posedge in) begin
		if (count >= 1250000) begin
			out <= ~out; // Toggle output.
			count <= 0;
		end else begin
			count <= count + 1;
		end
	end
endmodule