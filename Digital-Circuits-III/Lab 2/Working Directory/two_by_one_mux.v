/*
 * Title: Two to One Multiplexer
 * This is a two to one multiplexer written in Verilog.
 * 
 * Authors: Steven Jennings, Scott Allen
 * Date: 2 March 2017
 * Copyright (C) 2017 Steven Jennings
 */

module two_by_one_mux(m_out, x_in, y_in, sel);
	// Declare I/O
	output m_out;
	input x_in, y_in, sel;
	wire selnot, wx, wy;

	// Instantiating primitive Verilog gates
	not(selnot, sel);
	and(wx, x_in, selnot);
	and(wy, y_in, sel);
	or(m_out, wx, wy);

endmodule

/*
// test bench for 2-1 mux
module mux_tb;

reg local_x_in,local_y_in; //declare variables
reg local_sel;

wire local_m_out; //delare output

//Instantiante da mux
two_by_one_mux mymux(local_m_out,local_x_in,local_y_in,local_sel);

initial
begin
	$monitor($time,"x_in=%b,y_in=%b,sel=%b,---m_out=%b\n", local_x_in,local_y_in,local_sel,local_m_out);
end
//stimulate
initial
begin
	local_x_in=0;local_y_in=0;local_sel=0;
	#10 local_x_in=0;local_y_in=0;local_sel=0;
	#10 local_x_in=0;local_y_in=1;local_sel=0;
	#10 local_x_in=1;local_y_in=0;local_sel=0;
	#10 local_x_in=1;local_y_in=1;local_sel=0;
	#10 local_x_in=0;local_y_in=0;local_sel=1;
	#10 local_x_in=0;local_y_in=1;local_sel=1;
	#10 local_x_in=1;local_y_in=0;local_sel=1;
	#10 local_x_in=1;local_y_in=1;local_sel=1;
	end
endmodule
*/

