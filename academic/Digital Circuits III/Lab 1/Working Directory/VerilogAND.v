/*
 * Title: Verilog 2-Input AND Gate
 * This is a 2-input AND Gate made
 * from a Verilog primitive AND gate.
 * 
 * Author: Steven Jennings
 * Date: 27 Feburary 2017
 * Copyright (C) 2017 Steven Jennings
 */

module VerilogAND(Fv, Av, Bv);
	//Declaring Outputs
	output Fv;
	
	//Declaring Inputs
	input Av, Bv;
	
	//No internal wiring needed
	//No extra instantiation needed
	
	//Instantiating Verilog primitive AND gate
	and(Fv, Av, Bv);
endmodule
