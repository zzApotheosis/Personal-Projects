transcript on
if {[file exists rtl_work]} {
	vdel -lib rtl_work -all
}
vlib rtl_work
vmap work rtl_work

vlog -vlog01compat -work work +incdir+E:/Digital\ Circuist\ III/Lab\ 2 {E:/Digital Circuist III/Lab 2/two_by_one_mux.v}

