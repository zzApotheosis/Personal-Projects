library verilog;
use verilog.vl_types.all;
entity t_ff is
    port(
        q_out           : out    vl_logic;
        t_in            : in     vl_logic;
        clock           : in     vl_logic;
        clear           : in     vl_logic
    );
end t_ff;
