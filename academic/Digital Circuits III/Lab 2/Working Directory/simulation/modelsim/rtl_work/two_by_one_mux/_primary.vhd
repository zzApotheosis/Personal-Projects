library verilog;
use verilog.vl_types.all;
entity two_by_one_mux is
    port(
        m_out           : out    vl_logic;
        x_in            : in     vl_logic;
        y_in            : in     vl_logic;
        sel             : in     vl_logic
    );
end two_by_one_mux;
