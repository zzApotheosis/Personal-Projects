library verilog;
use verilog.vl_types.all;
entity sev_seg_decoder is
    port(
        led_out         : out    vl_logic_vector(6 downto 0);
        bin_in          : in     vl_logic_vector(3 downto 0)
    );
end sev_seg_decoder;
