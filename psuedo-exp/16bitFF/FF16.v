module FF16( input clk,
            input wr,
            input sel,
            input [15:0] wdata,
            input [15:0] rdata);
        reg [15:0] register;
        always @(posedge clk) begin
                if (sel & wr)
                register <= wdata;
                else
                register <= register;
        end

        assign rdata = (sel & ~wr) ? register: 0;

endmodule