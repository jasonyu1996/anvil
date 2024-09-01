module FF16( input clk,
            input rstn,
            input wr,
            input [15:0] wdata,
            input [15:0] rdata);
        reg [15:0] register;
        always @(posedge clk) begin
            if(!rstn)
            register <= 0;
            else begin
                if (sel & wr)
                register <= wdata;
                else
                register <= register;
            end
        end

        assign rdata = (sel & ~wr) ? register: 0;

endmodule