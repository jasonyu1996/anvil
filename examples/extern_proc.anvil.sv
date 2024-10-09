module real_extern_proc(input logic clk_i);
    always_ff @(posedge clk_i) begin
        $display("Hello World!");
    end
endmodule
