module real_extern_adder(
    input logic[7:0] data,
    output logic[7:0] res
);
    assign res = data + 8'd1;
endmodule
