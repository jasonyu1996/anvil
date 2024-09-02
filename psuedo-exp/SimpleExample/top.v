module example(
    input wire clk,
    input wire a,
    output reg b,
    output reg c
);

reg temp;

always @(posedge clk) begin
        temp = a;  // Blocking assignment
        b <= temp; // Non-blocking assignment
        c <= b;    // Non-blocking assignment
end

endmodule
