module RCA (
    input [3:0] a,    
    input [3:0] b,    
    input cin,        
    output [3:0] sum, 
    output cout       
);
    wire c1, c2, c3;  

    // Full adder for bit 0
    full_adder fa0 (
        .a(a[0]),
        .b(b[0]),
        .cin(cin),
        .sum(sum[0]),
        .cout(c1)
    );

    // Full adder for bit 1
    full_adder fa1 (
        .a(a[1]),
        .b(b[1]),
        .cin(c1),
        .sum(sum[1]),
        .cout(c2)
    );

    // Full adder for bit 2
    full_adder fa2 (
        .a(a[2]),
        .b(b[2]),
        .cin(c2),
        .sum(sum[2]),
        .cout(c3)
    );

    // Full adder for bit 3
    full_adder fa3 (
        .a(a[3]),
        .b(b[3]),
        .cin(c3),
        .sum(sum[3]),
        .cout(cout)
    );

endmodule

// Full adder module
module full_adder (
    input a, b, cin,  
    output sum, cout  
);
    assign #5 sum = a ^ b ^ cin;  // Sum with a delay of 5 units
    assign #5 cout = (a & b) | (b & cin) | (a & cin); // Carry out with a delay of 5 units
endmodule
