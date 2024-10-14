module pipelined_processor_8bit (
    input wire clk,
    input wire reset,
    input wire [7:0] instruction,
    output wire [7:0] result
);

// Pipeline registers
reg [7:0] IF_ID_instruction;
reg [7:0] ID_EX_operand_a, ID_EX_operand_b;
reg [2:0] ID_EX_opcode;
reg [7:0] EX_MEM_result;
reg [7:0] MEM_WB_result;

// Instruction Fetch (IF) stage
always @(posedge clk or posedge reset) begin
    if (reset)
        IF_ID_instruction <= 8'b0;
    else
        IF_ID_instruction <= instruction;
end

// Instruction Decode (ID) stage
always @(posedge clk or posedge reset) begin
    if (reset) begin
        ID_EX_operand_a <= 8'b0;
        ID_EX_operand_b <= 8'b0;
        ID_EX_opcode <= 3'b0;
    end else begin
        ID_EX_operand_a <= IF_ID_instruction[7:5];
        ID_EX_operand_b <= IF_ID_instruction[4:2];
        ID_EX_opcode <= IF_ID_instruction[1:0];
    end
end

// Execute (EX) stage
always @(posedge clk or posedge reset) begin
    if (reset)
        EX_MEM_result <= 8'b0;
    else begin
        case (ID_EX_opcode)
            2'b00: EX_MEM_result <= ID_EX_operand_a + ID_EX_operand_b; // ADD
            2'b01: EX_MEM_result <= ID_EX_operand_a - ID_EX_operand_b; // SUB
            2'b10: EX_MEM_result <= ID_EX_operand_a & ID_EX_operand_b; // AND
            2'b11: EX_MEM_result <= ID_EX_operand_a | ID_EX_operand_b; // OR
            default: EX_MEM_result <= 8'b0;
        endcase
    end
end

// Memory (MEM) stage
always @(posedge clk or posedge reset) begin
    if (reset)
        MEM_WB_result <= 8'b0;
    else
        MEM_WB_result <= EX_MEM_result;
end

assign result = MEM_WB_result;

endmodule

module pipeline_tb;

    reg clk;
    reg reset;
    reg [7:0] instruction;
    wire [7:0] result;
    
    pipelined_processor_8bit uut (
        .clk(clk),
        .reset(reset),
        .instruction(instruction),
        .result(result)
    );

    always #5 clk = ~clk;

    reg [31:0] cycle_count;
    always @(posedge clk or posedge reset) begin
        if (reset)
            cycle_count <= 0;
        else
            cycle_count <= cycle_count + 1;
    end

    initial begin
        clk = 0;
        reset = 1;
        instruction = 8'b0;
        
        #10 reset = 0;
        $dumpfile("pipeline.vcd");
        $dumpvars(0, pipeline_tb);
        
        #10 instruction = 8'b00100100; // ADD 1, 1
        #10 instruction = 8'b01000100; // ADD 2, 1
        #10 instruction = 8'b01101000; // ADD 3, 2
        #10 instruction = 8'b10001100; // ADD 4, 3
        
        #50;
        
        $finish;
    end

    always @(posedge clk) begin
        if (cycle_count >= 5 && cycle_count <= 9) begin
            $display("Cycle %d: Result = %d", cycle_count, result);
        end
    end

endmodule





