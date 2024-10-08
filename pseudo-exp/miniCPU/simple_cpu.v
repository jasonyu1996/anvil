// src : https://www.fpgajobs.com/blog/simple-cpu-under-100-lines-of-verilog/ with slight modifications

// SUB module definition
module sub(
    input [7:0] a,
    input [7:0] b,
    output [7:0] result
);
    assign result = a - b;
endmodule
// ADD module definition
module add(
    input [7:0] a,
    input [7:0] b,
    output [7:0] result
);
    assign result = a + b;
endmodule




module simple_cpu(
    input clock,
    input reset,
    output reg [7:0] alu_out
);

// Registers 
reg [7:0] register_file [0:1];  // Two 8-bit registers
reg [2:0] state;                // State for FSM control
reg [7:0] program_counter;      // Program Counter

// Program Memory 
reg [7:0] program_memory [255:0];

// Internal buses
wire [7:0] instruction = program_memory[program_counter];
wire [2:0] opcode      = instruction[7:5];
wire       reg_dest    = instruction[5];
wire       reg_src     = instruction[6];
wire [7:0] immediate   = {4'b0000, instruction[3:0]};

// Outputs of sub and add modules
wire [7:0] sub_result;
wire [7:0] add_result;

// Instantiate SUB and ADD modules
sub sub_inst (
    .a(register_file[reg_dest]),
    .b(register_file[reg_src]),
    .result(sub_result)
);

add add_inst (
    .a(register_file[reg_dest]),
    .b(register_file[reg_src]),
    .result(add_result)
);

always @(posedge clock or posedge reset) begin
    if (reset) begin
        program_counter  <= 0;
        register_file[0] <= 0;
        register_file[1] <= 0;
        alu_out          <= 0;
        state            <= 0;
    end else begin
        case (state)
            0: begin // Fetch
                state <= 1;
            end
            1: begin // Decode and Execute
                case (opcode)
                    3'b000: alu_out <= sub_result; // Use result from SUB module
                    3'b001: alu_out <= add_result; // Use result from ADD module
                    3'b010: alu_out <= register_file[reg_dest] & register_file[reg_src]; // AND
                    3'b011: alu_out <= register_file[reg_dest] | register_file[reg_src]; // OR
                    3'b100: alu_out <= register_file[reg_dest] ^ register_file[reg_src]; // XOR
                    3'b101: alu_out <= opcode;                                           // NOP
                    3'b110: alu_out <= immediate;                                        // LOAD0 (Imm)
                    3'b111: alu_out <= immediate;                                        // LOAD1 (Imm)
                    default: alu_out <= alu_out;
                endcase
                state <= 2;
            end
            2: begin // Write Back
                if (opcode >= 3'b110) begin // LOAD 
                    register_file[reg_dest] <= alu_out;
                end
                if (opcode <  3'b101) begin  // Arithmetic + Logic
                    register_file[1'b0] <= alu_out;
                end
                state <= 3;
            end
            3: begin // Increment PC
                program_counter <= program_counter + 1;
                state <= 0;
            end
        endcase
    end
end

endmodule

