module TwoStateMachines (
    input wire clk,        
    input wire rst,        
    input wire start,      
    input wire [7:0] in1, 
    input wire [7:0] in2, 
    output reg [7:0] shared_reg, 
    output reg done1,      
    output reg done2
);

    // State encoding for Thread 1 and Thread 2
    typedef enum reg [1:0] {
        IDLE = 2'b00,
        READ = 2'b01,
        PROCESS = 2'b10,
        WRITE = 2'b11
    } state_t;

    // Registers for Thread 1
    reg [1:0] state_thread1, next_state_thread1;
    reg [7:0] thread1_data_buffer; 

    // Registers for Thread 2
    reg [1:0] state_thread2, next_state_thread2;
    reg [7:0] thread2_data_buffer; 

    // Shared register hazard flag (for debugging potential lifetime violations)
    reg hazard_detected;

    // Thread 1 State Machine
    always @(posedge clk or posedge rst) begin
        if (rst) begin
            state_thread1 <= IDLE;
            done1 <= 0;
            thread1_data_buffer <= 8'd0;
        end else begin
            state_thread1 <= next_state_thread1;
        end
    end

    // Thread 1 Next State Logic and Data Operations
    always @(*) begin
        case (state_thread1)
            IDLE: begin
                if (start) next_state_thread1 = READ;
                else next_state_thread1 = IDLE;
            end
            READ: begin
                next_state_thread1 = PROCESS;
            end
            PROCESS: begin
                thread1_data_buffer = shared_reg + data_in1; 
                next_state_thread1 = WRITE;
            end
            WRITE: begin
                if (shared_reg != 8'd0) begin
                    hazard_detected = 1; 
                end
                shared_reg = thread1_data_buffer;  
                done1 = 1;
                next_state_thread1 = IDLE;
            end
        endcase
    end

    // Thread 2 State Machine
    always @(posedge clk or posedge rst) begin
        if (rst) begin
            state_thread2 <= IDLE;
            done2 <= 0;
            thread2_data_buffer <= 8'd0;
        end else begin
            state_thread2 <= next_state_thread2;
        end
    end

    // Thread 2 Next State Logic and Data Operations
    always @(*) begin
        case (state_thread2)
            IDLE: begin
                if (start) next_state_thread2 = READ;
                else next_state_thread2 = IDLE;
            end
            READ: begin
                next_state_thread2 = PROCESS;
            end
            PROCESS: begin
                thread2_data_buffer = shared_reg + data_in2; 
                next_state_thread2 = WRITE;
            end
            WRITE: begin
                if (shared_reg != 8'd0) begin
                    hazard_detected = 1; 
                end
                shared_reg = thread2_data_buffer;  
                done2 = 1;
                next_state_thread2 = IDLE;
            end
        endcase
    end

    always @(posedge clk) begin
        if (state_thread1 == WRITE && state_thread2 == WRITE) begin
            hazard_detected = 1; 
        end else begin
            hazard_detected = 0;
        end
    end

endmodule
