module fifo_circular_queue #(
    parameter DEPTH = 8,
    parameter WIDTH = 8
) (
    input wire clk,
    input wire rst_n,
    input wire [WIDTH-1:0] data_in,
    input wire wr_en,
    input wire rd_en,
    output reg [WIDTH-1:0] data_out,
    output reg empty,
    output reg full
);

    // Internal signals
    reg [WIDTH-1:0] queue [0:DEPTH-1];
    reg [$clog2(DEPTH):0] rd_ptr, wr_ptr;
    reg [$clog2(DEPTH):0] count;

    // Queue operations
    always @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            rd_ptr <= 0;
            wr_ptr <= 0;
            count <= 0;
            empty <= 1'b1;
            full <= 1'b0;
        end else begin
            if (wr_en && !full) begin
                queue[wr_ptr[2:0]] <= data_in;
                wr_ptr <= wr_ptr + 1;
                count <= count + 1;
            end
            if (rd_en && !empty) begin
                data_out <= queue[rd_ptr[2:0]];
                rd_ptr <= rd_ptr + 1;
                count <= count - 1;
            end
            empty <= (count == 0);
            full <= (count == DEPTH);
        end
    end

endmodule


module fifo_circular_queue_tb;

    parameter DEPTH = 8;
    parameter WIDTH = 8;
    parameter CLK_PERIOD = 10;

    
    reg clk;
    reg rst_n;
    reg [WIDTH-1:0] data_in;
    reg wr_en;
    reg rd_en;
    wire [WIDTH-1:0] data_out;
    wire empty;
    wire full;

    fifo_circular_queue #(
        .DEPTH(DEPTH),
        .WIDTH(WIDTH)
    ) uut (
        .clk(clk),
        .rst_n(rst_n),
        .data_in(data_in),
        .wr_en(wr_en),
        .rd_en(rd_en),
        .data_out(data_out),
        .empty(empty),
        .full(full)
    );

    always #((CLK_PERIOD)/2) clk = ~clk;

    
    initial begin
        
        clk = 0;
        rst_n = 0;
        data_in = 0;
        wr_en = 0;
        rd_en = 0;

        
        #(CLK_PERIOD*2);
        rst_n = 1;
        #(CLK_PERIOD);

        repeat (DEPTH) begin
            @(posedge clk);
            data_in = $random;
            wr_en = 1;
            #1;
            if (full) $display("Error: Queue became full before expected");
        end
        wr_en = 0;
        if (!full) $display("Error: Queue should be full");

        //Read until empty
        repeat (DEPTH) begin
            @(posedge clk);
            rd_en = 1;
            #1;
            if (empty) $display("Error: Queue became empty before expected");
        end
        rd_en = 0;
        if (!empty) $display("Error: Queue should be empty");

        // Test case 3: Alternating read and write
        repeat (20) begin
            @(posedge clk);
            data_in = $random;
            wr_en = 1;
            rd_en = 0;
            #(CLK_PERIOD);
            wr_en = 0;
            rd_en = 1;
        end

        // End simulation
        #(CLK_PERIOD*5);
        $display("Simulation completed");
        $finish;
    end

    initial begin
        $dumpfile("fifo_circular_queue_tb.vcd");
        $dumpvars(0, fifo_circular_queue_tb);
    end

endmodule

