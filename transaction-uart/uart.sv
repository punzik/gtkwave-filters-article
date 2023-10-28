`timescale 1ps/1ps

module uart;
    logic tx0, tx1;
    logic start0, start1;

    uart_tx #("FPGA", 1000, 1000) tx0_seq (start0, tx0);
    uart_tx #("SYSTEMS", 630, 1400) tx1_seq (start1, tx1);

    initial begin
        start0 <= 1'b0;
        start1 <= 1'b0;

        #3456;
        start0 <= 1'b1;

        #5678;
        start1 <= 1'b1;

        repeat(55) #1000;
        $finish;
    end

    initial begin
        $dumpfile("uart.vcd");
        $dumpvars(0, uart);
    end
endmodule

/* verilator lint_off DECLFILENAME */
module uart_tx #(parameter TEXT = "HELLO",
                 parameter BIT_LEN = 1000,
                 parameter CHAR_INTERVAL = BIT_LEN)
    (input logic start,
     output logic tx);

    initial begin
        logic [7:0] char;
        string s;

        tx = 1'b1;
        wait(start);

        s = TEXT;
        for (int n = 0; n < s.len(); n += 1) begin
            char = s[n];

            tx = 1'b0; #(BIT_LEN);  // Start bit
            for (int b = 0; b < 8; b += 1) begin
                tx = char[b]; #(BIT_LEN);
            end
            tx = 1'b1; #(BIT_LEN);  // Stop bit
            #(CHAR_INTERVAL);
        end
    end
endmodule
