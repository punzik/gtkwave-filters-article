`timescale 1ps/1ps

module sequence_sample;
    logic clock = 1'b0;
    initial forever #(10ns/2) clock = ~clock;

    /* verilator lint_off UNUSEDSIGNAL */
    logic reset;
    logic arvalid;
    logic arready;
    logic [31:0] araddr;
    logic [31:0] araddr_random = '0;
    logic [7:0] arlen;
    logic [2:0] arsize;
    logic bad_arlen;
    logic [3:0] arid;
    logic araddr_valid;
    logic done;

    logic [31:0] rdata;
    logic [31:0] rdata_random = '0;
    logic [3:0] rid;
    logic [3:0] rid_cnt = '0;
    logic rvalid;
    logic rready;
    logic rdata_switch;
    /* verilator lint_on UNUSEDSIGNAL */

    seq #("--__________________________________________________") seq_reset(clock, reset);
    seq #("____-______----______----____--______---______--____") seq_arvalid(clock, arvalid);
    seq #("__---_________-_______________-_______xxxx_____-____") seq_arready(clock, arready);
    seq #("_--------------------------------------------_______") seq_araddrv(clock, araddr_valid);
    seq #("_____________________________--_____________________") seq_arlen(clock, bad_arlen);

    seq #("_________________-__________-_______________________") seq_rvaild(clock, rvalid);
    seq #("________________-__________-________________________") seq_rswitch(clock, rdata_switch);


    seq #("___________________________________________________-") seq_done(clock, done);

    always_ff @(posedge arvalid) begin
        araddr_random <= $urandom;
        rdata_random <= $urandom;
    end

    assign araddr = araddr_valid ? (bad_arlen ? 32'h36f8fe20 : araddr_random) : 32'bx;
    assign arlen = bad_arlen ? 8'd255 : 8'd1;
    assign arsize = 3'b010;
    assign rready = 1'b1;

    initial arid = '0;
    always_ff @(negedge arvalid)
        arid <= arid + 1'b1;

    always_ff @(posedge clock)
      if (rdata_switch) begin
          rdata <= rdata_random;
          rid_cnt <= rid_cnt + 1'b1;

          case (rid_cnt)
            'd0: rid <= 'd1;
            'd1: rid <= 'd0;
            default: rid <= 'x;
          endcase
      end

    /* FSM */
    enum int unsigned {
        ST_IDLE = 0,
        ST_CHECK_ADDR,
        ST_RECEIVE_DATA,
        ST_SEND_ACK,
        ST_DONE
    } state;

    logic fsm_transition;
    always_ff @(posedge clock)
      fsm_transition <= &(2'($urandom));

    always_ff @(posedge clock)
      if (reset)
        state <= ST_IDLE;
      else
        case (state)
          ST_IDLE:         if (fsm_transition) state <= ST_CHECK_ADDR;
          ST_CHECK_ADDR:   if (fsm_transition) state <= ST_RECEIVE_DATA;
          ST_RECEIVE_DATA: if (fsm_transition) state <= ST_SEND_ACK;
          ST_SEND_ACK:     if (fsm_transition) state <= ST_DONE;
          ST_DONE:         if (fsm_transition) state <= ST_IDLE;
        endcase

    /* ANALOG */
    logic signed [7:0] signal;
    logic signal_dir;

    seq #("--------____---_______----__-----______") seq_sig_dir(clock, signal_dir);

    always_ff @(posedge clock)
      if (reset) signal <= '0;
      else       signal <= signal_dir ? signal + 1'b1 : signal - 1'b1;

    initial begin
        wait(done);
        @(posedge clock)
        $finish;
    end

    initial begin
        $dumpfile("sequence_sample.vcd");
        $dumpvars(0, sequence_sample);
    end
endmodule // sequence_sample

/* verilator lint_off DECLFILENAME */
module seq #(parameter string SEQUENCE = "")
    (input wire clock,
     output reg out);

    int l, n;
    string s;

    function automatic logic conv(logic [7:0] ch);
        case (ch)
          "-": conv = 1'b1;
          "_": conv = 1'b0;
          "z": conv = 1'bz;
          default: conv = 1'bx;
        endcase
    endfunction

    initial begin
        s = SEQUENCE;
        l = s.len();
        if (l == 0) $error("Sequence is empty");

        out = conv(s[0]);
        n = (n == l-1) ? 0 : 1;
    end

    always_ff @(posedge clock) begin
        n <= (n == l-1) ? 0 : n + 1;
        out <= conv(s[n]);
    end
endmodule // seq
