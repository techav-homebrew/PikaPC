/*
 * PikaPC Host Card Bus Control
 * techav
 * 2026/03/14
 *
 */

module HostBus (
    input   wire            busClk,         // main bus clock
    input   wire            busRESETn,      // main reset
    input   wire            cpuRWn,         // cpu read/write signal
    input   wire [3:0]      cpuWBEn,        // cpu write byte enable
    input   wire [7:0]      cpuCSn,         // cpu chip select signals
    output  wire            cpuREADY,       // cpu bus cycle ready signal
    output  wire            cpuNMIn,        // cpu non-maskable interrupt
    output  wire [4:0]      cpuIRQn,        // cpu interrupt requests

    input   wire [2:0]      busIRQn,        // bus interrupt request
    input   wire            busACKn,        // bus cycle acknowledge
    output  reg             busSTARTn,      // bus cycle start
    output  reg  [3:0]      busBEn,         // bus byte enables
    output  wire [3:1]      busXA,          // bus X Address
    output  reg             busAENn,        // buss address enable
    output  reg             busBDIR,        // bus data direction
    output  reg  [2:0]      busBEN8n,       // 8-bit bus enables
    output  reg             busBEN16n,      // 16-bit bus enable
    output  reg             busBEN32n,      // 32-bit bus enable

    input   wire            busREQn,        // bus request
    output  wire            busGNTn         // bus grant
);

/*****************************************************************************/
// main bus cycle state machine

parameter
    sIDLE   =   0,  // idle state
    sADDR   =   1,  // bus address output
    sWAIT   =   2,  // bus data / wait for ACK
    sTERM   =   3,  // terminate bus cyle
    sCEND   =   4;  // bus cycle end
reg [2:0] busState;
wire [2:0] busNext;
wire activeCycle;

always_comb begin
    if(!cpuCSn[2] || !cpuCSn[3] || ! cpuCSn[4] || !cpuCSn[5] 
        || !cpuCSn[6] || !cpuCSn[7] || !cpuCSn[1])
    begin
        activeCycle = 1'b1;
    end else begin
        activeCycle = 1'b0;
    end
end

always_comb begin
    case(busState)
        sIDLE: begin
            // ignore CS0 for now
            if(!cpuCSn[2] || !cpuCSn[3] || ! cpuCSn[4] || !cpuCSn[5] 
                || !cpuCSn[6] || !cpuCSn[7] || !cpuCSn[1])
            begin
                busNext = sADDR;
            end else begin
                busNext = sIDLE;
            end
        end
        sADDR: begin
            busNext = sWAIT;
        end
        sWAIT: begin
            if(busACKn) busNext = sWAIT;
            else busNext = sTERM;
        end
        sTERM: begin
            busNext = sCEND;
        end
        sCEND: begin
            if(cpuCSn == 8'b11111111) busNext = sIDLE;
            else busNext = sCEND;
        end
        default: begin
            busNext = sIDLE;
        end
    endcase
end

always @(negedge busClk or negedge busRESETn) begin
    if(!busRESETn) busState <= sIDLE;
    else if(!activeCycle) busState <= sIDLE;
    else busState <= busNext;
end

/*****************************************************************************/
// main bus cycle control signals

// START
/*always @(negedge busClk or negedge busRESETn) begin
    if(!busRESETn) busSTARTn <= 1'b1;*/
always @(negedge busClk or negedge activeCycle) begin
    if(!activeCycle) busSTARTn <= 1'b1;
    else begin
        if(busNext == sADDR || busNext == sWAIT 
            || busNext == sTERM || busNext == sCEND)
        begin
            busSTARTn <= 1'b0;
        end else begin
            busSTARTn <= 1'b1;
        end
    end
end

// AEN
always @(negedge busClk or negedge busRESETn) begin
    if(!busRESETn) busAENn <= 1'b1;
    else begin
        if(busNext == sADDR) busAENn <= 1'b0;
        else busAENn <= 1'b1;
    end
end

// data bus enable
always @(negedge busClk or negedge busRESETn) begin
    if(!busRESETn) begin
        busBEN32n <= 1'b1;
        busBEN16n <= 1'b1;
        busBEN8n  <= 3'b111;
    end else begin
        if(busNext == sWAIT || busNext == sCEND || busNext == sTERM) begin
            if (!cpuCSn[2] || !cpuCSn[6] || !cpuCSn[7] || !cpuCSn[1]) begin
                // 32-bit cycle
                busBEN32n <= 1'b0;
                busBEN16n <= 1'b1;
                busBEN8n  <= 3'b111;
            end else if(!cpuCSn[4]) begin
                // 16-bit cycle
                if(cpuWBEn[3]) begin
                    busBEN32n <= 1'b1;
                    busBEN16n <= 1'b0;
                    busBEN8n  <= 3'b101;
                end else begin
                    busBEN32n <= 1'b0;
                    busBEN16n <= 1'b1;
                    busBEN8n  <= 3'b111;
                end
            end else if(!cpuCSn[3] || !cpuCSn[5]) begin
                // 8-bit cycle (remember 3 is LSB because IBM!)
                case (cpuWBEn[3:2])
                    2'b11: begin
                        busBEN32n <= 1'b1;
                        busBEN16n <= 1'b1;
                        busBEN8n  <= 3'b110;
                    end
                    2'b01: begin
                        busBEN32n <= 1'b1;
                        busBEN16n <= 1'b1;
                        busBEN8n  <= 3'b101;
                    end
                    2'b10: begin
                        busBEN32n <= 1'b1;
                        busBEN16n <= 1'b1;
                        busBEN8n  <= 3'b011;
                    end
                    2'b00: begin
                        busBEN32n <= 1'b0;
                        busBEN16n <= 1'b1;
                        busBEN8n  <= 3'b111;
                    end
                endcase
            end else begin
                // ignored cycle (we really shouldn't end up here)
                busBEN32n <= 1'b1;
                busBEN16n <= 1'b1;
                busBEN8n  <= 3'b111;
            end
        end else begin
            busBEN32n <= 1'b1;
            busBEN16n <= 1'b1;
            busBEN8n  <= 3'b111;
        end
    end
end

// bus byte enable (this is the hard one ...)
always @(negedge busClk or negedge busRESETn) begin
    if(!busRESETn) begin
        busBEn <= 4'b1111;
    end else begin
        if(busNext == sADDR || busNext == sWAIT || busNext == sTERM) begin
            if(!cpuCSn[2] || !cpuCSn[6] || !cpuCSn[7] || !cpuCSn[1]) begin
                // 32-bit cycle
                if(cpuRWn) begin
                    busBEn <= 4'b0000;
                end else begin
                    busBEn[0] <= cpuWBEn[3];
                    busBEn[1] <= cpuWBEn[2];
                    busBEn[2] <= cpuWBEn[1];
                    busBEn[3] <= cpuWBEn[0];
                end
            end else if(!cpuCSn[4]) begin
                // 16-bit cycle
                if(cpuRWn) begin
                    if(cpuWBEn[3]) begin
                        busBEn <= 4'b0011;
                    end else begin
                        busBEn <= 4'b1100;
                    end
                end else begin
                    if(cpuWBEn[3]) begin
                        busBEn[1:0] <= 2'b11;
                        busBEn[2] <= cpuWBEn[1];
                        busBEn[3] <= cpuWBEn[0];
                    end else begin
                        busBEn[0] <= cpuWBEn[1];
                        busBEn[1] <= cpuWBEn[0];
                        busBEn[3:2] <= 2'b11;
                    end
                end
            end else if(!cpuCSn[3] || !cpuCSn[5]) begin
                // 8-bit cycle (remember 3 is LSB because IBM!)
                case (cpuWBEn[3:2])
                    2'b11: begin
                        busBEn <= 4'b1110;
                    end
                    2'b01: begin
                        busBEn <= 4'b1101;
                    end
                    2'b10: begin
                        busBEn <= 4'b1011;
                    end
                    2'b00: begin
                        busBEn <= 4'b0111;
                    end
                endcase
            end else begin
                // ignored cycle (we really shouldn't end up here)
                busBEn <= 4'b1111;
            end
        end else begin
            busBEn <= 4'b1111;
        end
    end
end

// bus direction
//assign busBDIR = !cpuRWn;
always @(negedge busClk or negedge busRESETn) begin
    if(!busRESETn) busBDIR <= 1'b1;
    else begin
        if(busNext != sIDLE) busBDIR <= !cpuRWn;
        else busBDIR <= 1'b1;
    end
end


// cpu Ready
assign cpuREADY = !busACKn;

// bus X address
assign busXA[1] = (!cpuCSn[0] || !cpuCSn[1] || !cpuCSn[2] || !cpuCSn[3]);
assign busXA[2] = (!cpuCSn[0] || !cpuCSn[1] || !cpuCSn[4] || !cpuCSn[5]);
assign busXA[3] = (!cpuCSn[0] || !cpuCSn[2] || !cpuCSn[4] || !cpuCSn[6]);

// interrupt requests
always_comb begin
    cpuNMIn = 1'b1;
    cpuIRQn = 5'b11111;
    case(busIRQn)
        3'b000: cpuNMIn = 1'b0;
        3'b110: cpuIRQn[0] = 1'b0;
        3'b101: cpuIRQn[1] = 1'b0;
        3'b100: cpuIRQn[2] = 1'b0;
        3'b011: cpuIRQn[3] = 1'b0;
        3'b010: cpuIRQn[4] = 1'b0;
    endcase
end


endmodule