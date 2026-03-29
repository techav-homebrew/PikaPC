/*
 * PikaPC SIMM72 Card DRAM Control
 * techav
 * 2026/03/25
 *
 */

module SimmBus (
    input   wire            busClk,         // main bus clock
    input   wire            busRESETn,      // main reset
    input   wire            busRWn,         // bus read/write signal
    input   wire [3:0]      busBEn,         // bus byte enables
    input   wire [24:2]     busAD,          // main address/data bus
    input   wire            busBERRn,       // bus error signal
    input   wire            busBURSTn,      // bus burst request signal
    output  wire            busBRACKn,      // bus burst ack signal
    input   wire            busSTARTn,      // bus cycle start

    inout   wire            busACKn,        // bus cycle acknowledge

    input   wire            cardCEn,        // card enable signal
    output  wire            cardBENn,       // card data buffer enable

    output  reg             memWEn,         // memory write enable
    output  reg  [10:0]     memADDR,        // memory address bus
    output  reg  [3:0]      memCASn,        // memory column address strobe
    output  reg  [1:0]      memRASn,        // memory row address strobe

    output  wire [3:0]      debugState      // state machine state output
);

// internal registers
reg [3:0] busState;             // state machine current state
reg [3:0] initCount;            // startup initialization sequence
reg [6:0] refreshTimer;         // counter until time to run refesh cycle
reg [22:0] addr;                // latched address
reg cEnable;                    // latched chip enable
reg aen;                        // keep track of address enable
reg refreshCall;                // set when time to run refresh cycle
reg refreshAck;                 // set when starting refresh cycle

// internal wires
wire [3:0] busNext;             // state machine next state

/*****************************************************************************/
// in-bound address latching

always @(posedge busClk or negedge busRESETn) begin
    if(!busRESETn) begin
        addr <= 0;
        cEnable <= 0; 
        aen <= 0;
    end else if(!busSTARTn) begin
        if(aen) begin
            addr <= addr;
            cEnable <= cEnable;
            aen <= aen;
        end else begin
            addr[22:0] <= busAD[24:2];
            cEnable <= cardCEn;
            aen <= 1;
        end
    end else begin
        addr <= 0;
        cEnable <= 0;
        aen <= 0;
    end
end

/*****************************************************************************/
// main control state machine

parameter
    sIDLE   =   0,  // idle state
    sMRAS   =   1,  // memory row address state
    sMCAS   =   2,  // memory column address state
    sMHLD   =   3,  // memory hold state
    sMEND   =   4,  // memory cycle end state
    sRCAS   =   5,  // refresh column state
    sRRAS   =   6,  // refresh row state
    sRHLD   =   7,  // refresh hold state
    sINIT   =   8;  // initialization hold state

always_comb begin
    case(busState)
        sIDLE: begin
            if(refreshCall) busNext = sRCAS;
            else if(!busSTARTn && !cardCEn) busNext = sMRAS;
            else if(!busSTARTn && aen && !cEnable) busNext = sMRAS;
            else busNext = sIDLE;
        end
        sMRAS: begin
            busNext = sMCAS;
        end
        sMCAS: begin
            busNext = sMHLD;
        end
        sMHLD: begin
            busNext = sMEND;
        end
        sMEND: begin
            if(!busSTARTn) busNext = sMEND;
            else if(refreshCall) busNext = sRCAS;
            else busNext = sIDLE;
        end
        sRCAS: begin
            busNext = sRRAS;
        end
        sRRAS: begin
            busNext = sRHLD;
        end
        sRHLD: begin
            if(initCount > 0) busNext = sRCAS;
            else busNext = sIDLE;
        end
        sINIT: begin
            busNext = sRCAS;
        end
        default: begin
            busNext = sIDLE;
        end
    endcase
end

always @(posedge busClk or negedge busRESETn) begin
    if(!busRESETn) busState <= sINIT;
    else busState <= busNext;
end

assign debugState = busState;

/*****************************************************************************/
//  startup initialization

// run 8 refresh cycles at startup before operation
always @(negedge busClk or negedge busRESETn) begin
    if(!busRESETn) initCount <= 4'h8;
    else if(initCount > 0 && busNext == sRHLD) initCount <= initCount - 4'h1;
    else initCount <= initCount;
end

/*****************************************************************************/
// DRAM refresh

// refresh cycle acknowledge
always @(posedge busClk or negedge busRESETn) begin
    if(!busRESETn) refreshAck <= 0;
    else begin
        if(busNext == sRCAS) refreshAck <= 1;
        else refreshAck <= 0;
    end
end

// refresh timing counter
always @(negedge busClk or negedge busRESETn) begin
    if(!busRESETn) begin
        refreshTimer <= 0;
        refreshCall <= 0;
    end
    else if(refreshTimer >= 7'h7E) begin
        refreshTimer <= 0;
        refreshCall <= 1;
    end
    else begin
        refreshTimer <= refreshTimer + 7'h1;
        if(refreshAck) refreshCall <= 0;
        else refreshCall <= refreshCall;
    end
end

/*****************************************************************************/
// DRAM control signals

// memory address
always @(posedge busClk or negedge busRESETn) begin
    if(!busRESETn) begin
        memADDR <= 11'h0;
    end else begin
        if(aen && busNext == sMRAS) memADDR <= addr[21:11];
        else if(busNext == sMRAS) memADDR <= busAD[23:13];
        else if(busNext == sMCAS) memADDR <= addr[10:0];
        else memADDR <= memADDR;
    end
end

// write enable
always @(posedge busClk or negedge busRESETn) begin
    if(!busRESETn) memWEn <= 1;
    else begin
        case(busNext)
            sMRAS, sMCAS, sMHLD, sMEND: memWEn <= busRWn;
            default: memWEn <= 1;
        endcase
    end
end

// row address strobe
always @(posedge busClk or negedge busRESETn) begin
    if(!busRESETn) memRASn <= 2'b11;
    else begin
        case(busNext)
            sMRAS, sMCAS, sMHLD: begin
                if(!aen && busNext == sMRAS) begin
                    memRASn[0] <= busAD[24];
                    memRASn[1] <= !busAD[24];
                end else begin
                    memRASn[0] <= addr[22];
                    memRASn[1] <= !addr[22];
                end
            end
            sRRAS, sRHLD: memRASn <= 2'b0;
            default: memRASn <= 2'b11;
        endcase
    end
end

// column address strobe
always @(posedge busClk or negedge busRESETn) begin
    if(!busRESETn) memCASn <= 4'b1111;
    else begin
        case(busNext)
            sMCAS, sMHLD: begin
                memCASn[0] <= busBEn[0];
                memCASn[1] <= busBEn[1];
                memCASn[2] <= busBEn[2];
                memCASn[3] <= busBEn[3];
            end
            sRCAS, sRRAS, sRHLD: memCASn <= 4'b0000;
            default: memCASn <= 4'b1111;
        endcase
    end
end

/*****************************************************************************/
// bus control signals

// data bus buffer enable (cardBENn)
always @(posedge busClk or negedge busRESETn) begin
    if(!busRESETn) cardBENn <= 1;
    else begin
        case(busNext)
            sMCAS, sMHLD, sMEND: cardBENn <= 0;
            default: cardBENn <= 1;
        endcase
    end
end

// bus cycle acknowledge (busACKn)
always @(negedge busClk or negedge busRESETn) begin
    if(!busRESETn) busACKn <= 1'bZ;
    else begin
        case(busState)
            sMCAS: busACKn <= 1'b0;
            sMHLD: busACKn <= 1'b1;
            default: busACKn <= 1'bZ;
        endcase
    end
end

endmodule