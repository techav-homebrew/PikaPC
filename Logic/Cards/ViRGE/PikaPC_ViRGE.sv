/*
 * PikaPC ViRGE Card Bus Control
 * techav
 * 2026/03/30
 *
 */

module ViRGEbus (
    input   wire                busClk,         // main bus clock
    input   wire                busRESETn,      // main reset

    input   wire                busRWn,         // bus read/write signal
    input   wire                busSTARTn,      // bus cycle start
    inout   wire                busACKn,        // bus cycle acknowledge        //
    input   wire                busBURSTn,      // bus burst request
    input   wire                busBRACKn,      // bus burst acknowledge
    inout   wire [2:0]          busIRQn,        // bus interrupt request        //
    input   wire                busIACKn,       // bus interrupt acknowledge

    input   wire                cardA23,        // card address 23
    input   wire                cardA24,        // card address 24
    input   wire                cardCEn,        // card select
    output  reg                 cardWENn,       // card write enable            //
    output  reg                 cardOEn,        // card output enable           //
    output  reg                 cardBENn,       // card data buffer enable      //
    output  reg                 cardAENn,       // card address latch           //

    input   wire                vidINT,         // virge interrupt request
    input   wire                vidSRDYn,       // virge ready
    output  reg                 vidRDYINn,      // virge ready acknowledge      //
    output  reg                 vidADSn,        // virge address strobe         //
    output  reg                 vidSAUP1,       // virge enable 1               //
    output  reg                 vidSAUP2,       // virge enable 2               //
    output  reg                 vidMIOn,        // virge Memory/IO              //

    output  reg [2:0]           debugState      // state machine state output
);

/*****************************************************************************/
// main bus cycle state machine

parameter
    sIDLE   =   0,  // idle state
    sADDR   =   1,  // bus address latch
    sDATA   =   2,  // bus data / wait for ACK
    sTERM   =   3,  // terminate bus cycle
    sCEND   =   4;  // bus cycle end
reg [2:0] busState;
wire [2:0] busNext;

always_comb begin
    case(busState)
        sIDLE: begin
            if(!busSTARTn && !cardCEn) busNext = sADDR;
            else busNext = sIDLE;
        end
        sADDR: begin
            busNext = sDATA;
        end
        sDATA: begin
            if(!vidSRDYn) busNext = sTERM;
            else busNext = sDATA;
        end
        sTERM: begin
            busNext = sCEND;
        end
        sCEND: begin
            if(!busSTARTn) busNext = sCEND;
            else busNext = sIDLE;
        end
    endcase
end

always @(posedge busClk or negedge busRESETn) begin
    if(!busRESETn) busState <= sIDLE;
    else if(busSTARTn) busState <= sIDLE;
    else busState <= busNext;
end

assign debugState = busState;

/*****************************************************************************/
// control signals

// interrupt
always_comb begin
    busIRQn = 3'bZZZ;
    if(vidINT) busIRQn[0] = 1'b0;
end

// bus cycle acknowledge
always_comb begin
    if(!vidSRDYn) busACKn <= 1'b0;
    else busACKn <= 1'bZ;
end

// vlb address strobe (falling edge!)
always @(negedge busClk or negedge busRESETn) begin
    if(!busRESETn) vidADSn <= 1'b1;
    else if (busState == sADDR) vidADSn <= 1'b0;
    else vidADSn <= 1'b1;
end

// bus cycle control signals
always @(posedge busClk or negedge busRESETn) begin
    if(!busRESETn) begin
        cardAENn <= 1'b1;
        cardBENn <= 1'b1;
        cardWENn <= 1'b1;
        vidMIOn  <= 1'b1;
        cardOEn  <= 1'b1;
        vidSAUP1 <= 1'b0;
        vidSAUP2 <= 1'b0;
    end else begin
        case(busNext)
            sADDR: begin
                cardAENn <= 1'b0;
                cardBENn <= 1'b1;
                cardWENn <= busRWn;
                vidMIOn  <= cardA23;
                cardOEn  <= busRWn;
                vidSAUP1 <= !cardA24;
                vidSAUP2 <= cardA24;
            end
            sDATA, sTERM, sCEND: begin
                cardAENn <= 1'b0;
                cardBENn <= 1'b0;
                cardWENn <= cardWENn;
                vidMIOn  <= vidMIOn;
                cardOEn  <= cardOEn;
                vidSAUP1 <= vidSAUP1;
                vidSAUP2 <= vidSAUP2;
            end
            default: begin
                cardAENn <= 1'b1;
                cardBENn <= 1'b1;
                cardWENn <= cardWENn;
                vidMIOn  <= vidMIOn;
                cardOEn  <= cardOEn;
                vidSAUP1 <= 1'b0;
                vidSAUP2 <= 1'b0;
            end
        endcase
    end
end

// ready in
always @(negedge busClk or negedge busRESETn) begin
    if(!busRESETn) vidRDYINn <= 1'b1;
    else begin
        if(busState == sTERM) vidRDYINn <= 1'b0;
        else vidRDYINn <= 1'b1;
    end
end

endmodule