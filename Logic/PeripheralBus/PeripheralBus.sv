/*
 * PikaPC PeripheralBus Control
 * techav
 * 2025/12/19
 *
 */

module PeripheralBus (
    input   wire                    sysClk,         // system clock
    input   wire                    nReset,         // system reset
    input   wire [7:2]              nCS,            // chip select signals
    input   wire [3:0]              nWBE,           // write byte enables
    input   wire                    nOE,            // output enable
    input   wire                    nVReady,        // ready input
    input   wire                    ADDR23,         // address 23 signal
    output  reg  [3:0]              nVBE,           // VLB byte enables
    output  reg  [3:0]              nPBE,           // buffer enables
    output  reg                     nPBE32,         // 32-bit buffer enable
    output  reg                     nVRdyIn,        // VLB ready in return
    output  reg                     nADS,           // VLB address strobe
    output  reg  [1:0]              SAUP,           // ViRGE select signals
    output  reg                     VMnIO           // VLB memory / IO signal
);


/*****************************************************************************/
// VLB Address Strobe State Machine

parameter 
    sADSIDL =   0,  // idle state
    sADSACT =   1,  // ADS active
    sADSWAT =   2;  // ADS wait
reg [1:0] adsState;
wire [1:0] adsNext;
wire vlbCycle;

assign vlbCycle = (!nCS[3] || !nCS[2]);

always_comb begin
    case(adsState)
        sADSIDL: begin
            if(vlbCycle) adsNext = sADSACT;
            else adsNext = sADSIDL;
        end
        sADSACT: begin
            adsNext = sADSWAT;
        end
        sADSWAT: begin
            if(vlbCycle) adsNext = sADSWAT;
            else adsNext = sADSIDL;
        end
    endcase
end

always @(negedge sysClk or negedge vlbCycle) begin
    if(!vlbCycle) adsState <= sADSIDL;
    else adsState <= adsNext;
end

always @(negedge sysClk or negedge nReset) begin
    if(!nReset) nADS <= 1;
    else begin
        if(adsNext == sADSACT) nADS <= 0;
        else nADS <= 1;
    end
end

// SAUP 
always @(negedge sysClk or negedge vlbCycle) begin
    if(!vlbCycle) SAUP <= 2'b0;
    else begin
        if (vlbCycle) begin
            if (ADDR23) SAUP <= 2'b10;
            else SAUP <= 2'b01;
        end else SAUP <= 2'b00;
    end
end

// M/IO
always @(negedge sysClk or posedge nCS[3]) begin
    if(nCS[3]) VMnIO <= 1'b1;
    else begin
        if(!nCS[3]) begin
            if(ADDR23) VMnIO <= 1'b1;
            else VMnIO <= 1'b0;
        end else VMnIO <= 1'b1;
    end
end

// Ready In
assign nVRdyIn = nVReady;

// buffer enables
wire [1:0] lowAddr;
assign lowAddr = nWBE[1:0];

wire pbCycle;

assign pbCycle = (nCS != 6'b111111);

always @(negedge sysClk or negedge pbCycle) begin
    if(!pbCycle) begin
        nPBE <= 4'b1111;
        nPBE32 <= 1'b1;
    end else begin
        if(!nCS[2] || !nCS[7]) begin                                // 32-bit
            nPBE32 <= 0;
            nPBE <= 4'b1110;
        end else if(!nCS[3]) begin                                  // 8-bit VLB
            nPBE32 <= 1;
            case(lowAddr)
                2'b00: nPBE <= 4'b1110;
                2'b01: nPBE <= 4'b1101;
                2'b10: nPBE <= 4'b1011;
                2'b11: nPBE <= 4'b0111;
            endcase
        end else if(!nCS[4] || !nCS[5] || !nCS[6]) begin            // 8-bit
            nPBE32 <= 1;
            nPBE <= 4'b1110;
        end else begin
            nPBE <= 4'b1111;
            nPBE32 <= 1'b1;
        end
    end
end

// VLB byte enable signals

always @(negedge sysClk or negedge vlbCycle) begin
    if(!vlbCycle) nVBE <= 4'b1111;
    else begin
        if(!nCS[2]) begin
            if(!nOE) nVBE <= 4'b0000;
            else if(!nWBE[0]) nVBE <= 4'b0111;
            else if(!nWBE[1]) nVBE <= 4'b1011;
            else if(!nWBE[2]) nVBE <= 4'b1101;
            else if(!nWBE[3]) nVBE <= 4'b1110;
            else nVBE <= 4'b1111;
        end else if(!nCS[3]) begin
            case(lowAddr)
                2'b00: nVBE <= 4'b1110;
                2'b01: nVBE <= 4'b1101;
                2'b10: nVBE <= 4'b1011;
                2'b11: nVBE <= 4'b0111;
            endcase
        end else begin
            nVBE <= 4'b1111;
        end
    end
end

endmodule