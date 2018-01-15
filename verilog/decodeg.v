/*
    This program is free software: you can redistribute it and/or modify
     it under the terms of the GNU General Public License as published by
     the Free Software Foundation, either version 3 of the License, or
     (at your option) any later version.
 
     This program is distributed in the hope that it will be useful,
     but WITHOUT ANY WARRANTY; without even the implied warranty of
     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
     GNU General Public License for more details.
 
     You should have received a copy of the GNU General Public License
     along with this program.  If not, see <http://www.gnu.org/licenses/>.
 
 
     Copyright 2016 Andrew 'bunnie' Huang, all rights reserved 
 */

// this code based on decoder.v by Bob Feng from Xilinx (2010)
//  Copyright © 2004 Xilinx, Inc.
//  All rights reserved
`timescale 1 ns / 1ps

module decodeg (
  input wire 	   reset, //
  input wire 	   pclk, //  pixel clock
  input wire [9:0] rawdata, //  data from hdmi, raw symbols eye-aligned

  input wire 	     other_ch0_vld, //  other channel0 has valid data now
  input wire 	     other_ch1_vld, //  other channel1 has valid data now
  input wire 	     other_ch0_rdy, //  other channel0 has detected a valid starting pixel
  input wire 	     other_ch1_rdy, //  other channel1 has detected a valid starting pixel

  input wire 	     iamvld, //  I have valid data now
  output wire 	     iamrdy, //  I have detected a valid new pixel
		
  output wire 	   psalgnerr, //  Phase alignment error
  output reg 	   c0,
  output reg 	   c1,
  output reg 	   de, 
  output reg [9:0] sdout,
  output reg [7:0] dout,
  output reg 	   dgb,
  output reg 	   vgb,
  output reg 	   ctl_vld,
  output wire 	   line_end
		);

  ///////////////////////////////////////
  // Per Channel De-skew Instance
  ///////////////////////////////////////
  wire [9:0] sdata;
  chnlbond cbnd (
    .clk(pclk),
    .rawdata(rawdata),
    .iamvld(iamvld),
    .other_ch0_vld(other_ch0_vld),
    .other_ch1_vld(other_ch1_vld),
    .other_ch0_rdy(other_ch0_rdy),
    .other_ch1_rdy(other_ch1_rdy),
    .iamrdy(iamrdy),
    .sdata(sdata)
  );

   ////------- common above
   ////------- variant below

   ////
   // hack to accelerate detection of line end so that HDCP rekey
   // can meet stringent timing spec requirement
   ////
   assign line_end = de &&
		     ((sdata == CTRLTOKEN0) ||
		      (sdata == CTRLTOKEN1) ||
		      (sdata == CTRLTOKEN2) ||
		      (sdata == CTRLTOKEN3));
   
  /////////////////////////////////////////////////////////////////
  // Below performs the 10B-8B decoding function defined in DVI 1.0
  // Specification: Section 3.3.3, Figure 3-6, page 31. 
  /////////////////////////////////////////////////////////////////
  // Distinct Control Tokens
  parameter CTRLTOKEN0 = 10'b1101010100;
  parameter CTRLTOKEN1 = 10'b0010101011;
  parameter CTRLTOKEN2 = 10'b0101010100;
  parameter CTRLTOKEN3 = 10'b1010101011;

  parameter DATA_GB    = 10'b0100110011;
  parameter VID_B_GB   = 10'b1011001100;
  parameter VID_G_GB   = 10'b0100110011;
  parameter VID_R_GB   = 10'b1011001100;

  wire [7:0] data;
   genvar    i;
   generate
      for( i = 0; i <= 7; i = i + 1 ) begin : genloop
	 assign data[i] = sdata[9] ? !sdata[i] : sdata[i];
      end
   endgenerate
//  assign data = (sdata[9]) ? ~sdata[7:0] : sdata[7:0]; 

  always @ (posedge pclk) begin
    if(iamrdy && other_ch0_rdy && other_ch1_rdy) begin
      case (sdata) 
        CTRLTOKEN0: begin
          c0 <=#1 1'b0;
          c1 <=#1 1'b0;
          de <=#1 1'b0;
           dgb <= #1 1'b0;
	   vgb <= #1 1'b0;
	   ctl_vld <= #1 1'b1;
        end

        CTRLTOKEN1: begin
          c0 <=#1 1'b1;
          c1 <=#1 1'b0;
          de <=#1 1'b0;
           dgb <= #1 1'b0;
	   vgb <= #1 1'b0;
	   ctl_vld <= #1 1'b1;
        end

        CTRLTOKEN2: begin
          c0 <=#1 1'b0;
          c1 <=#1 1'b1;
          de <=#1 1'b0;
           dgb <= #1 1'b0;
	   vgb <= #1 1'b0;
	   ctl_vld <= #1 1'b1;
        end
        
        CTRLTOKEN3: begin
          c0 <=#1 1'b1;
          c1 <=#1 1'b1;
          de <=#1 1'b0;
           dgb <= #1 1'b0;
	   vgb <= #1 1'b0;
	   ctl_vld <= #1 1'b1;
        end

	DATA_GB: begin
	   c0 <=#1 1'b0;
	   c1 <=#1 1'b0;
	   de <=#1 1'b0;
           dgb <= #1 1'b1; // green is ambiguous between data and video guardbands
	   vgb <= #1 1'b1;
	   ctl_vld <= #1 1'b0;
	end

	VID_G_GB: begin
	   c0 <=#1 1'b0;
	   c1 <=#1 1'b0;
	   de <=#1 1'b0;
           dgb <= #1 1'b1; // green is ambiguous between data and video guardbands
	   vgb <= #1 1'b1;
	   ctl_vld <= #1 1'b0;
	end
        
        default: begin 
          dout[0] <=#1 data[0];
          dout[1] <=#1 (sdata[8]) ? (data[1] ^ data[0]) : (data[1] ~^ data[0]);
          dout[2] <=#1 (sdata[8]) ? (data[2] ^ data[1]) : (data[2] ~^ data[1]);
          dout[3] <=#1 (sdata[8]) ? (data[3] ^ data[2]) : (data[3] ~^ data[2]);
          dout[4] <=#1 (sdata[8]) ? (data[4] ^ data[3]) : (data[4] ~^ data[3]);
          dout[5] <=#1 (sdata[8]) ? (data[5] ^ data[4]) : (data[5] ~^ data[4]);
          dout[6] <=#1 (sdata[8]) ? (data[6] ^ data[5]) : (data[6] ~^ data[5]);
          dout[7] <=#1 (sdata[8]) ? (data[7] ^ data[6]) : (data[7] ~^ data[6]);

          de <=#1 1'b1;

           dgb <= #1 1'b0;
	   vgb <= #1 1'b0;
	   ctl_vld <= #1 1'b0;
        end                                                                      
      endcase                                                                    

      sdout <=#1 sdata;
    end else begin
      c0 <= 1'b0;
      c1 <= 1'b0;
      de <= 1'b0;
      dout <= 8'h0;
      sdout <= 10'h0;

       dgb <= 1'b0;
       vgb <= 1'b0;
       ctl_vld <= 1'b0;
    end
  end
endmodule
