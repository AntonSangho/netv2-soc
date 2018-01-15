//////////////////////////////////////////////////////////////////////////////
//
//  Alphamax LLC NeTV2 dvi decoder for Artix FPGAs
//  GPLv3 / 2016
//
//////////////////////////////////////////////////////////////////////////////
//
//  Inspried by Spartan-6 code from a Xilinx XAPP by Bob Feng, but modified beyond recognition...
//
//////////////////////////////////////////////////////////////////////////////

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

`timescale 1 ns / 1ps

module hdmi_decoder (
		     input wire 	p_clk,
		     input wire 	reset_in,

		     input wire [9:0] 	raw_r, // unsynchronized symbols
		     input wire [9:0] 	raw_g,
		     input wire [9:0] 	raw_b,

		     input wire 	vld_r,
		     input wire 	vld_g,
		     input wire 	vld_b,
		     
		     output reg 	hsync, // hsync data
		     output reg 	vsync, // vsync data
		     output reg 	de, // data enable

		     output wire [29:0] sdout, // channel-bonded and synchronized
		     output wire [7:0] 	red, // pixel data out
		     output wire [7:0] 	green, // pixel data out
		     output wire [7:0] 	blue, // pixel data out

		     output reg 	encoding, // high when data island is valid
		     output wire 	hdcp_ena, // OR of data and video encyrption internal signals
		     output wire [3:0] 	red_di, // red data island
		     output wire [3:0] 	green_di, // green data island
		     output wire [3:0] 	blue_di, // blue data island
		     output reg 	data_gb, // guardbands
		     output reg 	video_gb,
		     output reg [3:0] 	ctl_code, // control code
		     output reg 	cv,
		     output wire 	valid,
		     output wire 	basic_de, // a DE that strobes even during data guardbands
		     output wire 	line_end  // fast-track signal that line ends for HDCP rekey
		    ) ; 


   reg 		     reset;
   
   wire 	    g_dgb, b_dgb, r_dgb;
   wire 	    g_vgb, b_vgb, r_vgb;
   wire             g_cv, b_cv, r_cv;

   wire [3:0] 	    r_t4;
   wire [3:0] 	    b_t4;
   wire [3:0] 	    g_t4;
   
   wire [9:0] 	    sdout_blue;
   wire [9:0] 	    sdout_green;
   wire [9:0] 	    sdout_red;

   reg 	     de_q1, de_reg;
   reg       hsync_q1, vsync_q1;
   wire      de_q2;
   wire      hsync_q2, vsync_q2;
   reg       cv_q;

   reg 	     data_gb_q, video_gb_q;
   reg [3:0] ctl_code_q;
   wire [3:0]  ctl_code_wire;

   always @ (posedge p_clk) begin
     reset <= reset_in;
   end
   
   parameter INIT =      8'b1 << 0;
   parameter GOING_T4 =  8'b1 << 1;
   parameter TERC4 =     8'b1 << 2;
   parameter LEAVE_T4 =  8'b1 << 3;
   parameter GOING_VID = 8'b1 << 4;
   parameter VIDEO =     8'b1 << 5;
   parameter PREAM_T4  = 8'b1 << 6;
   parameter PREAM_VID = 8'b1 << 7;
   parameter nSTATES =   8;

   reg [(nSTATES-1):0] cstate = {{(nSTATES-1){1'b0}},1'b1};
   reg [(nSTATES-1):0] nstate;

   parameter ENC_TMDS = 1'b0;
   parameter ENC_TERC4 = 1'b1;
   parameter HDCP_OFF = 1'b0;
   parameter HDCP_ON  = 1'b1;
   
//   reg 		       encoding;
   reg 		       encrypting_data;
   reg 		       encrypting_video;

   assign hdcp_ena = encrypting_data | encrypting_video;

   always @ (posedge p_clk) begin
      if (reset)
	cstate <= INIT;
            else
	cstate <=#1 nstate;
   end

   always @ (*) begin
      case (cstate) //synthesis parallel_case full_case
	//// NOTE NOTE NOTE
	//// green channel uses same code for video and data gb
	//// so we can't consider its information in this state machine
	INIT: begin
	   if( b_vgb & r_vgb & g_vgb ) begin
//	   if( b_vgb | r_vgb | g_vgb ) begin
	      nstate = GOING_VID;
	   end else if (ctl_code_wire == 4'b0101) begin
	      // we've found a preamble for data
	      nstate =  PREAM_T4;
	   end else if (ctl_code_wire == 4'b0001) begin
	      nstate = PREAM_VID;
	   end else begin
	      nstate = INIT;
	   end
	end
	PREAM_T4: begin
	   if( b_vgb & r_vgb & g_vgb ) begin
//	   if( b_vgb | r_vgb | g_vgb ) begin
	      nstate = GOING_VID;
	   end else if (r_dgb & g_dgb) begin
	      // data guardband only happens on b/r channels
	      nstate =  GOING_T4;
	   end else if (ctl_code_wire == 4'b0101) begin
	      nstate = PREAM_T4;
	   end else begin
	      nstate = INIT;
	   end
	end
	GOING_T4: begin
	    // wait till both dgb signals drop
	   nstate = (r_dgb & g_dgb) ? GOING_T4 : TERC4;
	end
	TERC4: begin
	   if( b_cv | r_cv | g_cv ) begin
	      nstate = INIT;
	   end else if( b_vgb & r_vgb & g_vgb ) begin
//	   end else if( b_vgb | r_vgb | g_vgb ) begin
	      // if we see a video guardband and we think we're in terc4 encoding
	      // it means we missed the end of data guardband; simply re-initialize
	      // the machine so we always recover from bit error drops
	      nstate = GOING_VID;
	   end else if( r_dgb & g_dgb ) begin
	      // otherwise, gracefully leave
	      nstate = LEAVE_T4;
	   end else begin
	      nstate = TERC4;
	   end
	end // case: TERC4
	LEAVE_T4: begin
	   // wait till both dgb signals drop
	   nstate = (r_dgb & g_dgb) ? LEAVE_T4 : INIT;
	end
	PREAM_VID: begin
	   if( ctl_code_wire == 4'b0001 ) begin
	      nstate = PREAM_VID;
	   end else if( b_vgb & r_vgb & g_vgb ) begin
//	   end else if( b_vgb | r_vgb | g_vgb ) begin
	      nstate = GOING_VID;
	   end else  begin
	      nstate = INIT;
	   end
	end
	GOING_VID: begin
	   nstate = ( b_vgb & r_vgb & g_vgb ) ? GOING_VID : VIDEO;
//	   nstate = ( b_vgb | r_vgb | g_vgb ) ? GOING_VID : VIDEO;
	end
	VIDEO: begin
	   if( b_cv | r_cv | g_cv ) begin
//	   if( b_cv & r_cv & g_cv ) begin
	      nstate = INIT;
	   end else begin
	      nstate = VIDEO;
	   end
	end
      endcase // case (cstate)
   end

   always @ (posedge p_clk) begin
      if( reset ) begin
	 encoding <=#1 ENC_TMDS;
	 encrypting_data <=#1 HDCP_OFF;
      end else begin
	 case (cstate) // synthesis parallel_case full_case
	   INIT: begin
	      encoding         <= #1 ENC_TMDS;
	      encrypting_data  <= #1 HDCP_OFF;
	      encrypting_video <= #1 HDCP_OFF;
	      de <= #1 1'b0;
	   end
	   PREAM_T4: begin
	      encoding         <= #1 ENC_TMDS;
	      encrypting_data  <= #1 HDCP_OFF;
	      encrypting_video <= #1 HDCP_OFF;
	      de <= #1 1'b0;
	   end
	   GOING_T4: begin
	      encoding         <= #1 ENC_TERC4;
	      encrypting_data  <= #1 HDCP_OFF;
	      encrypting_video <= #1 HDCP_OFF;
	      de <= #1 1'b0;
	   end
	   TERC4: begin
	      encoding         <= #1 ENC_TERC4;
	      encrypting_data  <= #1 HDCP_ON;
	      encrypting_video <= #1 HDCP_OFF;
	      de <= #1 1'b0;
	   end
	   LEAVE_T4: begin
	      encoding         <= #1 ENC_TERC4;
	      encrypting_video <= #1 HDCP_OFF;
	      encrypting_data  <= #1 HDCP_OFF;
	      de <= #1 1'b0;
	   end
	   PREAM_VID: begin
	      encoding         <= #1 ENC_TMDS;
	      encrypting_data  <= #1 HDCP_OFF;
	      encrypting_video <= #1 HDCP_OFF;
	      de <= #1 1'b0;
	   end
	   GOING_VID: begin
	      encoding         <= #1 ENC_TMDS;
	      encrypting_data  <= #1 HDCP_OFF;
	      encrypting_video <= #1 HDCP_OFF;
	      de <= #1 1'b0;
	   end
	   VIDEO: begin
	      encoding         <= #1 ENC_TMDS;
	      encrypting_data  <= #1 HDCP_OFF;
	      encrypting_video <= #1 HDCP_ON;
	      de <= #1 1'b1;
	   end
	 endcase // case (cstate)
      end // else: !if( reset )
   end // always @ (posedge p_clk or posedge reset)
   
   
   assign sdout = {sdout_red[9:0], sdout_green[9:0], sdout_blue[9:0]};

   // what the fuck is this all about....
//  assign sdout = {sdout_red[9:5], sdout_green[9:5], sdout_blue[9:5],
//                  sdout_red[4:0], sdout_green[4:0], sdout_blue[4:0]};

  wire de_b, de_g, de_r;

   assign de_q2 = de_b;
   // to do: modify this against guard bands -- if dgb activates, set a flag to set terc4 coding
   // until dgb triggers again; include a force-clear if a vgb is encountered

   wire 	     blue_vld;
   wire 	     green_vld;
   wire 	     red_vld;
   wire 	     blue_rdy;
   wire 	     green_rdy;
   wire 	     red_rdy;

   assign blue_vld = vld_b;
   assign green_vld = vld_g;
   assign red_vld = vld_r;
   
   assign valid = blue_vld & green_vld & red_vld;
   
   wire line_end_r;
   wire line_end_g;
   wire line_end_b;

   
  decodeb dec_b (
    .reset        (reset),
    .pclk         (p_clk),
    .rawdata      (raw_b),

    .other_ch0_rdy(green_rdy),
    .other_ch1_rdy(red_rdy),
    .other_ch0_vld(green_vld),
    .other_ch1_vld(red_vld),

    .iamvld       (blue_vld),
    .iamrdy       (blue_rdy),
		 
    .c0           (hsync_q2),
    .c1           (vsync_q2),
    .de           (de_b),
    .sdout        (sdout_blue),
    .dgb          (b_dgb),
    .vgb          (b_vgb),
    .ctl_vld      (b_cv),
    .line_end     (line_end_b)
		 ) ;

   // adds two pipe stages so no need to rebalance later on
   tmds_data_dec tmds_b (
			 .clk(p_clk),
			 .de(de_b),
			 .tmds_dat(sdout_blue),
			 .dec_dat(blue) );

  decodeg dec_g (
    .reset        (reset),
    .pclk         (p_clk),
    .rawdata      (raw_g),

    .other_ch0_rdy(blue_rdy),
    .other_ch1_rdy(red_rdy),
    .other_ch0_vld(blue_vld),
    .other_ch1_vld(red_vld),

    .iamvld       (green_vld),
    .iamrdy       (green_rdy),
		 
    .c0           (ctl_code_wire[0]),
    .c1           (ctl_code_wire[1]),
    .de           (de_g),
    .sdout        (sdout_green),
    .dgb          (g_dgb),
    .vgb          (g_vgb),
    .ctl_vld      (g_cv),
    .line_end     (line_end_g)
		 ) ;

   tmds_data_dec tmds_g (
			 .clk(p_clk),
			 .de(de_g),
			 .tmds_dat(sdout_green),
			 .dec_dat(green) );
    
  decoder dec_r (
    .reset        (reset),
    .pclk         (p_clk),
    .rawdata      (raw_r),

    .other_ch0_rdy(blue_rdy),
    .other_ch1_rdy(green_rdy),
    .other_ch0_vld(blue_vld),
    .other_ch1_vld(green_vld),

    .iamvld       (red_vld),
    .iamrdy       (red_rdy),
		 
    .c0           (ctl_code_wire[2]),
    .c1           (ctl_code_wire[3]),
    .de           (de_r),
    .sdout        (sdout_red),
    .dgb          (r_dgb),
    .vgb          (r_vgb),
    .ctl_vld      (r_cv),
    .line_end     (line_end_r)
		 ) ;

   tmds_data_dec tmds_r (
			 .clk(p_clk),
			 .de(de_r),
			 .tmds_dat(sdout_red),
			 .dec_dat(red) );
   
   assign basic_de = de_b | de_r | de_g | b_vgb | r_vgb | g_vgb;
   
   assign line_end = line_end_g | line_end_r | line_end_b;

   // pipe alignment registers
   always @(posedge p_clk) begin
	 // reversed the naming convention for the following pipe stages
	 hsync <= hsync_q1;
	 hsync_q1 <= hsync_q2;

	 vsync <= vsync_q1;
	 vsync_q1 <= vsync_q2;

	 de_reg <= de_q1;
	 de_q1 <= de_q2;

	 data_gb_q <= r_dgb & g_vgb; // data guardbands only on red and green channels
	 data_gb <= data_gb_q;
	 
	 video_gb_q <= r_vgb & b_vgb & g_vgb;
	 video_gb <= video_gb_q;
	 
	 ctl_code_q <= ctl_code_wire;
	 ctl_code <= ctl_code_q;

	 cv_q <= b_cv & r_cv & g_cv;
	 cv <= cv_q;
   end // always @ (posedge p_clk or posedge reset)
//   assign de = de_reg & (encoding == ENC_TMDS);

   // looks like maybe the terc4 path is short by one pipeline stage...
   decode_terc4 dec_t4_g (
			 .rstin( reset ),
			 .clkin(p_clk),
			 .din(sdout_green),
			 .dout(g_t4));
   
   decode_terc4 dec_t4_r (
			 .rstin( reset ),
			 .clkin(p_clk),
			 .din(sdout_red),
			 .dout(r_t4));
   
   decode_terc4 dec_t4_b (
			 .rstin( reset ),
			 .clkin(p_clk),
			 .din(sdout_blue),
			 .dout(b_t4));
   assign red_di = r_t4;
   assign green_di = g_t4;
   assign blue_di = b_t4;

endmodule
