## This file is a general .xdc for the Nexys4 rev B board
## To use it in a project:
## - uncomment the lines corresponding to used pins
## - rename the used ports (in each line, after get_ports) according to the top level signal names in the project

# Ignore false paths crossing clock domains in pixel output stage

## Clock signal
set_property -dict {PACKAGE_PIN F22 IOSTANDARD LVCMOS33} [get_ports CLK_IN]
#create_clock -period 10.000 -name sys_clk_pin -waveform {0.000 5.000} -add [get_ports CLK_IN]
create_clock -period 20.000 -name sys_clk_pin -waveform {0.000 10.000} -add [get_ports CLK_IN]
#create_clock -period 20.000 -name sys_clk_pin -add [get_ports CLK_IN]

set_false_path -from [get_cells led*]
set_false_path -to [get_cells vga*]
set_false_path -to [get_cells *jb*]

## Make Ethernet clocks unrelated to other clocks to avoid erroneous timing
## violations, and hopefully make everything synthesise faster.
set_clock_groups -asynchronous \
     -group { cpuclock hdmi_clk_OBUF vdac_clk_OBUF clock162 clock325 } \
     -group { CLKFBOUT clk_fb_eth clock100 clock200 eth_clock_OBUF } \

# Deal with more false paths crossing ethernet / cpu clock domains
set_false_path -from [get_clocks cpuclock] -to [get_clocks ethclock]
set_false_path -from [get_clocks ethclock] -to [get_clocks cpuclock]


## Switches
set_property -dict {PACKAGE_PIN Y22  IOSTANDARD LVCMOS18} [get_ports {sw[0]}]
set_property -dict {PACKAGE_PIN AA22 IOSTANDARD LVCMOS18} [get_ports {sw[1]}]
set_property -dict {PACKAGE_PIN Y23  IOSTANDARD LVCMOS18} [get_ports {sw[2]}]
set_property -dict {PACKAGE_PIN AA24 IOSTANDARD LVCMOS18} [get_ports {sw[3]}]
set_property -dict {PACKAGE_PIN Ac23 IOSTANDARD LVCMOS18} [get_ports {sw[4]}]
set_property -dict {PACKAGE_PIN AC24 IOSTANDARD LVCMOS18} [get_ports {sw[5]}]
set_property -dict {PACKAGE_PIN AA25 IOSTANDARD LVCMOS18} [get_ports {sw[6]}]
set_property -dict {PACKAGE_PIN AB25 IOSTANDARD LVCMOS18} [get_ports {sw[7]}]

## LEDs
set_property -dict {PACKAGE_PIN R26 IOSTANDARD LVCMOS33} [get_ports {led[0]}]
set_property -dict {PACKAGE_PIN P26 IOSTANDARD LVCMOS33} [get_ports {led[1]}]
set_property -dict {PACKAGE_PIN E25 IOSTANDARD LVCMOS33} [get_ports {led[2]}]
set_property -dict {PACKAGE_PIN C14 IOSTANDARD LVCMOS33} [get_ports {led[3]}]
set_property -dict {PACKAGE_PIN B14 IOSTANDARD LVCMOS33} [get_ports {led[4]}]
#set_property -dict {PACKAGE_PIN V17 IOSTANDARD LVCMOS33} [get_ports {led[5]}]
#set_property -dict {PACKAGE_PIN U17 IOSTANDARD LVCMOS33} [get_ports {led[6]}]
#set_property -dict {PACKAGE_PIN U16 IOSTANDARD LVCMOS33} [get_ports {led[7]}]
#set_property -dict {PACKAGE_PIN V16 IOSTANDARD LVCMOS33} [get_ports {led[8]}]
#set_property -dict {PACKAGE_PIN T15 IOSTANDARD LVCMOS33} [get_ports {led[9]}]
#set_property -dict {PACKAGE_PIN U14 IOSTANDARD LVCMOS33} [get_ports {led[10]}]
#set_property -dict {PACKAGE_PIN T16 IOSTANDARD LVCMOS33} [get_ports {led[11]}]
#set_property -dict {PACKAGE_PIN V15 IOSTANDARD LVCMOS33} [get_ports {led[12]}]
#set_property -dict {PACKAGE_PIN V14 IOSTANDARD LVCMOS33} [get_ports {led[13]}]
#set_property -dict {PACKAGE_PIN V12 IOSTANDARD LVCMOS33} [get_ports {led[14]}]
#set_property -dict {PACKAGE_PIN V11 IOSTANDARD LVCMOS33} [get_ports {led[15]}]

#set_property -dict { PACKAGE_PIN K5  IOSTANDARD LVCMOS33 } [get_ports RGB1_Red]
#set_property -dict { PACKAGE_PIN F13 IOSTANDARD LVCMOS33 } [get_ports RGB1_Green]
#set_property -dict { PACKAGE_PIN F6  IOSTANDARD LVCMOS33 } [get_ports RGB1_Blue]
#set_property -dict { PACKAGE_PIN K6  IOSTANDARD LVCMOS33 } [get_ports RGB2_Red]
#set_property -dict { PACKAGE_PIN H6  IOSTANDARD LVCMOS33 } [get_ports RGB2_Green]
#set_property -dict { PACKAGE_PIN L16 IOSTANDARD LVCMOS33 } [get_ports RGB2_Blue]

##7 segment display
#set_property -dict {PACKAGE_PIN T10 IOSTANDARD LVCMOS33} [get_ports {sseg_ca[0]}]
#set_property -dict {PACKAGE_PIN R10 IOSTANDARD LVCMOS33} [get_ports {sseg_ca[1]}]
#set_property -dict {PACKAGE_PIN K16 IOSTANDARD LVCMOS33} [get_ports {sseg_ca[2]}]
#set_property -dict {PACKAGE_PIN K13 IOSTANDARD LVCMOS33} [get_ports {sseg_ca[3]}]
#set_property -dict {PACKAGE_PIN P15 IOSTANDARD LVCMOS33} [get_ports {sseg_ca[4]}]
#set_property -dict {PACKAGE_PIN T11 IOSTANDARD LVCMOS33} [get_ports {sseg_ca[5]}]
#set_property -dict {PACKAGE_PIN L18 IOSTANDARD LVCMOS33} [get_ports {sseg_ca[6]}]

#set_property -dict {PACKAGE_PIN H15 IOSTANDARD LVCMOS33} [get_ports {sseg_ca[7]}]

#set_property -dict {PACKAGE_PIN J17 IOSTANDARD LVCMOS33} [get_ports {sseg_an[0]}]
#set_property -dict {PACKAGE_PIN J18 IOSTANDARD LVCMOS33} [get_ports {sseg_an[1]}]
#set_property -dict {PACKAGE_PIN T9 IOSTANDARD LVCMOS33} [get_ports {sseg_an[2]}]
#set_property -dict {PACKAGE_PIN J14 IOSTANDARD LVCMOS33} [get_ports {sseg_an[3]}]
#set_property -dict {PACKAGE_PIN P14 IOSTANDARD LVCMOS33} [get_ports {sseg_an[4]}]
#set_property -dict {PACKAGE_PIN T14 IOSTANDARD LVCMOS33} [get_ports {sseg_an[5]}]
#set_property -dict {PACKAGE_PIN K2 IOSTANDARD LVCMOS33} [get_ports {sseg_an[6]}]
#set_property -dict {PACKAGE_PIN U13 IOSTANDARD LVCMOS33} [get_ports {sseg_an[7]}]

##Buttons
set_property -dict {PACKAGE_PIN J7   IOSTANDARD LVCMOS33} [get_ports fpga_done]
set_property -dict {PACKAGE_PIN P6   IOSTANDARD LVCMOS33} [get_ports btnProgram]
set_property -dict {PACKAGE_PIN AB26 IOSTANDARD LVCMOS18} [get_ports btnCpuReset]
set_property -dict {PACKAGE_PIN AC26 IOSTANDARD LVCMOS18} [get_ports {btn[0]}]
set_property -dict {PACKAGE_PIN AD18 IOSTANDARD LVCMOS18} [get_ports {btn[1]}]
set_property -dict {PACKAGE_PIN AF19 IOSTANDARD LVCMOS18} [get_ports {btn[2]}]
set_property -dict {PACKAGE_PIN AF20 IOSTANDARD LVCMOS18} [get_ports {btn[3]}]

## Hyper RAM
set_property -dict {PACKAGE_PIN B16 IOSTANDARD LVCMOS33 PULLUP FALSE SLEW FAST DRIVE 16} [get_ports hr_clk_p]
set_property -dict {PACKAGE_PIN D20 IOSTANDARD LVCMOS33 PULLUP FALSE SLEW FAST DRIVE 16} [get_ports {hr_d[0]}]
set_property -dict {PACKAGE_PIN A24 IOSTANDARD LVCMOS33 PULLUP FALSE SLEW FAST DRIVE 16} [get_ports {hr_d[1]}]
set_property -dict {PACKAGE_PIN E22 IOSTANDARD LVCMOS33 PULLUP FALSE SLEW FAST DRIVE 16} [get_ports {hr_d[2]}]
set_property -dict {PACKAGE_PIN D24 IOSTANDARD LVCMOS33 PULLUP FALSE SLEW FAST DRIVE 16} [get_ports {hr_d[3]}]
set_property -dict {PACKAGE_PIN D23 IOSTANDARD LVCMOS33 PULLUP FALSE SLEW FAST DRIVE 16} [get_ports {hr_d[4]}]
set_property -dict {PACKAGE_PIN E21 IOSTANDARD LVCMOS33 PULLUP FALSE SLEW FAST DRIVE 16} [get_ports {hr_d[5]}]
set_property -dict {PACKAGE_PIN A23 IOSTANDARD LVCMOS33 PULLUP FALSE SLEW FAST DRIVE 16} [get_ports {hr_d[6]}]
set_property -dict {PACKAGE_PIN D19 IOSTANDARD LVCMOS33 PULLUP FALSE SLEW FAST DRIVE 16} [get_ports {hr_d[7]}]
set_property -dict {PACKAGE_PIN B17 IOSTANDARD LVCMOS33 PULLUP FALSE SLEW FAST DRIVE 16} [get_ports hr_rwds]
set_property -dict {PACKAGE_PIN C16 IOSTANDARD LVCMOS33 PULLUP FALSE} [get_ports hr_reset]
set_property -dict {PACKAGE_PIN D16 IOSTANDARD LVCMOS33 PULLUP FALSE} [get_ports hr_cs0]
# Place HyperRAM close to I/O pins
create_pblock pblock_hyperram
add_cells_to_pblock pblock_hyperram [get_cells [list hyperram0]]
resize_pblock pblock_hyperram -add {SLICE_X0Y150:SLICE_X49Y199}
resize_pblock pblock_hyperram -add {SLICE_X0Y100:SLICE_X49Y149}
# resize_pblock pblock_hyperram -add {SLICE_X0Y186:SLICE_X35Y224}
# resize_pblock pblock_hyperram -add {SLICE_X8Y175:SLICE_X23Y186}

# ##Pmod Header JA
# set_property -dict {PACKAGE_PIN D20 IOSTANDARD LVCMOS33} [get_ports {jalo[0]}]
# set_property -dict {PACKAGE_PIN A24 IOSTANDARD LVCMOS33} [get_ports {jalo[1]}]
# set_property -dict {PACKAGE_PIN E22 IOSTANDARD LVCMOS33} [get_ports {jalo[2]}]
# set_property -dict {PACKAGE_PIN D24 IOSTANDARD LVCMOS33} [get_ports {jalo[3]}]
# set_property -dict {PACKAGE_PIN D19 IOSTANDARD LVCMOS33} [get_ports {jahi[0]}]
# set_property -dict {PACKAGE_PIN A23 IOSTANDARD LVCMOS33} [get_ports {jahi[1]}]
# set_property -dict {PACKAGE_PIN E21 IOSTANDARD LVCMOS33} [get_ports {jahi[2]}]
# set_property -dict {PACKAGE_PIN D23 IOSTANDARD LVCMOS33} [get_ports {jahi[3]}]

# ##Pmod Header JB
# set_property -dict {PACKAGE_PIN A15 IOSTANDARD LVCMOS33} [get_ports {jblo[0]}]
# set_property -dict {PACKAGE_PIN D16 IOSTANDARD LVCMOS33} [get_ports {jblo[1]}]
# set_property -dict {PACKAGE_PIN B16 IOSTANDARD LVCMOS33} [get_ports {jblo[2]}]
# set_property -dict {PACKAGE_PIN A17 IOSTANDARD LVCMOS33} [get_ports {jblo[3]}]
# set_property -dict {PACKAGE_PIN B15 IOSTANDARD LVCMOS33} [get_ports {jbhi[0]}]
# set_property -dict {PACKAGE_PIN D15 IOSTANDARD LVCMOS33} [get_ports {jbhi[1]}]
# set_property -dict {PACKAGE_PIN C16 IOSTANDARD LVCMOS33} [get_ports {jbhi[2]}]
# set_property -dict {PACKAGE_PIN B17 IOSTANDARD LVCMOS33} [get_ports {jbhi[3]}]

##Pmod Header JC
set_property -dict {PACKAGE_PIN B9  IOSTANDARD LVCMOS33} [get_ports {jclo[0]}]
set_property -dict {PACKAGE_PIN A10 IOSTANDARD LVCMOS33} [get_ports {jclo[1]}]
set_property -dict {PACKAGE_PIN D10 IOSTANDARD LVCMOS33} [get_ports {jclo[2]}]
set_property -dict {PACKAGE_PIN B11 IOSTANDARD LVCMOS33} [get_ports {jclo[3]}]
set_property -dict {PACKAGE_PIN C9  IOSTANDARD LVCMOS33} [get_ports {jchi[0]}]
set_property -dict {PACKAGE_PIN B10 IOSTANDARD LVCMOS33} [get_ports {jchi[1]}]
set_property -dict {PACKAGE_PIN E10 IOSTANDARD LVCMOS33} [get_ports {jchi[2]}]
set_property -dict {PACKAGE_PIN B12 IOSTANDARD LVCMOS33} [get_ports {jchi[3]}]

##Pmod Header JD
#set_property -dict {PACKAGE_PIN H4 IOSTANDARD LVCMOS33} [get_ports {jdlo[0]}]
#set_property -dict {PACKAGE_PIN H1 IOSTANDARD LVCMOS33} [get_ports {jdlo[1]}]
#set_property -dict {PACKAGE_PIN G1 IOSTANDARD LVCMOS33} [get_ports {jdlo[2]}]
#set_property -dict {PACKAGE_PIN G3 IOSTANDARD LVCMOS33} [get_ports {jdlo[3]}]
### Modem PCM digital audio interface is 1.8V, so we will need a level converter
#set_property -dict {PACKAGE_PIN H2 IOSTANDARD LVCMOS33} [get_ports {jdhi[0]}]
#set_property -dict {PACKAGE_PIN G4 IOSTANDARD LVCMOS33} [get_ports {jdhi[1]}]
#set_property -dict {PACKAGE_PIN G2 IOSTANDARD LVCMOS33} [get_ports {jdhi[2]}]
#set_property -dict {PACKAGE_PIN F3 IOSTANDARD LVCMOS33} [get_ports {jdhi[3]}]

##Pmod Header JXADC

## GPIO Interface
set_property -dict {PACKAGE_PIN AE26 IOSTANDARD LVCMOS18} [get_ports {gpio[0]}]
set_property -dict {PACKAGE_PIN AD26 IOSTANDARD LVCMOS18} [get_ports {gpio[1]}]
set_property -dict {PACKAGE_PIN AF25 IOSTANDARD LVCMOS18} [get_ports {gpio[2]}]
set_property -dict {PACKAGE_PIN AF24 IOSTANDARD LVCMOS18} [get_ports {gpio[3]}]
set_property -dict {PACKAGE_PIN AF23 IOSTANDARD LVCMOS18} [get_ports {gpio[4]}]
set_property -dict {PACKAGE_PIN AE23 IOSTANDARD LVCMOS18} [get_ports {gpio[5]}]
set_property -dict {PACKAGE_PIN AF22 IOSTANDARD LVCMOS18} [get_ports {gpio[6]}]
set_property -dict {PACKAGE_PIN AE22 IOSTANDARD LVCMOS18} [get_ports {gpio[7]}]

##VGA Connector
set_property -dict {PACKAGE_PIN B20 IOSTANDARD LVCMOS33} [get_ports {  vgared[0]}]
set_property -dict {PACKAGE_PIN C19 IOSTANDARD LVCMOS33} [get_ports {  vgared[1]}]
set_property -dict {PACKAGE_PIN A20 IOSTANDARD LVCMOS33} [get_ports {  vgared[2]}]
set_property -dict {PACKAGE_PIN A18 IOSTANDARD LVCMOS33} [get_ports {  vgared[3]}]
set_property -dict {PACKAGE_PIN B19 IOSTANDARD LVCMOS33} [get_ports {  vgared[4]}]
set_property -dict {PACKAGE_PIN A19 IOSTANDARD LVCMOS33} [get_ports {vgagreen[0]}]
set_property -dict {PACKAGE_PIN C17 IOSTANDARD LVCMOS33} [get_ports {vgagreen[1]}]
set_property -dict {PACKAGE_PIN E18 IOSTANDARD LVCMOS33} [get_ports {vgagreen[2]}]
set_property -dict {PACKAGE_PIN C18 IOSTANDARD LVCMOS33} [get_ports {vgagreen[3]}]
set_property -dict {PACKAGE_PIN C13 IOSTANDARD LVCMOS33} [get_ports {vgagreen[4]}]
set_property -dict {PACKAGE_PIN D18 IOSTANDARD LVCMOS33} [get_ports {vgagreen[5]}]
set_property -dict {PACKAGE_PIN A14 IOSTANDARD LVCMOS33} [get_ports { vgablue[0]}]
set_property -dict {PACKAGE_PIN D13 IOSTANDARD LVCMOS33} [get_ports { vgablue[1]}]
set_property -dict {PACKAGE_PIN D14 IOSTANDARD LVCMOS33} [get_ports { vgablue[2]}]
set_property -dict {PACKAGE_PIN A12 IOSTANDARD LVCMOS33} [get_ports { vgablue[3]}]
set_property -dict {PACKAGE_PIN A13 IOSTANDARD LVCMOS33} [get_ports { vgablue[4]}]
set_property -dict {PACKAGE_PIN C12 IOSTANDARD LVCMOS33} [get_ports hsync]
set_property -dict {PACKAGE_PIN C11 IOSTANDARD LVCMOS33} [get_ports vsync]

# HDMI output
############## HDMIOUT define##################
# set_property PACKAGE_PIN Y1 [get_ports TMDS_clk_n]
# set_property IOSTANDARD TMDS_33 [get_ports TMDS_clk_n]
# set_property PACKAGE_PIN W1 [get_ports TMDS_clk_p]
# set_property IOSTANDARD TMDS_33 [get_ports TMDS_clk_p]

# set_property PACKAGE_PIN AB1 [get_ports {TMDS_data_n[0]}]
# set_property IOSTANDARD TMDS_33 [get_ports {TMDS_data_n[0]}]
# set_property PACKAGE_PIN AA1 [get_ports {TMDS_data_p[0]}]
# set_property IOSTANDARD TMDS_33 [get_ports {TMDS_data_p[0]}]

# set_property PACKAGE_PIN AB2 [get_ports {TMDS_data_n[1]}]
# set_property IOSTANDARD TMDS_33 [get_ports {TMDS_data_n[1]}]
# set_property PACKAGE_PIN AB3 [get_ports {TMDS_data_p[1]}]
# set_property IOSTANDARD TMDS_33 [get_ports {TMDS_data_p[1]}]

# set_property PACKAGE_PIN AB5 [get_ports {TMDS_data_n[2]}]
# set_property IOSTANDARD TMDS_33 [get_ports {TMDS_data_n[2]}]
# set_property PACKAGE_PIN AA5 [get_ports {TMDS_data_p[2]}]
# set_property IOSTANDARD TMDS_33 [get_ports {TMDS_data_p[2]}]

# set_property -dict {PACKAGE_PIN A5 IOSTANDARD LVCMOS33} [get_ports hdmi_scl]
# set_property -dict {PACKAGE_PIN B4 IOSTANDARD LVCMOS33} [get_ports hdmi_sda]
# set_property -dict {PACKAGE_PIN M2 IOSTANDARD LVCMOS33} [get_ports hdmi_enable]
# set_property -dict {PACKAGE_PIN Y8 IOSTANDARD LVCMOS33} [get_ports hdmi_hotplugdetect]
# set_property -dict {PACKAGE_PIN W9 IOSTANDARD LVCMOS33} [get_ports hdmi_cec_a]


set_property -dict {PACKAGE_PIN AF7  IOSTANDARD LVCMOS18} [get_ports HDMI_I2C_SCL]
set_property -dict {PACKAGE_PIN AC8  IOSTANDARD LVCMOS18} [get_ports HDMI_I2C_SDA]
set_property -dict {PACKAGE_PIN AE7  IOSTANDARD LVCMOS18} [get_ports HDMI_TX_INT]

set_property -dict {PACKAGE_PIN AE18 IOSTANDARD LVCMOS18} [get_ports HDMI_I2S]
set_property -dict {PACKAGE_PIN AC18 IOSTANDARD LVCMOS18} [get_ports HDMI_LRCLK]
set_property -dict {PACKAGE_PIN AC16 IOSTANDARD LVCMOS18} [get_ports HDMI_MCLK]
set_property -dict {PACKAGE_PIN AF18 IOSTANDARD LVCMOS18} [get_ports HDMI_SCLK]

set_property -dict {PACKAGE_PIN AC13 IOSTANDARD LVCMOS18} [get_ports HDMI_TX_CLK]
set_property -dict {PACKAGE_PIN AE17 IOSTANDARD LVCMOS18} [get_ports HDMI_TX_DE]
set_property -dict {PACKAGE_PIN AF17 IOSTANDARD LVCMOS18} [get_ports HDMI_TX_HS]
set_property -dict {PACKAGE_PIN AB16 IOSTANDARD LVCMOS18} [get_ports HDMI_TX_VS]

set_property -dict {PACKAGE_PIN AD8  IOSTANDARD LVCMOS18} [get_ports {HDMI_TX_D[23]}]
set_property -dict {PACKAGE_PIN AE8  IOSTANDARD LVCMOS18} [get_ports {HDMI_TX_D[22]}]
set_property -dict {PACKAGE_PIN AF8  IOSTANDARD LVCMOS18} [get_ports {HDMI_TX_D[21]}]
set_property -dict {PACKAGE_PIN Ac9  IOSTANDARD LVCMOS18} [get_ports {HDMI_TX_D[20]}]
set_property -dict {PACKAGE_PIN AD9  IOSTANDARD LVCMOS18} [get_ports {HDMI_TX_D[19]}]
set_property -dict {PACKAGE_PIN AF10 IOSTANDARD LVCMOS18} [get_ports {HDMI_TX_D[18]}]
set_property -dict {PACKAGE_PIN AF9  IOSTANDARD LVCMOS18} [get_ports {HDMI_TX_D[17]}]
set_property -dict {PACKAGE_PIN AD10 IOSTANDARD LVCMOS18} [get_ports {HDMI_TX_D[16]}]
set_property -dict {PACKAGE_PIN AE10 IOSTANDARD LVCMOS18} [get_ports {HDMI_TX_D[15]}]
set_property -dict {PACKAGE_PIN AE12 IOSTANDARD LVCMOS18} [get_ports {HDMI_TX_D[14]}]
set_property -dict {PACKAGE_PIN AF12 IOSTANDARD LVCMOS18} [get_ports {HDMI_TX_D[13]}]
set_property -dict {PACKAGE_PIN AB11 IOSTANDARD LVCMOS18} [get_ports {HDMI_TX_D[12]}]
set_property -dict {PACKAGE_PIN AC11 IOSTANDARD LVCMOS18} [get_ports {HDMI_TX_D[11]}]
set_property -dict {PACKAGE_PIN AE13 IOSTANDARD LVCMOS18} [get_ports {HDMI_TX_D[10]}]
set_property -dict {PACKAGE_PIN Af13 IOSTANDARD LVCMOS18} [get_ports {HDMI_TX_D[9]}]
set_property -dict {PACKAGE_PIN AD13 IOSTANDARD LVCMOS18} [get_ports {HDMI_TX_D[8]}]
set_property -dict {PACKAGE_PIN AF14 IOSTANDARD LVCMOS18} [get_ports {HDMI_TX_D[7]}]
set_property -dict {PACKAGE_PIN AF15 IOSTANDARD LVCMOS18} [get_ports {HDMI_TX_D[6]}]
set_property -dict {PACKAGE_PIN AC14 IOSTANDARD LVCMOS18} [get_ports {HDMI_TX_D[5]}]
set_property -dict {PACKAGE_PIN AD14 IOSTANDARD LVCMOS18} [get_ports {HDMI_TX_D[4]}]
set_property -dict {PACKAGE_PIN AD15 IOSTANDARD LVCMOS18} [get_ports {HDMI_TX_D[3]}]
set_property -dict {PACKAGE_PIN AE15 IOSTANDARD LVCMOS18} [get_ports {HDMI_TX_D[2]}]
set_property -dict {PACKAGE_PIN AD16 IOSTANDARD LVCMOS18} [get_ports {HDMI_TX_D[1]}]
set_property -dict {PACKAGE_PIN AE16 IOSTANDARD LVCMOS18} [get_ports {HDMI_TX_D[0]}]

set_property PULLUP true [get_ports HDMI_I2C_SCL]
set_property PULLUP true [get_ports HDMI_I2C_SDA]
set_property PULLUP true [get_ports HDMI_TX_INT]

# Other things I don't yet know

# FPGA JTAG interface
set_property -dict {PACKAGE_PIN R6 IOSTANDARD LVCMOS33} [get_ports fpga_tdi]
set_property -dict {PACKAGE_PIN R7 IOSTANDARD LVCMOS33} [get_ports fpga_tdo]
set_property -dict {PACKAGE_PIN L8 IOSTANDARD LVCMOS33} [get_ports fpga_tck]
set_property -dict {PACKAGE_PIN N8 IOSTANDARD LVCMOS33} [get_ports fpga_tms]
# set_property -dict {PACKAGE_PIN P6 IOSTANDARD LVCMOS33} [get_ports fpga_init]


# set_property -dict {PACKAGE_PIN G21 IOSTANDARD LVCMOS33} [get_ports grove_scl]
# set_property -dict {PACKAGE_PIN G22 IOSTANDARD LVCMOS33} [get_ports grove_sda]

# PWM Audio
#
set_property -dict {PACKAGE_PIN A8 IOSTANDARD LVCMOS33 } [get_ports pwm_l]
set_property -dict {PACKAGE_PIN A9 IOSTANDARD LVCMOS33 } [get_ports pwm_r]

## Micro SD Connector  --  No RESET/CS pin ???  Or is it sd_dat[3] ???
set_property -dict { PACKAGE_PIN D26 IOSTANDARD LVCMOS33 } [get_ports sdCD]
set_property -dict { PACKAGE_PIN B26 IOSTANDARD LVCMOS33 } [get_ports sdClock]
# set_property -dict { PACKAGE_PIN B25 IOSTANDARD LVCMOS33 } [get_ports sdMISO]
# set_property -dict { PACKAGE_PIN F25 IOSTANDARD LVCMOS33 } [get_ports sdMOSI]
set_property -dict { PACKAGE_PIN F25 IOSTANDARD LVCMOS33 } [get_ports sdCMD]
set_property -dict { PACKAGE_PIN B25 IOSTANDARD LVCMOS33 } [get_ports {sd_dat[0]}]
set_property -dict { PACKAGE_PIN C26 IOSTANDARD LVCMOS33 } [get_ports {sd_dat[1]}]
set_property -dict { PACKAGE_PIN D25 IOSTANDARD LVCMOS33 } [get_ports {sd_dat[2]}]
set_property -dict { PACKAGE_PIN E26 IOSTANDARD LVCMOS33 } [get_ports {sd_dat[3]}]

##Accelerometer
#set_property -dict {PACKAGE_PIN E15 IOSTANDARD LVCMOS33} [get_ports aclMISO]
#set_property -dict {PACKAGE_PIN F14 IOSTANDARD LVCMOS33} [get_ports aclMOSI]
#set_property -dict {PACKAGE_PIN F15 IOSTANDARD LVCMOS33} [get_ports aclSCK]
#set_property -dict {PACKAGE_PIN D15 IOSTANDARD LVCMOS33} [get_ports aclSS]
#set_property -dict {PACKAGE_PIN B13 IOSTANDARD LVCMOS33} [get_ports aclInt1]
#set_property -dict {PACKAGE_PIN C16 IOSTANDARD LVCMOS33} [get_ports aclInt2]

##Temperature Sensor
#set_property -dict {PACKAGE_PIN C14 IOSTANDARD LVCMOS33} [get_ports tmpSCL]
#set_property -dict {PACKAGE_PIN C15 IOSTANDARD LVCMOS33} [get_ports tmpSDA]
#set_property -dict {PACKAGE_PIN D13 IOSTANDARD LVCMOS33} [get_ports tmpInt]
#set_property -dict {PACKAGE_PIN B14 IOSTANDARD LVCMOS33} [get_ports tmpCT]

##Omnidirectional Microphone
#set_property -dict {PACKAGE_PIN J5 IOSTANDARD LVCMOS33} [get_ports micClk]
#set_property -dict {PACKAGE_PIN H5 IOSTANDARD LVCMOS33} [get_ports micData]
#set_property -dict {PACKAGE_PIN F5 IOSTANDARD LVCMOS33} [get_ports micLRSel]

##PWM Audio Amplifier
#set_property -dict {PACKAGE_PIN A11 IOSTANDARD LVCMOS33} [get_ports ampPWM]
#set_property -dict {PACKAGE_PIN D12 IOSTANDARD LVCMOS33} [get_ports ampSD]

##USB-RS232 Interface
#set_property -dict {PACKAGE_PIN C4 IOSTANDARD LVCMOS33} [get_ports RsRx]
#set_property -dict {PACKAGE_PIN D4 IOSTANDARD LVCMOS33} [get_ports UART_TXD]
#set_property -dict { PACKAGE_PIN D3 IOSTANDARD LVCMOS33 } [get_ports RsCts]
#set_property -dict { PACKAGE_PIN E5 IOSTANDARD LVCMOS33 } [get_ports RsRts]

##USB HID (PS/2)
#set_property PACKAGE_PIN F4 [get_ports ps2clk]
#set_property IOSTANDARD LVCMOS33 [get_ports ps2clk]
#set_property PULLUP true [get_ports ps2clk]
#set_property PACKAGE_PIN B2 [get_ports ps2data]
#set_property IOSTANDARD LVCMOS33 [get_ports ps2data]
#set_property PULLUP true [get_ports ps2data]

##SMSC Ethernet PHY
#set_property -dict {PACKAGE_PIN C9 IOSTANDARD LVCMOS33} [get_ports eth_mdc]
#set_property -dict {PACKAGE_PIN A9 IOSTANDARD LVCMOS33} [get_ports eth_mdio]
#set_property -dict {PACKAGE_PIN B3 IOSTANDARD LVCMOS33} [get_ports eth_reset]
## the below eth_rxdv was called PhyCrs, unsure if this is correct, need to check
#set_property -dict {PACKAGE_PIN D9 IOSTANDARD LVCMOS33} [get_ports eth_rxdv]
#set_property -dict {PACKAGE_PIN C10 IOSTANDARD LVCMOS33} [get_ports eth_rxer]
#set_property -dict {PACKAGE_PIN C11 IOSTANDARD LVCMOS33} [get_ports {eth_rxd[0]}]
#set_property -dict {PACKAGE_PIN D10 IOSTANDARD LVCMOS33} [get_ports {eth_rxd[1]}]
#set_property -dict {PACKAGE_PIN B9 IOSTANDARD LVCMOS33} [get_ports eth_txen]
#set_property -dict {PACKAGE_PIN A10 IOSTANDARD LVCMOS33} [get_ports {eth_txd[0]}]
#set_property -dict {PACKAGE_PIN A8 IOSTANDARD LVCMOS33} [get_ports {eth_txd[1]}]
#set_property -dict {PACKAGE_PIN D5 IOSTANDARD LVCMOS33} [get_ports eth_clock]
#set_property -dict {PACKAGE_PIN B8 IOSTANDARD LVCMOS33} [get_ports eth_interrupt]

##Quad SPI Flash
# set_property -dict {PACKAGE_PIN C8  IOSTANDARD LVCMOS33} [get_ports QspiSCK]   ## Dedicated CCLK_0 Config clock pin, don't declare it again
set_property -dict {PACKAGE_PIN C23 IOSTANDARD LVTTL} [get_ports QspiCSn]
set_property -dict {PACKAGE_PIN B24 IOSTANDARD LVTTL} [get_ports {QspiDB[0]}]
set_property -dict {PACKAGE_PIN A25 IOSTANDARD LVTTL} [get_ports {QspiDB[1]}]
set_property -dict {PACKAGE_PIN B22 IOSTANDARD LVTTL} [get_ports {QspiDB[2]}]
set_property -dict {PACKAGE_PIN A22 IOSTANDARD LVTTL} [get_ports {QspiDB[3]}]


#set_false_path -from [get_clocks -of_objects [get_pins dotclock1/mmcm_adv_inst/CLKOUT2]] -to [get_clocks -of_objects [get_pins dotclock1/mmcm_adv_inst/CLKOUT3]]
#set_false_path -from [get_clocks -of_objects [get_pins dotclock1/mmcm_adv_inst/CLKOUT3]] -to [get_clocks -of_objects [get_pins dotclock1/mmcm_adv_inst/CLKOUT0]]
set_false_path -from [get_pins {machine0/viciv0/bitplanes_x_start_reg[2]/C}] -to [get_pins machine0/viciv0/vicii_sprites0/bitplanes0/x_in_bitplanes_reg/D]
# set_false_path -from [get_pins {jblo_reg[3]/C}] -to [get_pins {machine0/pmodb_in_buffer_reg[2]/D}]
# set_false_path -from [get_pins {jblo_reg[4]/C}] -to [get_pins {machine0/pmodb_in_buffer_reg[3]/D}]
set_false_path -from [get_pins machine0/iomapper0/block4b.c65uart0/reg_status3_rx_framing_error_reg/C] -to [get_pins {machine0/cpu0/read_data_copy_reg[3]/D}]
set_false_path -from [get_pins machine0/iomapper0/block4b.c65uart0/reg_status0_rx_full_reg/C] -to [get_pins {machine0/cpu0/read_data_copy_reg[0]/D}]
set_false_path -from [get_pins {machine0/viciv0/vicii_sprite_bitmap_collisions_reg[6]/C}] -to [get_pins {machine0/cpu0/read_data_copy_reg[6]/D}]
set_false_path -from [get_pins {machine0/viciv0/vicii_sprite_sprite_collisions_reg[5]/C}] -to [get_pins {machine0/cpu0/read_data_copy_reg[5]/D}]
set_false_path -from [get_pins {machine0/viciv0/vicii_sprites0/bitplanes0/v_bitplane_y_start_reg[5]/C}] -to [get_pins {machine0/viciv0/vicii_sprites0/bitplanes0/p_0_out/A[12]}]
set_false_path -from [get_pins {machine0/viciv0/vicii_sprites0/bitplanes0/v_bitplane_y_start_reg[5]/C}] -to [get_pins {machine0/viciv0/vicii_sprites0/bitplanes0/p_0_out/A[13]}]
set_false_path -from [get_pins {machine0/viciv0/vicii_sprites0/bitplanes0/v_bitplane_y_start_reg[5]/C}] -to [get_pins {machine0/viciv0/vicii_sprites0/bitplanes0/p_0_out/A[13]}]
set_false_path -from [get_pins {machine0/viciv0/vicii_sprites0/bitplanes0/v_bitplane_y_start_reg[5]/C}] -to [get_pins {machine0/viciv0/vicii_sprites0/bitplanes0/p_0_out/A[15]}]
set_false_path -from [get_pins {machine0/viciv0/vicii_sprites0/bitplanes0/v_bitplane_y_start_reg[5]/C}] -to [get_pins {machine0/viciv0/vicii_sprites0/bitplanes0/p_0_out/A[14]}]
set_false_path -from [get_pins {machine0/viciv0/vicii_sprites0/bitplanes0/v_bitplane_y_start_reg[5]/C}] -to [get_pins {machine0/viciv0/vicii_sprites0/bitplanes0/p_0_out/A[5]}]
set_false_path -from [get_pins {machine0/viciv0/vicii_sprites0/bitplanes0/v_bitplane_y_start_reg[5]/C}] -to [get_pins {machine0/viciv0/vicii_sprites0/bitplanes0/p_0_out/A[10]}]


set_false_path -from [get_pins machine0/iomapper0/block2.framepacker0/buffer_moby_toggle_reg/C] -to [get_pins {machine0/iomapper0/ethernet0/FSM_onehot_eth_tx_state_reg[0]/CE}]

##################################################################################################
## Controller 0
## Memory Device: DDR3_SDRAM->Components->MT41K128M16XX-15E
## Data Width: 16
## Time Period: 2500
## Data Mask: 1
##################################################################################################

set_property IO_BUFFER_TYPE NONE [get_ports {ddr3_ck_n[*]} ]
set_property IO_BUFFER_TYPE NONE [get_ports {ddr3_ck_p[*]} ]

#create_clock -period 5 [get_ports sys_clk_i]

############## NET - IOSTANDARD ##################


# PadFunction: IO_L10P_T1_34
set_property -dict {VCCAUX_IO NORMAL SLEW FAST IOSTANDARD SSTL15_T_DCI PACKAGE_PIN W1} [get_ports {ddr3_dq[0]}]

# PadFunction: IO_L8P_T1_34
set_property -dict {VCCAUX_IO NORMAL SLEW FAST IOSTANDARD SSTL15_T_DCI PACKAGE_PIN V2} [get_ports {ddr3_dq[1]}]

# PadFunction: IO_L10N_T1_34
set_property -dict {VCCAUX_IO NORMAL SLEW FAST IOSTANDARD SSTL15_T_DCI PACKAGE_PIN Y1} [get_ports {ddr3_dq[2]}]

# PadFunction: IO_L7P_T1_34
set_property -dict {VCCAUX_IO NORMAL SLEW FAST IOSTANDARD SSTL15_T_DCI PACKAGE_PIN Y3} [get_ports {ddr3_dq[3]}]

# PadFunction: IO_L11N_T1_SRCC_34
set_property -dict {VCCAUX_IO NORMAL SLEW FAST IOSTANDARD SSTL15_T_DCI PACKAGE_PIN AC2} [get_ports {ddr3_dq[4]}]

# PadFunction: IO_L7N_T1_34
set_property -dict {VCCAUX_IO NORMAL SLEW FAST IOSTANDARD SSTL15_T_DCI PACKAGE_PIN Y2} [get_ports {ddr3_dq[5]}]

# PadFunction: IO_L11P_T1_SRCC_34
set_property -dict {VCCAUX_IO NORMAL SLEW FAST IOSTANDARD SSTL15_T_DCI PACKAGE_PIN AB2} [get_ports {ddr3_dq[6]}]

# PadFunction: IO_L12P_T1_MRCC_34
set_property -dict {VCCAUX_IO NORMAL SLEW FAST IOSTANDARD SSTL15_T_DCI PACKAGE_PIN AA3} [get_ports {ddr3_dq[7]}]

# PadFunction: IO_L2N_T0_34
set_property -dict {VCCAUX_IO NORMAL SLEW FAST IOSTANDARD SSTL15_T_DCI PACKAGE_PIN U1} [get_ports {ddr3_dq[8]}]

# PadFunction: IO_L6P_T0_34
set_property -dict {VCCAUX_IO NORMAL SLEW FAST IOSTANDARD SSTL15_T_DCI PACKAGE_PIN V4} [get_ports {ddr3_dq[9]}]

# PadFunction: IO_L1P_T0_34
set_property -dict {VCCAUX_IO NORMAL SLEW FAST IOSTANDARD SSTL15_T_DCI PACKAGE_PIN U6} [get_ports {ddr3_dq[10]}]

# PadFunction: IO_L4N_T0_34
set_property -dict {VCCAUX_IO NORMAL SLEW FAST IOSTANDARD SSTL15_T_DCI PACKAGE_PIN W3} [get_ports {ddr3_dq[11]}]

# PadFunction: IO_L5N_T0_34
set_property -dict {VCCAUX_IO NORMAL SLEW FAST IOSTANDARD SSTL15_T_DCI PACKAGE_PIN V6} [get_ports {ddr3_dq[12]}]

# PadFunction: IO_L2P_T0_34
set_property -dict {VCCAUX_IO NORMAL SLEW FAST IOSTANDARD SSTL15_T_DCI PACKAGE_PIN U2} [get_ports {ddr3_dq[13]}]

# PadFunction: IO_L5P_T0_34
set_property -dict {VCCAUX_IO NORMAL SLEW FAST IOSTANDARD SSTL15_T_DCI PACKAGE_PIN U7} [get_ports {ddr3_dq[14]}]

# PadFunction: IO_L1N_T0_34
set_property -dict {VCCAUX_IO NORMAL SLEW FAST IOSTANDARD SSTL15_T_DCI PACKAGE_PIN U5} [get_ports {ddr3_dq[15]}]

# PadFunction: IO_L17P_T2_34
set_property -dict {VCCAUX_IO NORMAL SLEW FAST IOSTANDARD SSTL15 PACKAGE_PIN Y6} [get_ports {ddr3_addr[13]}]

# PadFunction: IO_L13N_T2_MRCC_34
set_property -dict {VCCAUX_IO NORMAL SLEW FAST IOSTANDARD SSTL15 PACKAGE_PIN AB4} [get_ports {ddr3_addr[12]}]

# PadFunction: IO_L18N_T2_34
set_property -dict {VCCAUX_IO NORMAL SLEW FAST IOSTANDARD SSTL15 PACKAGE_PIN AD5} [get_ports {ddr3_addr[11]}]

# PadFunction: IO_L22P_T3_34
set_property -dict {VCCAUX_IO NORMAL SLEW FAST IOSTANDARD SSTL15 PACKAGE_PIN AE3} [get_ports {ddr3_addr[10]}]

# PadFunction: IO_L23P_T3_34
set_property -dict {VCCAUX_IO NORMAL SLEW FAST IOSTANDARD SSTL15 PACKAGE_PIN AE6} [get_ports {ddr3_addr[9]}]

# PadFunction: IO_L13P_T2_MRCC_34
set_property -dict {VCCAUX_IO NORMAL SLEW FAST IOSTANDARD SSTL15 PACKAGE_PIN AA4} [get_ports {ddr3_addr[8]}]

# PadFunction: IO_L17N_T2_34
set_property -dict {VCCAUX_IO NORMAL SLEW FAST IOSTANDARD SSTL15 PACKAGE_PIN Y5} [get_ports {ddr3_addr[7]}]

# PadFunction: IO_L22N_T3_34
set_property -dict {VCCAUX_IO NORMAL SLEW FAST IOSTANDARD SSTL15 PACKAGE_PIN AE2} [get_ports {ddr3_addr[6]}]

# PadFunction: IO_L16P_T2_34
set_property -dict {VCCAUX_IO NORMAL SLEW FAST IOSTANDARD SSTL15 PACKAGE_PIN AB6} [get_ports {ddr3_addr[5]}]

# PadFunction: IO_L19P_T3_34
set_property -dict {VCCAUX_IO NORMAL SLEW FAST IOSTANDARD SSTL15 PACKAGE_PIN AD4} [get_ports {ddr3_addr[4]}]

# PadFunction: IO_L16N_T2_34
set_property -dict {VCCAUX_IO NORMAL SLEW FAST IOSTANDARD SSTL15 PACKAGE_PIN AC6} [get_ports {ddr3_addr[3]}]

# PadFunction: IO_L18P_T2_34
set_property -dict {VCCAUX_IO NORMAL SLEW FAST IOSTANDARD SSTL15 PACKAGE_PIN AD6} [get_ports {ddr3_addr[2]}]

# PadFunction: IO_L24N_T3_34
set_property -dict {VCCAUX_IO NORMAL SLEW FAST IOSTANDARD SSTL15 PACKAGE_PIN AF2} [get_ports {ddr3_addr[1]}]

# PadFunction: IO_L21P_T3_DQS_34
set_property -dict {VCCAUX_IO NORMAL SLEW FAST IOSTANDARD SSTL15 PACKAGE_PIN AF5} [get_ports {ddr3_addr[0]}]

# PadFunction: IO_L23N_T3_34
set_property -dict {VCCAUX_IO NORMAL SLEW FAST IOSTANDARD SSTL15 PACKAGE_PIN AE5} [get_ports {ddr3_ba[2]}]

# PadFunction: IO_L20N_T3_34
set_property -dict {VCCAUX_IO NORMAL SLEW FAST IOSTANDARD SSTL15 PACKAGE_PIN AE1} [get_ports {ddr3_ba[1]}]

# PadFunction: IO_L19N_T3_VREF_34
set_property -dict {VCCAUX_IO NORMAL SLEW FAST IOSTANDARD SSTL15 PACKAGE_PIN AD3} [get_ports {ddr3_ba[0]}]

# PadFunction: IO_L14N_T2_SRCC_34
set_property -dict {VCCAUX_IO NORMAL SLEW FAST IOSTANDARD SSTL15 PACKAGE_PIN AC3} [get_ports {ddr3_ras_n}]

# PadFunction: IO_L14P_T2_SRCC_34
set_property -dict {VCCAUX_IO NORMAL SLEW FAST IOSTANDARD SSTL15 PACKAGE_PIN AC4} [get_ports {ddr3_cas_n}]

# PadFunction: IO_L21N_T3_DQS_34
set_property -dict {VCCAUX_IO NORMAL SLEW FAST IOSTANDARD SSTL15 PACKAGE_PIN AF4} [get_ports {ddr3_we_n}]

# PadFunction: IO_L6N_T0_VREF_34
set_property -dict {VCCAUX_IO NORMAL SLEW FAST IOSTANDARD LVCMOS15 PACKAGE_PIN W4} [get_ports {ddr3_reset_n}]

# PadFunction: IO_L20P_T3_34
set_property -dict {VCCAUX_IO NORMAL SLEW FAST IOSTANDARD SSTL15 PACKAGE_PIN AD1} [get_ports {ddr3_cke[0]}]

# PadFunction: IO_L24P_T3_34
set_property -dict {VCCAUX_IO NORMAL SLEW FAST IOSTANDARD SSTL15 PACKAGE_PIN AF3} [get_ports {ddr3_odt[0]}]

# PadFunction: IO_L8N_T1_34
set_property -dict {VCCAUX_IO NORMAL SLEW FAST IOSTANDARD SSTL15 PACKAGE_PIN V1} [get_ports {ddr3_dm[0]}]

# PadFunction: IO_L4P_T0_34
set_property -dict {VCCAUX_IO NORMAL SLEW FAST IOSTANDARD SSTL15 PACKAGE_PIN V3} [get_ports {ddr3_dm[1]}]

# PadFunction: IO_L9P_T1_DQS_34
set_property -dict {VCCAUX_IO NORMAL SLEW FAST IOSTANDARD DIFF_SSTL15_T_DCI PACKAGE_PIN AB1} [get_ports {ddr3_dqs_p[0]}]

# PadFunction: IO_L9N_T1_DQS_34
set_property -dict {VCCAUX_IO NORMAL SLEW FAST IOSTANDARD DIFF_SSTL15_T_DCI PACKAGE_PIN AC1} [get_ports {ddr3_dqs_n[0]}]

# PadFunction: IO_L3P_T0_DQS_34
set_property -dict {VCCAUX_IO NORMAL SLEW FAST IOSTANDARD DIFF_SSTL15_T_DCI PACKAGE_PIN W6} [get_ports {ddr3_dqs_p[1]}]

# PadFunction: IO_L3N_T0_DQS_34
set_property -dict {VCCAUX_IO NORMAL SLEW FAST IOSTANDARD DIFF_SSTL15_T_DCI PACKAGE_PIN W5} [get_ports {ddr3_dqs_n[1]}]

# PadFunction: IO_L15P_T2_DQS_34
set_property -dict {VCCAUX_IO NORMAL SLEW FAST IOSTANDARD DIFF_SSTL15 PACKAGE_PIN AA5} [get_ports {ddr3_ck_p[0]}]

# PadFunction: IO_L15N_T2_DQS_34
set_property -dict {VCCAUX_IO NORMAL SLEW FAST IOSTANDARD DIFF_SSTL15 PACKAGE_PIN AB5} [get_ports {ddr3_ck_n[0]}]


set_property INTERNAL_VREF  0.750 [get_iobanks 34]


set_property LOC PHASER_OUT_PHY_X1Y9 [get_cells  -hier -filter {NAME =~ */ddr_phy_4lanes_0.u_ddr_phy_4lanes/ddr_byte_lane_B.ddr_byte_lane_B/phaser_out}]
set_property LOC PHASER_OUT_PHY_X1Y8 [get_cells  -hier -filter {NAME =~ */ddr_phy_4lanes_0.u_ddr_phy_4lanes/ddr_byte_lane_A.ddr_byte_lane_A/phaser_out}]
set_property LOC PHASER_OUT_PHY_X1Y11 [get_cells  -hier -filter {NAME =~ */ddr_phy_4lanes_0.u_ddr_phy_4lanes/ddr_byte_lane_D.ddr_byte_lane_D/phaser_out}]
set_property LOC PHASER_OUT_PHY_X1Y10 [get_cells  -hier -filter {NAME =~ */ddr_phy_4lanes_0.u_ddr_phy_4lanes/ddr_byte_lane_C.ddr_byte_lane_C/phaser_out}]


## set_property LOC PHASER_IN_PHY_X1Y9 [get_cells  -hier -filter {NAME =~ */ddr_phy_4lanes_0.u_ddr_phy_4lanes/ddr_byte_lane_B.ddr_byte_lane_B/phaser_in_gen.phaser_in}]
## set_property LOC PHASER_IN_PHY_X1Y8 [get_cells  -hier -filter {NAME =~ */ddr_phy_4lanes_0.u_ddr_phy_4lanes/ddr_byte_lane_A.ddr_byte_lane_A/phaser_in_gen.phaser_in}]
set_property LOC PHASER_IN_PHY_X1Y11 [get_cells  -hier -filter {NAME =~ */ddr_phy_4lanes_0.u_ddr_phy_4lanes/ddr_byte_lane_D.ddr_byte_lane_D/phaser_in_gen.phaser_in}]
set_property LOC PHASER_IN_PHY_X1Y10 [get_cells  -hier -filter {NAME =~ */ddr_phy_4lanes_0.u_ddr_phy_4lanes/ddr_byte_lane_C.ddr_byte_lane_C/phaser_in_gen.phaser_in}]





set_property LOC OUT_FIFO_X1Y9 [get_cells  -hier -filter {NAME =~ */ddr_phy_4lanes_0.u_ddr_phy_4lanes/ddr_byte_lane_B.ddr_byte_lane_B/out_fifo}]
set_property LOC OUT_FIFO_X1Y8 [get_cells  -hier -filter {NAME =~ */ddr_phy_4lanes_0.u_ddr_phy_4lanes/ddr_byte_lane_A.ddr_byte_lane_A/out_fifo}]
set_property LOC OUT_FIFO_X1Y11 [get_cells  -hier -filter {NAME =~ */ddr_phy_4lanes_0.u_ddr_phy_4lanes/ddr_byte_lane_D.ddr_byte_lane_D/out_fifo}]
set_property LOC OUT_FIFO_X1Y10 [get_cells  -hier -filter {NAME =~ */ddr_phy_4lanes_0.u_ddr_phy_4lanes/ddr_byte_lane_C.ddr_byte_lane_C/out_fifo}]


set_property LOC IN_FIFO_X1Y11 [get_cells  -hier -filter {NAME =~ */ddr_phy_4lanes_0.u_ddr_phy_4lanes/ddr_byte_lane_D.ddr_byte_lane_D/in_fifo_gen.in_fifo}]
set_property LOC IN_FIFO_X1Y10 [get_cells  -hier -filter {NAME =~ */ddr_phy_4lanes_0.u_ddr_phy_4lanes/ddr_byte_lane_C.ddr_byte_lane_C/in_fifo_gen.in_fifo}]


set_property LOC PHY_CONTROL_X1Y2 [get_cells  -hier -filter {NAME =~ */ddr_phy_4lanes_0.u_ddr_phy_4lanes/phy_control_i}]


set_property LOC PHASER_REF_X1Y2 [get_cells  -hier -filter {NAME =~ */ddr_phy_4lanes_0.u_ddr_phy_4lanes/phaser_ref_i}]


set_property LOC OLOGIC_X1Y143 [get_cells  -hier -filter {NAME =~ */ddr_phy_4lanes_0.u_ddr_phy_4lanes/ddr_byte_lane_D.ddr_byte_lane_D/ddr_byte_group_io/*slave_ts}]
set_property LOC OLOGIC_X1Y131 [get_cells  -hier -filter {NAME =~ */ddr_phy_4lanes_0.u_ddr_phy_4lanes/ddr_byte_lane_C.ddr_byte_lane_C/ddr_byte_group_io/*slave_ts}]



set_property LOC PLLE2_ADV_X1Y2 [get_cells -hier -filter {NAME =~ */u_ddr3_infrastructure/plle2_i}]
set_property LOC MMCME2_ADV_X1Y2 [get_cells -hier -filter {NAME =~ */u_ddr3_infrastructure/gen_mmcm.mmcm_i}]



set_multicycle_path -from [get_cells -hier -filter {NAME =~ */mc0/mc_read_idle_r_reg}] \
                    -to   [get_cells -hier -filter {NAME =~ */input_[?].iserdes_dq_.iserdesdq}] \
                    -setup 6

set_multicycle_path -from [get_cells -hier -filter {NAME =~ */mc0/mc_read_idle_r_reg}] \
                    -to   [get_cells -hier -filter {NAME =~ */input_[?].iserdes_dq_.iserdesdq}] \
                    -hold 5

set_false_path -through [get_pins -filter {NAME =~ */DQSFOUND} -of [get_cells -hier -filter {REF_NAME == PHASER_IN_PHY}]]

set_multicycle_path -through [get_pins -filter {NAME =~ */OSERDESRST} -of [get_cells -hier -filter {REF_NAME == PHASER_OUT_PHY}]] -setup 2 -start
set_multicycle_path -through [get_pins -filter {NAME =~ */OSERDESRST} -of [get_cells -hier -filter {REF_NAME == PHASER_OUT_PHY}]] -hold 1 -start

#set_max_delay -datapath_only -from [get_cells -hier -filter {NAME =~ *temp_mon_enabled.u_tempmon/* && IS_SEQUENTIAL}] -to [get_cells -hier -filter {NAME =~ *temp_mon_enabled.u_tempmon/device_temp_sync_r1*}] 20
set_max_delay -to [get_pins -hier -include_replicated_objects -filter {NAME =~ *temp_mon_enabled.u_tempmon/device_temp_sync_r1_reg[*]/D}] 20
set_max_delay -from [get_cells -hier *rstdiv0_sync_r1_reg*] -to [get_pins -filter {NAME =~ */RESET} -of [get_cells -hier -filter {REF_NAME == PHY_CONTROL}]] -datapath_only 5
#set_false_path -through [get_pins -hier -filter {NAME =~ */u_iodelay_ctrl/sys_rst}]
set_false_path -through [get_nets -hier -filter {NAME =~ */u_iodelay_ctrl/sys_rst_i}]

set_max_delay -datapath_only -from [get_cells -hier -filter {NAME =~ *ddr3_infrastructure/rstdiv0_sync_r1_reg*}] -to [get_cells -hier -filter {NAME =~ *temp_mon_enabled.u_tempmon/xadc_supplied_temperature.rst_r1*}] 20


set_property BITSTREAM.GENERAL.COMPRESS TRUE [current_design]
set_property BITSTREAM.CONFIG.CONFIGRATE 33 [current_design]
set_property CONFIG_VOLTAGE 3.3 [current_design]
set_property CFGBVS VCCO [current_design]
set_property CONFIG_MODE SPIx4 [current_design]
set_property BITSTREAM.CONFIG.SPI_32BIT_ADDR YES [current_design]
set_property BITSTREAM.CONFIG.SPI_BUSWIDTH {4} [current_design]
