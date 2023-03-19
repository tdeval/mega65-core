#!/bin/env python3

from vunit import VUnit

# Create VUnit instance by parsing command line arguments
vu = VUnit.from_argv()

# Create library 'unisim'
unisim = vu.add_library("unisim")

# Add all files ending in .vhd in current working directory to library
unisim.add_source_files("/opt/Xilinx/Vivado/2022.2/data/vhdl/src/unisims/primitive/*.vhd")
unisim.add_source_files("/opt/Xilinx/Vivado/2022.2/data/vhdl/src/unisims/unisim_VCOMP.vhd")
unisim.add_source_files("/opt/Xilinx/Vivado/2022.2/data/vhdl/src/unisims/unisim_VPKG.vhd")

# Create library 'work'
lib = vu.add_library("lib")

# Add all files ending in .vhd in current working directory to library
lib.add_source_files("src/vhdl/tb_qspi_mx25l51245g.vhdl")
lib.add_source_files("src/vhdl/conversions.vhdl")
lib.add_source_files("src/vhdl/gen_utils.vhdl")
lib.add_source_files("src/vhdl/cputypes.vhdl")
lib.add_source_files("src/vhdl/debugtools.vhdl")
lib.add_source_files("src/vhdl/crc1581.vhdl")
lib.add_source_files("src/vhdl/ghdl_ram8x4096_sync.vhdl")
lib.add_source_files("src/vhdl/i2c_master.vhdl")
lib.add_source_files("src/vhdl/icape2sim.vhdl")
lib.add_source_files("src/vhdl/mfm_bits_to_bytes.vhdl")
lib.add_source_files("src/vhdl/mfm_bits_to_gaps.vhdl")
lib.add_source_files("src/vhdl/mfm_gaps.vhdl")
lib.add_source_files("src/vhdl/mfm_quantise_gaps.vhdl")
lib.add_source_files("src/vhdl/mfm_gaps_to_bits.vhdl")
lib.add_source_files("src/vhdl/rll27_quantise_gaps.vhdl")
lib.add_source_files("src/vhdl/rll27_gaps_to_bits.vhdl")
lib.add_source_files("src/vhdl/mfm_decoder.vhdl")
lib.add_source_files("src/vhdl/raw_bits_to_gaps.vhdl")
lib.add_source_files("src/vhdl/reconfig.vhdl")
lib.add_source_files("src/vhdl/rll27_bits_to_gaps.vhdl")
lib.add_source_files("src/vhdl/sdcard.vhdl")
lib.add_source_files("src/vhdl/touch.vhdl")
# lib.add_source_files("src/vhdl/test_qspi_mx25l51245g.vhdl")
lib.add_source_files("src/vhdl/vfpga/vfpga_clock_controller_pausable.vhdl")
# lib.add_source_files("src/vhdl/sdcardio_noreport.vhdl")
lib.add_source_files("src/vhdl/sdcardio_mx25l.vhdl")
lib.add_source_files("src/vhdl/s25fl512s.vhd")
# lib.add_source_files("src/vhdl/tb_pixel_driver.vhdl")
# lib.add_source_files("src/vhdl/kb_matrix_ram.vhdl")
# # lib.add_source_files("src/vhdl/debugtools.vhdl")
# lib.add_source_files("src/vhdl/mega65kbd_to_matrix.vhdl")
# lib.add_source_files("i2c_slave/rtl/vhdl/*.vhdl")
# lib.add_source_files("src/vhdl/pixel_driver.vhdl")
# lib.add_source_files("src/vhdl/frame_generator.vhdl")
# lib.add_source_files("src/vhdl/ghdl_ram32x1024_sync.vhdl")

vu.set_compile_option("ghdl.a_flags", ["-frelaxed-rules", "-fsynopsys"])
vu.set_compile_option("ghdl.flags", ["-frelaxed-rules"])
vu.set_sim_option("ghdl.elab_flags", ["-frelaxed-rules"])
vu.set_sim_option("ghdl.sim_flags", ["--ieee-asserts=disable"])

# Run vunit function
vu.main()
