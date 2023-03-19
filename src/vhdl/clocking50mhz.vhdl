library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use IEEE.numeric_std.ALL;

library unisim;
use unisim.vcomponents.all;

entity clocking50mhz is
   port (
      -- Clock in ports
      clk_in     : in  std_logic;

      -- Clock out ports
      clock27    : out std_logic;
      clock41    : out std_logic;
      clock50    : out std_logic;
      clock81p   : out std_logic;
      clock163   : out std_logic;
      clock200   : out std_logic;
      clock270   : out std_logic;
      clock325   : out std_logic
   );
end entity;


architecture RTL of clocking50mhz is

  signal clk_fb     : std_logic := '0';
  signal clk_fb_adjust0     : std_logic := '0';
  signal clk_fb_eth : std_logic := '0';
  signal clock10125mhz : std_logic := '0';
  signal u_clock10125mhz : std_logic := '0';

  signal u_clock27 : std_logic := '0';
  signal u_clock41 : std_logic := '0';
  signal u_clock50 : std_logic := '0';
  signal u_clock81p : std_logic := '0';
  signal u_clock100 : std_logic := '0';
  signal u_clock135p : std_logic := '0';
  signal u_clock163 : std_logic := '0';
  signal u_clock200 : std_logic := '0';
  signal u_clock270 : std_logic := '0';
  signal u_clock325 : std_logic := '0';

begin

  bufg_inter_connect10125:
  bufg port map ( I => u_clock10125mhz,
                  O => clock10125mhz);

  bufg27:
  bufg port map ( I => u_clock27,
                  O => clock27);

  bufg41:
  bufg port map ( I => u_clock41,
                  O => clock41);

  bufg50:
  bufg port map ( I => u_clock50,
                  O => clock50);

  bufg81:
  bufg port map ( I => u_clock81p,
                  O => clock81p);

  bufg163:
  bufg port map ( I => u_clock163,
                  O => clock163);

  bufg200:
  bufg port map ( I => u_clock200,
                  O => clock200);

  bufg270:
  bufg port map ( I => u_clock270,
                  O => clock270);

  bufg325:
  bufg port map ( I => u_clock325,
                  O => clock325);

  -- We want 27MHz true pixel clock and various multiples
  -- from out 50 or 100MHz input clock.
  -- We can use the MMCME2_ADV improved clock fiddling factors
  -- to easily get this.
  -- 50 MHz x 20.250 / 10 = 506.25MHz / 5 = 101.25 MHz
  -- 101.25MHz x 8 = 810MHz
  -- 324MHz = 810 / 2.5
  -- 162MHz = 810 / 5
  -- 81MHz = 810 / 10
  -- 40.5MHz = 810 / 20
  -- 27MHz = 810 / 30
  -- So we need only 2 MMCME2_ADV's to achieve this

  adjust0 : MMCME2_ADV
  generic map
   (BANDWIDTH => "OPTIMIZED",
    CLKOUT4_CASCADE => false,
    COMPENSATION => "ZHOLD",
    STARTUP_WAIT => false,

    CLKIN1_PERIOD => 20.000000,
    REF_JITTER1 => 0.010000,

    -- Create 506.25 MHz
    DIVCLK_DIVIDE => 1,
    -- For 100MHz input clock use 10.125 here instead of 20.250
    CLKFBOUT_MULT_F => 20.250000,
    CLKFBOUT_PHASE => 0.000000,
    CLKFBOUT_USE_FINE_PS => false,

    -- CLKOUT0 = CLK_OUT1 = 506.25MHz/5 = 101.25MHz = clock10125mhz
    CLKOUT0_DIVIDE_F => 10.000000,
    CLKOUT0_DUTY_CYCLE => 0.500000,
    CLKOUT0_PHASE => 0.000000,
    CLKOUT0_USE_FINE_PS => false
    )

  port map
    -- Output clocks
   (
    CLKFBOUT => clk_fb_adjust0,
    CLKOUT0 => u_clock10125mhz,
    -- Input clock control
    CLKFBIN => clk_fb_adjust0,
    CLKIN1 => clk_in,
    CLKIN2 => '0',
    -- Tied to always select the primary input clock
    CLKINSEL => '1',
    -- Ports for dynamic reconfiguration
    DADDR(6 downto 0) => B"0000000",
    DCLK => '0',
    DEN => '0',
    DI(15 downto 0) => B"0000000000000000",
    DWE => '0',
    -- Ports for dynamic phase shift
    PSCLK => '0',
    PSEN => '0',
    PSINCDEC => '0',
    -- Other control and status signals
    PWRDWN => '0',
    RST => '0'
    );

  mmcm_adv0 : MMCME2_ADV
  generic map
   (BANDWIDTH => "OPTIMIZED",
    CLKOUT4_CASCADE => false,
    COMPENSATION => "ZHOLD",
    STARTUP_WAIT => false,

    -- Create 810MHz clock from 101.25MHz x 8 /
    DIVCLK_DIVIDE => 1,
    CLKFBOUT_MULT_F => 8.000000,
    CLKFBOUT_PHASE => 0.000000,
    CLKFBOUT_USE_FINE_PS => false,

    CLKIN1_PERIOD => 9.877000,
    REF_JITTER1 => 0.010000,

    -- CLKOUT0 = CLK_OUT1 = clock325 ~= 810MHz/2.5 = 324MHz
    CLKOUT0_DIVIDE_F => 2.500000,
    CLKOUT0_DUTY_CYCLE => 0.500000,
    CLKOUT0_PHASE => 0.000000,
    CLKOUT0_USE_FINE_PS => false,

    -- CLKOUT1 = CLK_OUT2 = clock135 ~= 810MHz/6
    CLKOUT1_DIVIDE => 6,
    CLKOUT1_DUTY_CYCLE => 0.500000,
    CLKOUT1_PHASE => 0.000000,
    CLKOUT1_USE_FINE_PS => false,

    -- CLKOUT2 = CLK_OUT3 = clock81p = 810MHz/10
    CLKOUT2_DIVIDE => 10,
    CLKOUT2_DUTY_CYCLE => 0.500000,
    CLKOUT2_PHASE => 0.000000,
    CLKOUT2_USE_FINE_PS => false,

    -- CLKOUT3 = CLK_OUT4 = clock41 ~= 810MHz/20 = 40.50MHz
    CLKOUT3_DIVIDE => 20,
    CLKOUT3_DUTY_CYCLE => 0.500000,
    CLKOUT3_PHASE => 0.000000,
    CLKOUT3_USE_FINE_PS => false,

    -- CLKOUT4 = CLK_OUT5 = clock27 = 810MHz/30
    CLKOUT4_DIVIDE => 30,
    CLKOUT4_DUTY_CYCLE => 0.500000,
    CLKOUT4_PHASE => 180.000000,
    CLKOUT4_USE_FINE_PS => false,

    -- CLKOUT5 = CLK_OUT6 = clock163 ~= 810MHz/5 = 162MHz
    CLKOUT5_DIVIDE => 5,
    CLKOUT5_DUTY_CYCLE => 0.500000,
    CLKOUT5_PHASE => 0.000000,
    CLKOUT5_USE_FINE_PS => false,

    -- CLKOUT6 = CLK_OUT7 = clock270 = 810MHz/3
    CLKOUT6_DIVIDE => 3,
    CLKOUT6_DUTY_CYCLE => 0.500000,
    CLKOUT6_PHASE => 0.000000,
    CLKOUT6_USE_FINE_PS => false
    )
  port map
    -- Output clocks
   (
    CLKFBOUT => clk_fb,

    -- Input clock control
    CLKFBIN => clk_fb,
    CLKIN1 => clock10125mhz,
    CLKIN2 => '0',
    -- Tied to always select the primary input clock
    CLKINSEL => '1',

    CLKOUT0 => u_clock325,
    CLKOUT1 => u_clock135p,
    CLKOUT2 => u_clock81p,
    CLKOUT3 => u_clock41,
    CLKOUT4 => u_clock27,
    CLKOUT5 => u_clock163,
    CLKOUT6 => u_clock270,

    -- Ports for dynamic reconfiguration
    DADDR(6 downto 0) => B"0000000",
    DCLK => '0',
    DEN => '0',
    DI(15 downto 0) => B"0000000000000000",
    DWE => '0',
    PSCLK => '0',
    PSEN => '0',
    PSINCDEC => '0',
    -- Other control and status signals
    PWRDWN => '0',
    RST => '0'
    );

  mmcm_adv1_eth : MMCME2_ADV
  generic map
   (BANDWIDTH => "OPTIMIZED",
    CLKOUT4_CASCADE => false,
    COMPENSATION => "ZHOLD",
    STARTUP_WAIT => false,

    DIVCLK_DIVIDE => 1,
    CLKFBOUT_MULT_F => 20.000000,
    CLKFBOUT_PHASE => 0.000000,
    CLKFBOUT_USE_FINE_PS => false,

    CLKIN1_PERIOD => 20.000000,
    REF_JITTER1 => 0.010000,

    CLKOUT0_DIVIDE_F => 20.000000,
    CLKOUT0_DUTY_CYCLE => 0.500000,
    CLKOUT0_PHASE => 0.000000,
    CLKOUT0_USE_FINE_PS => false,

    CLKOUT1_DIVIDE => 10,
    CLKOUT1_DUTY_CYCLE => 0.500000,
    CLKOUT1_PHASE => 0.000000,
    CLKOUT1_USE_FINE_PS => false,

    CLKOUT2_DIVIDE => 5,
    CLKOUT2_DUTY_CYCLE => 0.500000,
    CLKOUT2_PHASE => 0.000000,
    CLKOUT2_USE_FINE_PS => false
    )
  port map
    -- Output clocks
   (
    CLKFBOUT => clk_fb_eth,

    -- Input clock control
    CLKFBIN => clk_fb_eth,
    CLKIN1 => clk_in,
    CLKIN2 => '0',
    -- Tied to always select the primary input clock
    CLKINSEL => '1',
    CLKOUT0 => u_clock50,
    CLKOUT1 => u_clock100,
    CLKOUT2 => u_clock200,

    -- Ports for dynamic reconfiguration
    DADDR(6 downto 0) => B"0000000",
    DCLK => '0',
    DEN => '0',
    DI(15 downto 0) => B"0000000000000000",
    DWE => '0',
    -- Ports for dynamic phase shift
    PSCLK => '0',
    PSEN => '0',
    PSINCDEC => '0',
    -- Other control and status signals
    PWRDWN => '0',
    RST => '0'
    );




end rtl;
