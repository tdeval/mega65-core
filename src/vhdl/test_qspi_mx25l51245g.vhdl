library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use STD.textio.all;

-- library work;
use work.debugtools.all;
use work.cputypes.all;

entity test_qspi_mx25l51245g is
end entity;

architecture foo of test_qspi_mx25l51245g is

  type CharFile is file of character;

  signal clock40mhz : std_logic := '1';
  signal clock80mhz : std_logic := '1';
  -- Rate for 720K DD disks
  signal cycles_per_interval : unsigned(7 downto 0) := to_unsigned(81,8);

    -- The track/sector/side we are being asked to find
  signal target_track : unsigned(7 downto 0) := x"00";
  signal target_sector : unsigned(7 downto 0) := x"01";
  signal target_side : unsigned(7 downto 0) := x"01";
  signal target_any : std_logic := '0';

  -- Indicate when we have hit the start of the gap leading
  -- to the data area (this is so that sector writing can
  -- begin.  It does have to take account of the latency of
  -- the write stage, and also any write precompensation).
  signal sector_found : std_logic := '0';
  signal sector_data_gap : std_logic := '0';
  signal found_track : unsigned(7 downto 0) := x"00";
  signal found_sector : unsigned(7 downto 0) := x"00";
  signal found_side : unsigned(7 downto 0) := x"00";

    -- Bytes of the sector when reading
  signal first_byte : std_logic := '0';
  signal byte_valid : std_logic := '0';
  signal byte_out : unsigned(7 downto 0);
  signal crc_error : std_logic := '0';
  signal sector_end : std_logic := '0';

  signal last_sector_end : std_logic := '0';
  signal last_sector_found : std_logic := '0';
  signal last_crc_error : std_logic := '0';

  signal byte_count : integer := 0;

  signal ready_for_next : std_logic := '0';
  signal byte_valid_in : std_logic := '0';
  signal byte_in : unsigned(7 downto 0) := x"00";
  signal clock_byte_in : unsigned(7 downto 0) := x"FF";

  signal sdcardio_cs : std_logic := '0';
  signal f011_cs : std_logic := '0';
  signal fastio_addr : unsigned(19 downto 0) := to_unsigned(0,20);
  signal fastio_addr_fast : unsigned(19 downto 0) := to_unsigned(0,20);
  signal fastio_wdata : unsigned(7 downto 0) := to_unsigned(0,8);
  signal fastio_rdata : unsigned(7 downto 0) := to_unsigned(0,8);
  signal fastio_write : std_logic := '0';
  signal fastio_read : std_logic := '0';

  signal f_rdata : std_logic := '1';
  signal f_wdata : std_logic := '1';
  signal f_track0 : std_logic := '1';
  signal f_writeprotect : std_logic := '1';
  signal f_diskchanged : std_logic := '1';
  signal f_index : std_logic := '1';

  signal cycle_count : integer := 0;

  signal sectorbuffercs : std_logic := '0';

  signal QspiDB_comb : std_ulogic_vector(3 downto 0) := "1010";
  
  signal QspiDB_out : unsigned(3 downto 0);
  signal QspiDB_in : unsigned(3 downto 0) := "1111";
  signal qspidb_oe : std_logic;
  signal QspiCSn : std_logic;
  signal QspiCSn_buf : std_logic;
  signal qspi_clock : std_logic;
  signal qspi_clock_buf : std_logic;
  signal qspi_quad : std_logic;

  signal flash_si : std_ulogic;
  signal flash_so : std_ulogic;
  signal flash_wpn : std_ulogic;
  signal flash_hldn : std_ulogic;

begin

  flash0: entity work.s25fl512s
    generic map (
      TimingModel => "..............S"
    )
    port map (
        -- Controls
        SCK => qspi_clock,
        CSNeg => qspiCSn,
        RSTNeg => '1',
        -- Data Inputs/Outputs
        SI => QspiDB_comb(0),
        SO => QspiDB_comb(1)
        -- WPNeg => QspiDB_comb(2),
        -- HOLDNeg => QspiDB_comb(3)
    );


  fdc0: entity work.sdcardio
    generic map (
      cpu_frequency => 40500000,
      target => mega65r3 )
    port map (
      clock => clock40mhz,
      pixelclk => clock80mhz,
      reset => '1',
      sdcardio_cs => sdcardio_cs,
      f011_cs => f011_cs,

      qspidb => qspidb_out,
      qspidb_oe => qspidb_oe,
      qspidb_in => qspidb_in,
      qspicsn => qspicsn,
      qspi_quad => qspi_quad,
      qspi_clock => qspi_clock,

      audio_mix_rdata => x"ffff",
      audio_loopback => x"ffff",

      hypervisor_mode => '1',
      secure_mode => '0',
      fpga_temperature => (others => '0'),
      pwm_knob => x"ffff",

      fastio_addr_fast => fastio_addr_fast,
      fastio_addr => fastio_addr,
      fastio_write => fastio_write,
      fastio_read => fastio_read,
      fastio_wdata => fastio_wdata,
      fastio_rdata_sel => fastio_rdata,

      virtualise_f011_drive0 => '0',
      virtualise_f011_drive1 => '0',
      colourram_at_dc00 => '0',
      viciii_iomode => "11",
      sectorbuffercs => sectorbuffercs,
      sectorbuffercs_fast => sectorbuffercs,
      last_scan_Code => (others => '1'),

      dipsw => (others => '1'),
      j21in => (others => '1'),
      sw => (others => '1'),
      btn => (others => '1'),
      miso_i => '1',
      f_index => f_index,
      f_track0 => f_track0,
      f_writeprotect => f_writeprotect,
      f_rdata => f_rdata,
      f_wdata => f_wdata,
      f_diskchanged => f_diskchanged,

      sd1541_request_toggle => '0',
      sd1541_enable => '0',
      sd1541_track => to_unsigned(0,6),

      aclMISO => '0',
      aclInt1 => '0',
      aclInt2 => '0',
      tmpInt => '0',
      tmpCT => '0'
    );


  process is
  begin
    while true loop
      clock40mhz <= '1';
      clock80mhz <= '1';
      wait for 6.25 ns;
      clock80mhz <= '0';
      wait for 6.25 ns;
      clock40mhz <= '0';
      clock80mhz <= '1';
      wait for 6.25 ns;
      clock80mhz <= '0';
      wait for 6.25 ns;
    end loop;
  end process;

  process (qspi_clock,qspi_quad,qspidb_oe,qspidb_out) is
  begin
    report "qspi_clock = " & std_logic'image(qspi_clock);

    -- if rising_edge(qspi_clock) then
      -- if qspi_quad='1' then
      --   if  qspidb_oe='0' then
      --     qspidb_comb <= "ZZZZ";
      --     report "TRI: Tristating. Reading " & to_string(std_logic_vector(qspidb_comb));
      --   end if;
      --   if  qspidb_oe='1' then
      --     qspidb_comb <= qspidb_out;
      --     report "TRI: Exporting " & to_string(std_logic_vector(qspidb_out));
      --   end if;
      -- end if;
      -- if qspi_quad='0' then
        if  qspidb_oe='0' then
          qspidb_comb(1) <= 'Z';
          report "SPI: Tristating. Reading " & to_string(std_logic_vector(qspidb_comb));
        end if;
        if  qspidb_oe='1' then
          qspidb_comb(0) <= std_ulogic'(qspidb_out(0));
          report "SPI: Exporting " & std_logic'image(qspidb_out(0));
        end if;
        qspidb_comb(3 downto 2) <= (others => '1');
      -- end if;
    -- end if;
    qspidb_in <= unsigned(qspidb_comb);
  end process;

  process (clock40mhz,byte_out) is

    procedure PEEK(addr : in unsigned(19 downto 0); val : out unsigned(7 downto 0)) is
    begin
      fastio_addr <= addr;
      val := fastio_rdata;
      fastio_read <= '1';
      fastio_write <= '0';
      if addr(19 downto 8) = x"0d6" then
        sdcardio_cs <= '1';
      else
        sdcardio_cs <= '0';
      end if;
      if addr(19 downto 8) = x"d08" then
        f011_cs <= '1';
      else
        f011_cs <= '0';
      end if;
      if addr(19 downto 12) = x"d6" then
        sectorbuffercs <= '1';
      else
        sectorbuffercs <= '0';
      end if;
      -- report "PEEK $" & to_hstring(std_logic_vector(addr)) & ",$" & to_hstring(fastio_rdata);
    end PEEK;

    procedure POKE(addr : in unsigned(19 downto 0); val : in unsigned(7 downto 0)) is
    begin
      fastio_addr <= addr;
      fastio_wdata <= val;
      fastio_read <= '0';
      fastio_write <= '1';
      if addr(19 downto 8) = x"d36" then
        sdcardio_cs <= '1';
      else
        sdcardio_cs <= '0';
      end if;
      if addr(19 downto 8) = x"d08" then
        f011_cs <= '1';
      else
        f011_cs <= '0';
      end if;
      if addr(19 downto 12) = x"d6" then
        sectorbuffercs <= '1';
      else
        sectorbuffercs <= '0';
      end if;
      -- report "POKE $" & to_hstring(addr) & ",$" & to_hstring(val);
    end POKE;


    variable peek_data : unsigned(7 downto 0) := to_unsigned(0,8);
    variable subcycle_count : unsigned(3 downto 0) := to_unsigned(0,4);

  begin

    -- qspidb_comb <= qspidb_out when qspidb_oe='1' else "ZZZZ";
    -- qspidb_in <= unsigned(qspidb_comb);
    -- flash_si <= '0';
    -- flash_so <= 'Z';
    -- qspidb_in(0) <= flash_si;
    -- qspidb_in(1) <= flash_so;
    -- qspidb_in(2) <= flash_wpn;
    -- qspidb_in(3) <= flash_hldn;

    -- if rising_edge(clock40mhz) then
    -- if qspi_quad='1' then
    --   if  qspidb_oe='0' then
    --     qspidb_comb <= "ZZZZ";
    --     report "TRI: Tristating. Reading " & to_string(std_logic_vector(qspidb_comb));
    --   end if;
    --   if  qspidb_oe='1' then
    --     qspidb_comb <= qspidb_out;
    --     report "TRI: Exporting " & to_string(std_logic_vector(qspidb_out));
    --   end if;
    -- end if;
    -- if qspi_quad='0' then
    --   if  qspidb_oe='0' then
    --     qspidb_comb(1) <= 'Z';
    --     report "SPI: Tristating. Reading " & to_string(std_logic_vector(qspidb_comb));
    --   end if;
    --   if  qspidb_oe='1' then
    --     qspidb_comb(0) <= qspidb_out(0);
    --     report "SPI: Exporting " & std_ulogic'image(qspidb_out(0));
    --   end if;
    --   qspidb_comb(3 downto 2) <= (others => '1');
    -- end if;
    -- qspidb_in <= unsigned(qspidb_comb);
    -- flash_si <= qspidb_comb(0);
    -- flash_so <= qspidb_comb(1);
    -- flash_wpn <= qspidb_comb(2);
    -- flash_hldn <= qspidb_comb(3);
    -- qspidb_in(0) <= flash_si;
    -- qspidb_in(1) <= flash_so;
    -- qspidb_in(2) <= flash_wpn;
    -- qspidb_in(3) <= flash_hldn;

    qspi_clock_buf <= qspi_clock;
    qspiCSn_buf <= qspicsn;

    if rising_edge(clock40mhz) then
      -- subcycle_count := subcycle_count + 1;

      -- if subcycle_count = "1111" then
        cycle_count <= cycle_count + 1;
      -- end if;

      f_rdata <= f_wdata;

      case cycle_count is

        -- Disable free-running clock
        when 301 => POKE(x"d36cd",x"00");

        -- Read CFI block
        when 401 => POKE(x"d3680",x"6b");
        when 402 => POKE(x"d3020",x"00");

        -- -- Read SFDP block
        -- when 601 => POKE(x"d3680",x"6d");
        -- when 602 => POKE(x"d3020",x"00");

        -- Enable Quad mode
        when  1801 => POKE(x"d3680",x"66"); -- Write enable
        when  1802 => POKE(x"d0000",x"00"); -- stop writing to reg
        when  2801 => POKE(x"d3683",x"02"); -- Write to SR1/CR1
        when  2802 => POKE(x"d3684",x"00");
        when  2803 => POKE(x"d3680",x"69");
        when  2804 => POKE(x"d0000",x"00");

        -- Put some data in the write buffer
        when 3001 => POKE(x"d6e00",x"12");
        when 3002 => POKE(x"d6e01",x"34");
        when 3003 => POKE(x"d6e02",x"56");
        when 3004 => POKE(x"d6e03",x"78");
        when 3005 => POKE(x"d6e04",x"9a");

        when 3006 => POKE(x"d6ff0",x"f0");
        when 3007 => POKE(x"d6ff1",x"f1");
        when 3008 => POKE(x"d6ff2",x"f2");
        when 3009 => POKE(x"d6ff3",x"f3");
        when 3010 => POKE(x"d6ff4",x"f4");
        when 3011 => POKE(x"d6ff5",x"f5");
        when 3012 => POKE(x"d6ff6",x"f6");
        when 3013 => POKE(x"d6ff7",x"f7");
        when 3014 => POKE(x"d6ff8",x"48");
        when 3015 => POKE(x"d6ff9",x"49");
        when 3016 => POKE(x"d6ffa",x"4a");
        when 3017 => POKE(x"d6ffb",x"4b");
        when 3018 => POKE(x"d6ffc",x"4c");
        when 3019 => POKE(x"d6ffd",x"4d");
        when 3020 => POKE(x"d6ffe",x"4e");
        when 3021 => POKE(x"d6fff",x"4f");
        when 3022 => POKE(x"d3020",x"00");


        when 4100 => POKE(x"d3680",x"5f"); -- select reduced dummy cycles
        when 4101 => POKE(x"d3020",x"00");

        -- Write to address $00000000
        when 4206 => POKE(x"d3681",x"00");
        when 4207 => POKE(x"d3682",x"00");
        when 4208 => POKE(x"d3683",x"00");
        when 4209 => POKE(x"d3684",x"00");

                     -- Enable writing
        when 4211 => POKE(x"d3680",x"66"); -- Write enable
        when 4212 => POKE(x"d0000",x"00"); -- stop writing to reg

                     -- Do page write
        when 5400 => POKE(x"d3680",x"54"); -- $53 = sector read, $54 = program 512
                                        -- bytes, $58 = erase page, $59 = erase
                                        -- 4KB page, $66 = write enable, $55 =
                                        -- program 256 bytes, $6c = program 16 bytes
        when 5401 => POKE(x"d0000",x"00"); -- stop writing to reg

                     -- Verify written page
        when 16001 => POKE(x"d3680",x"56"); -- Read page back
        when 16002 => POKE(x"d0000",x"00"); -- stop writing to reg
                     -- Check bytes differ (= verify fail in this case) flag in
                     -- bit 6 of $D689
        when 30001 => PEEK(x"d3689",peek_data);
        when 30002 => POKE(x"d0000",x"00"); -- stop writing to reg


        when others => null;
      end case;

    end if;
  end process;

end foo;
