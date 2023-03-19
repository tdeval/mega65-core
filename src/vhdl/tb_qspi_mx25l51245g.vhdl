library vunit_lib;
context vunit_lib.vunit_context;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use STD.textio.all;

-- library work;
use work.debugtools.all;
use work.cputypes.all;

entity tb_qspi_mx25l51245g is
  generic (runner_cfg : string);
end entity;

architecture foo of tb_qspi_mx25l51245g is

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

  signal cycle_count : unsigned(31 downto 0) := to_unsigned(1,32);

  signal sectorbuffercs : std_logic := '0';

  signal QspiDB_comb : std_ulogic_vector(3 downto 0);
  
  signal QspiDB_out : unsigned(3 downto 0);
  signal QspiDB_in : unsigned(3 downto 0) := "1111";
  signal qspidb_oe : std_logic;
  signal QspiRSTn : std_logic := '1';
  signal QspiCSn : std_logic;
  signal QspiCSn_buf : std_logic;

  signal qspi_clock : std_logic;
  signal qspi_clock_buf : std_logic;
  signal qspi_dual : std_logic := '0';
  signal qspi_quad : std_logic := '1';

  -- signal flash_si : std_ulogic := '0';
  signal flash_si : std_ulogic;
  signal flash_so : std_ulogic;
  signal flash_wpn : std_ulogic;
  signal flash_hldn : std_ulogic;

begin

  flash0: entity work.s25fl512s
    generic map (
      LongTimming => FALSE,
      TimingModel => "..............S"
    )
    port map (
        -- Controls
        SCK => qspi_clock,
        CSNeg => qspiCSn,
        RSTNeg => QspiRSTn,
        -- Data Inputs/Outputs
        SI => QspiDB_out(0),
        SO => QspiDB_out(1),
        WPNeg => QspiDB_out(2),
        HOLDNeg => QspiDB_out(3),
        oSI => QspiDB_in(0),
        oSO => QspiDB_in(1),
        oWPNeg => QspiDB_in(2),
        oHOLDNeg => QspiDB_in(3)
        -- SI => flash_si,
        -- SO => flash_so,
        -- WPNeg => flash_wpn,
        -- HOLDNeg => flash_hldn
    );


  fdc0: entity work.sdcardio
    generic map (
      cpu_frequency => 40500000,
      target => mega65r3
    )
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
      -- QSPIdual => qspi_dual,
      -- QSPIquad => qspi_quad,
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

  main : process
  begin
    test_runner_setup(runner, runner_cfg);    
    
    while test_suite loop

      if run("clock40mhz and qspi_clock signals are generated") then
--         -- Allow all signals to begin propagating
--         for i in 1 to 10 loop
--           pixelclock <= '0'; wait for 6.172 ns; pixelclock <= '1'; wait for 6.172 ns;
--         end loop;
--         for i in 1 to 10000 loop
--           pixelclock <= '0'; wait for 6.172 ns; pixelclock <= '1'; wait for 6.172 ns;
--           if vga_hsync='0' then
--             h0 <= '1';
--           else
--             h1 <= '1';
--           end if;
--           if vga_hsync='0' then
--             v0 <= '1';
--           else
--             v1 <= '1';
--           end if;
--           if (h0 and h1 and v0 and v1) = '1' then
--             exit;
--           end if;
--         end loop;
--         if (h0 and h1 and v0 and v1) = '1' then
--           report "Saw HSYNC and VSYNC low and high";
--         else
--           assert false report "Expected to see HSYNC and VSYNC both toggle at some point";
--         end if;
--       elsif run("component video signal has SYNC pulses") then
--         -- Allow all signals to begin propagating
--         for i in 1 to 10 loop
--           pixelclock <= '0'; wait for 6.172 ns; pixelclock <= '1'; wait for 6.172 ns;
--         end loop;
--         for i in 1 to 10000 loop
--           pixelclock <= '0'; wait for 6.172 ns; pixelclock <= '1'; wait for 6.172 ns;
--           if cv_luma = x"00" then
--             h0 <= '1';
--           end if;
--           if to_integer(cv_luma) > 77 then -- 256*0.3 is threshold for SYNC
--             h1 <= '1';
--           end if;
--           if (h0 and h1) = '1' then
--             exit;
--           end if;
--         end loop;
--         if (h0 and h1) = '1' then
--           report "Saw composite SYNC both low and high";
--         else
--           assert false report "Expected to see composite SYNC both low and high";
--         end if;
--       elsif run("Simulation of two fields for one frame completes") then
--         -- 81MHz = 81M cycles for 1 second.  At 50Hz, we need 81M / 50 = 1.62M
--         -- cycles. We allow a bit of margin

--         test_pattern_enable <= '1';
--         pal50_select <= '0';
--         interlace_mode <= '1';
--         mono_mode <= '0';
        
-- --        for i in 1 to 1_640_000 loop
--         for i in 1 to 700_000 loop
-- --        for i in 1 to 3_000_000 loop
--           pixelclock <= '0'; wait for 6.172 ns; pixelclock <= '1'; wait for 6.172 ns;
--         end loop;

        while cycle_count(19) /= '1' loop
          -- report "test_suite : " & test_suite;

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

          if qspi_quad='1' then
            if  qspidb_oe='0' then
              qspidb_comb <= "ZZZZ";
              -- flash_si   <= 'Z';
              -- flash_so   <= 'Z';
              -- flash_wpn  <= 'Z';
              -- flash_hldn <= 'Z';
              report "TRI: Tristating. Reading " & to_string(std_logic_vector(qspidb_comb));
            end if;
            if  qspidb_oe='1' then
              qspidb_comb <= std_ulogic_vector(qspidb_out);
              -- flash_si    <= std_ulogic(qspidb_out(0));
              -- flash_so    <= std_ulogic(qspidb_out(1));
              -- flash_wpn   <= std_ulogic(qspidb_out(2));
              -- flash_hldn  <= std_ulogic(qspidb_out(3));
              report "TRI: Exporting " & to_string(std_ulogic_vector(qspidb_out));
            end if;
            -- qspidb_in <= unsigned(qspidb_comb);
          elsif qspi_dual='1' then
            if  qspidb_oe='0' then
              qspidb_comb(1 downto 0) <= "ZZ";
              -- flash_s1 <= 'Z';
              -- flash_so <= 'Z';
              report "SPI: Tristating. Reading " & to_string(std_logic_vector(qspidb_comb));
            end if;
            if  qspidb_oe='1' then
              qspidb_comb(1 downto 0) <= std_ulogic_vector(qspidb_out(1 downto 0));
              -- flash_si <= std_ulogic(qspidb_out(0));
              -- flash_si <= qspidb_out(0);
              -- flash_so <= std_ulogic(qspidb_out(1));
              -- flash_so <= qspidb_out(1);
              report "SPI: Exporting " & to_string(std_logic_vector(qspidb_out(1 downto 0)));
            end if;
            qspidb_comb(3 downto 2) <= (others => '1');
            -- flash_wpn  <= '1';
            -- flash_hldn <= '1';
            -- qspidb_in(0) <= flash_si;
            -- qspidb_in(1) <= flash_so;
          else
            if  qspidb_oe='0' then
              qspidb_comb(1) <= 'Z';
              -- flash_so <= 'Z';
              report "SPI: Tristating. Reading " & to_string(std_logic_vector(qspidb_comb));
            end if;
            if  qspidb_oe='1' then
              qspidb_comb(0) <= std_ulogic(qspidb_out(0));
              -- flash_si <= std_ulogic(qspidb_out(0));
              -- flash_si <= qspidb_out(0);
              report "SPI: Exporting " & std_logic'image(qspidb_out(0));
            end if;
            qspidb_comb(3 downto 2) <= (others => '1');
            -- flash_wpn  <= '1';
            -- flash_hldn <= '1';
            -- qspidb_in(1) <= flash_so;
          end if;
        end loop;
        -- test_suite <= FALSE;

      end if;

      -- clock40mhz <= '1';
      -- clock80mhz <= '1';
      -- wait for 6.25 ns;
      -- clock80mhz <= '0';
      -- wait for 6.25 ns;
      -- clock40mhz <= '0';
      -- clock80mhz <= '1';
      -- wait for 6.25 ns;
      -- clock80mhz <= '0';
      -- wait for 6.25 ns;

      -- if qspi_quad='1' then
      --   if  qspidb_oe='0' then
      --     qspidb_comb <= "ZZZZ";
      --     flash_si   <= 'Z';
      --     flash_so   <= 'Z';
      --     flash_wpn  <= 'Z';
      --     flash_hldn <= 'Z';
      --     report "TRI: Tristating. Reading " & to_string(std_logic_vector(qspidb_comb));
      --   end if;
      --   if  qspidb_oe='1' then
      --     qspidb_comb <= std_ulogic_vector(qspidb_out);
      --     flash_si    <= std_ulogic(qspidb_out(0));
      --     flash_so    <= std_ulogic(qspidb_out(1));
      --     flash_wpn   <= std_ulogic(qspidb_out(2));
      --     flash_hldn  <= std_ulogic(qspidb_out(3));
      --     report "TRI: Exporting " & to_string(std_ulogic_vector(qspidb_out));
      --   end if;
      --   qspidb_in <= unsigned(qspidb_comb);
      -- end if;
      -- if qspi_quad='0' then
      --   if  qspidb_oe='0' then
      --     qspidb_comb(1) <= 'Z';
      --     flash_so <= 'Z';
      --     report "SPI: Tristating. Reading " & to_string(std_logic_vector(qspidb_comb));
      --   end if;
      --   if  qspidb_oe='1' then
      --     qspidb_comb(0) <= std_ulogic(qspidb_out(0));
      --     flash_so <= std_ulogic(qspidb_out(0));
      --     report "SPI: Exporting " & std_logic'image(qspidb_out(0));
      --   end if;
      --   qspidb_comb(3 downto 2) <= (others => '1');
      --   flash_wpn  <= '1';
      --   flash_hldn <= '1';
      --   qspidb_in(1) <= flash_si;
      -- end if;


    end loop;
    -- QspiDB_comb <= std_ulogic_vector(qspidb_out) when qspidb_oe='1' else "ZZZZ";
    -- qspidb_in <= unsigned(QspiDB_comb);

    test_runner_cleanup(runner);
  end process;

--   process is
--   begin
--     while true loop
--       clock40mhz <= '1';
--       clock80mhz <= '1';
--       wait for 6.25 ns;
--       clock80mhz <= '0';
--       wait for 6.25 ns;
--       clock40mhz <= '0';
--       clock80mhz <= '1';
--       wait for 6.25 ns;
--       clock80mhz <= '0';
--       wait for 6.25 ns;
--     end loop;
--   end process;

  process (clock40mhz,byte_out) is

    procedure PEEK(addr : in unsigned(19 downto 0); val : out unsigned(7 downto 0)) is
    begin
      fastio_addr <= addr;
      val := fastio_rdata;
      fastio_read <= '1';
      fastio_write <= '0';
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
    variable subcycle_count : unsigned(8 downto 0) := to_unsigned(0,9);

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
        -- if  qspidb_oe='0' then
        --   qspidb_comb(1) <= 'Z';
        --   report "SPI: Tristating. Reading " & to_string(std_logic_vector(qspidb_comb));
        -- end if;
        -- if  qspidb_oe='1' then
        --   qspidb_comb(0) <= std_ulogic'(qspidb_out(0));
        --   report "SPI: Exporting " & std_logic'image(qspidb_out(0));
        -- end if;
        -- qspidb_comb(3 downto 2) <= (others => '1');
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

    -- qspi_clock_buf <= qspi_clock;
    -- qspiCSn_buf <= qspicsn;

    if rising_edge(clock40mhz) then
      -- subcycle_count := subcycle_count + 1;

      -- if subcycle_count = "1111" then
        cycle_count <= cycle_count + 1;
      -- end if;

      f_rdata <= f_wdata;

      case to_integer(cycle_count) is

        -- Disable free-running clock
        when 12300 => QspiRSTn <= '0';
        when 12301 => POKE(x"d36cd",x"00");

        -- Read CFI block
        when 15000 => POKE(x"d3680",x"68"); -- Release CS on completion
        when 15001 => POKE(x"d0000",x"00");
        when 15240 => QspiRSTn <= '1';
        when 15241 => POKE(x"d3680",x"6b");
        when 15242 => POKE(x"d3020",x"00");

        -- -- Read SFDP block
        -- when 601 => POKE(x"d3680",x"6d");
        -- when 602 => POKE(x"d3020",x"00");

        -- Enable Quad mode
        when 30001 => POKE(x"d3680",x"66"); -- Write enable
        when 30002 => POKE(x"d0000",x"00"); -- stop writing to reg
        when 30201 => POKE(x"d3683",x"02"); -- Write to SR1/CR1
        when 30202 => POKE(x"d3684",x"00");
        when 30203 => POKE(x"d3680",x"69"); -- Write CSR1
        when 30204 => POKE(x"d0000",x"00"); -- stop writing to reg
        when 30205 => POKE(x"d3680",x"68"); -- Release CS on completion
        when 30206 => POKE(x"d0000",x"00"); -- stop writing to reg

        when 38701 => POKE(x"d3680",x"66"); -- Write enable
        when 38702 => POKE(x"d0000",x"00"); -- stop writing to reg

        -- Write to address $00000000
        when 38800 => POKE(x"d3681",x"00");
        when 38801 => POKE(x"d3682",x"00");
        when 38802 => POKE(x"d3683",x"00");
        when 38803 => POKE(x"d3684",x"01");
        -- Erase page
        when 38804 => POKE(x"d3680",x"58");
        when 38805 => POKE(x"d0000",x"00");

        -- Put some data in the write buffer
        when 38901 => POKE(x"d6e00",x"01");
        when 38902 => POKE(x"d6e01",x"23");
        when 38903 => POKE(x"d6e02",x"45");
        when 38904 => POKE(x"d6e03",x"67");
        when 38905 => POKE(x"d6e04",x"89");
        when 38906 => POKE(x"d6e05",x"ab");
                      subcycle_count := to_unsigned(16#1f0#,9) - to_unsigned(16#006#,9);
        when 38907 => IF subcycle_count /= 0 THEN
                        POKE(to_unsigned(16#d6ff0#,20) - subcycle_count,x"ff");
                        subcycle_count := subcycle_count - 1;
                        cycle_count <= cycle_count;
                      END IF;

        when 38908 => POKE(x"d6ff0",x"f0");
        when 38909 => POKE(x"d6ff1",x"f1");
        when 38910 => POKE(x"d6ff2",x"f2");
        when 38911 => POKE(x"d6ff3",x"f3");
        when 38912 => POKE(x"d6ff4",x"f4");
        when 38913 => POKE(x"d6ff5",x"f5");
        when 38914 => POKE(x"d6ff6",x"f6");
        when 38915 => POKE(x"d6ff7",x"f7");
        when 38916 => POKE(x"d6ff8",x"48");
        when 38917 => POKE(x"d6ff9",x"49");
        when 38918 => POKE(x"d6ffa",x"4a");
        when 38919 => POKE(x"d6ffb",x"4b");
        when 38920 => POKE(x"d6ffc",x"4c");
        when 38921 => POKE(x"d6ffd",x"4d");
        when 38922 => POKE(x"d6ffe",x"4e");
        when 38923 => POKE(x"d6fff",x"4f");
        when 38924 => POKE(x"d3020",x"00");



        when 39000 => POKE(x"d3680",x"5f"); -- select reduced dummy cycles
        when 39001 => POKE(x"d3020",x"00");

        -- Enable writing
        when 116401 => POKE(x"d3680",x"66"); -- Write enable
        when 116402 => POKE(x"d0000",x"00"); -- stop writing to reg

        -- Do page write
        when 116600 => POKE(x"d3680",x"54"); -- $53 = sector read, $54 = program 512
                                             -- bytes, $58 = erase page, $59 = erase
                                             -- 4KB page, $66 = write enable, $55 =
                                             -- program 256 bytes, $6c = program 16 bytes
        when 116601 => POKE(x"d0000",x"00"); -- stop writing to reg

        -- Read from address $00000000
        when 151401 => POKE(x"d3681",x"00");
        when 151402 => POKE(x"d3682",x"00");
        when 151403 => POKE(x"d3683",x"00");
        when 151404 => POKE(x"d3684",x"01");
        when 151405 => POKE(x"d3680",x"68"); -- Release CS on completion
        when 151406 => POKE(x"d0000",x"00"); -- stop writing to reg

        when 151500 => POKE(x"d3680",x"5d"); -- select reduced dummy cycles
        -- when 151500 => POKE(x"d3680",x"5f"); -- select reduced dummy cycles
        when 151501 => POKE(x"d3020",x"00");

                     -- Verify written page
        when 152401 => POKE(x"d3680",x"56"); -- Read page back
        when 152402 => POKE(x"d0000",x"00"); -- stop writing to reg
                     -- Check bytes differ (= verify fail in this case) flag in
                     -- bit 6 of $D689

        when 156001 => POKE(x"d36cc",x"ff");

        when 173901 => PEEK(x"d3689",peek_data);
        when 173902 => POKE(x"d0000",x"00"); -- stop writing to reg


        when others => null;
      end case;

      -- if cycle_count > 65535 then
      --   cycle_count <= to_unsigned(0,32);
      -- end if;

    end if;
  end process;

end foo;
