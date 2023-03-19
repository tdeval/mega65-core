-------------------------------------------------------------------------------
--  File Name: s25fl512s.vhd
-------------------------------------------------------------------------------
--  Copyright (C) 2009-2020 Free Model Foundry; http://www.FreeModelFoundry.com
--
--  This program is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License version 2 as
--  published by the Free Software Foundation.
--
--  MODIFICATION HISTORY:
--
--  version: |  author:     | mod date:   |  changes made:
--    V1.0     V.Mancev       09 Nov 26     Inital Release
--    V1.1     V.Mancev       10 Mar 04     addr_cnt for second read in
--                                          high performance read continuous
--                                          mode can change its value only
--                                          when CSNeg = '0'
--    V1.2     V.Mancev       10 July 28    During the QUAD mode HOLD# input
--                                          is not monitored for its normal
--                                          function
--    V1.3     V.Mancev       10 Oct 28     Latest datasheet aligned
--    V1.4     V.Mancev       10 Nov 15     QUAD Program operation during Erase
--                                          Suspend is added
--                                          Warning for Resume to Suspend time
--                                          is added
--                                          During Erase Suspend, after Program
--                                          operation is completed, WEL bit is
--                                          cleared
--                                          Implemetation of Software Reset is
--                                          Changed
--    V1.2     V.Mancev       11 July 11    Latest datasheet aligned
--    V1.3     V.Mancev       22 July 11    Corrections for P4E and P4E4
--    V1.4     V. Mancev      18 Nov 11     Time tHO is changed to 1 ns
--                                          (customer's request)
--                                          BRWR instruction is corrected
--    V1.5     S.Petrovic     12 Jun 02     QPP Instruction is allowed on
--                                          previously programmed page
--    V1.6     S.Petrovic     12 Jun 12     Removed unnecessary message
--                                          regarding QPP instruction
--    V1.7     S.Petrovic     13 Jan 28     Reverted restriction for QPP
--                                          on programmed page and
--                                          added clearing with sector erase
--    V1.8     S.Petrovic     13 Nov 28     Corrected FSM state transitions
--                                          initiated by Power-Up and HW Reset
--                                          in StateGen process
--    V1.9     S.Petrovic     13 Dec 20     Corrected DLP read
--    V1.10    S.Petrovic     14 Dec 24     Added Read SFDP instruction
--    V1.11    M.Stojanovic   15 May 15     Ignored upper address bits for RD4
--    V1.12    M.Stojanovic   15 May 29     Ignored upper address bits for all
--                                          commands in QUAD mode
--    V1.13    S.Stevanovic   16 May 10     During QPP and QPP4 commands
--                                          the same page must not be
--                                          programmed more than once. However
--                                          do not generate P_ERR if this
--                                          occurs.
--    V1.14      M.Krneta     19 May 08     Updated according to the rev *Q
--                                          (QPP and QPP4 commands changed,
--                                          ECCRD command added,
--                                          LOCK bit removed)
--    V1.15     B.Barac        20 May 18    Speeding up speedsim
--                                          buy factors 10-100 000
-------------------------------------------------------------------------------
--  PART DESCRIPTION:
--
--  Library:    FLASH
--  Technology: FLASH MEMORY
--  Part:       S25FL512S
--
--   Description: 512 Megabit Serial Flash Memory
--
-------------------------------------------------------------------------------
--  Comments :
--      For correct simulation, simulator resolution should be set to 1 ps
--      A device ordering (trim) option determines whether a feature is enabled
--      or not, or provide relevant parameters:
--        -15th character in TimingModel determines if enhanced high
--         performance option is available
--            (0,2,3,R,A,B,C,D) EHPLC
--            (Y,Z,S,T,K,L)     Security EHPLC
--            (4,6,7,8,9,Q)     HPLC
--        -15th character in TimingModel determines if RESET# input
--        is available
--            (R,A,B,C,D,Q.6,7,K,L,S,T,M,N,U,V)  RESET# is available
--            (0,2,3,4,8,9,Y.Z.W,X)              RESET# is tied to the inactive
--                                               state,inside the package.
--
-------------------------------------------------------------------------------
--  Known Bugs:
--
-------------------------------------------------------------------------------
LIBRARY IEEE;   USE IEEE.std_logic_1164.ALL;
                USE STD.textio.ALL;
                USE IEEE.VITAL_timing.ALL;
                USE IEEE.VITAL_primitives.ALL;

LIBRARY WORK;   USE work.gen_utils.ALL;
                USE work.conversions.ALL;
                USE work.debugtools.ALL;
-------------------------------------------------------------------------------
-- ENTITY DECLARATION
-------------------------------------------------------------------------------
ENTITY s25fl512s IS
    GENERIC (
    ---------------------------------------------------------------------------
    -- TIMING GENERICS:
    ---------------------------------------------------------------------------
        -- tipd delays: interconnect path delays (delay between components)
        --    There should be one for each IN or INOUT pin in the port list
        --    They are given default values of zero delay.
        tipd_SCK                : VitalDelayType01  := VitalZeroDelay01;
        tipd_SI                 : VitalDelayType01  := VitalZeroDelay01;
        tipd_SO                 : VitalDelayType01  := VitalZeroDelay01;

        tipd_CSNeg              : VitalDelayType01  := VitalZeroDelay01;
        tipd_HOLDNeg            : VitalDelayType01  := VitalZeroDelay01;
        tipd_WPNeg              : VitalDelayType01  := VitalZeroDelay01;
        tipd_RSTNeg             : VitalDelayType01  := VitalZeroDelay01;

        -- tpd delays: propagation delays (pin-to-pin delay within a component)
        tpd_SCK_oSO_normal       : VitalDelayType01Z := UnitDelay01Z; -- tV, tHO
        tpd_CSNeg_oSO            : VitalDelayType01Z := UnitDelay01Z; -- tDIS
        tpd_HOLDNeg_oSO          : VitalDelayType01Z := UnitDelay01Z;--
        tpd_RSTNeg_oSO           : VitalDelayType01Z := UnitDelay01Z;--
            -- DDR operation values
        tpd_SCK_oSO_DDR          : VitalDelayType01Z := UnitDelay01Z;--tV(66MHz)

        -- tsetup values: setup times
        --   setup time is minimum time before the referent signal edge the
        --   input should be stable
        tsetup_CSNeg_SCK_normal_noedge_posedge
                                : VitalDelayType    := UnitDelay; -- tCSS /
        tsetup_CSNeg_SCK_DDR_noedge_posedge
                                : VitalDelayType    := UnitDelay; -- tCSS /
        tsetup_SI_SCK_normal_noedge_posedge: VitalDelayType
                                                    := UnitDelay; -- tSU:DAT /
        tsetup_WPNeg_CSNeg      : VitalDelayType    := UnitDelay; -- tWPS \
        tsetup_HOLDNeg_SCK      : VitalDelayType    := UnitDelay;
        tsetup_RSTNeg_CSNeg     : VitalDelayType    := UnitDelay; -- tRP
            -- DDR operation values
        tsetup_SI_SCK_DDR_noedge_posedge:   VitalDelayType
                                                    := UnitDelay;   -- tSU /
        tsetup_SI_SCK_DDR_noedge_negedge:   VitalDelayType
                                                    := UnitDelay;   -- tSU \
        tsetup_SI_SCK_DDR80_noedge_posedge: VitalDelayType
                                                    := UnitDelay;   -- tSU /
        tsetup_SI_SCK_DDR80_noedge_negedge: VitalDelayType
                                                    := UnitDelay;   -- tSU \

        -- thold values: hold times
        --   hold time is minimum time the input should be present stable
        --   after the referent signal edge
        thold_CSNeg_SCK_normal_noedge_posedge
                                : VitalDelayType    := UnitDelay; -- tCSH /
        thold_CSNeg_SCK_DDR_noedge_posedge
                                : VitalDelayType    := UnitDelay; -- tCSH /
        thold_SI_SCK_normal_noedge_posedge: VitalDelayType
                                                    := UnitDelay; -- tHD:DAT /
        thold_WPNeg_CSNeg       : VitalDelayType    := UnitDelay; -- tWPH /
        thold_HOLDNeg_SCK       : VitalDelayType    := UnitDelay; --
        thold_CSNeg_RSTNeg      : VitalDelayType    := UnitDelay; -- tRPH
            -- DDR operation values
        thold_SI_SCK_DDR_noedge_posedge:   VitalDelayType
                                                    := UnitDelay;  -- tHD /
        thold_SI_SCK_DDR_noedge_negedge:   VitalDelayType
                                                    := UnitDelay;  -- tHD \
        thold_SI_SCK_DDR80_noedge_posedge: VitalDelayType
                                                    := UnitDelay;  -- tHD /
        thold_SI_SCK_DDR80_noedge_negedge: VitalDelayType
                                                    := UnitDelay;  -- tHD \

        --tpw values: pulse width
        tpw_SCK_serial_posedge  : VitalDelayType := UnitDelay; -- tWH
        tpw_SCK_dual_posedge    : VitalDelayType := UnitDelay; -- tWH
        tpw_SCK_fast_posedge    : VitalDelayType := UnitDelay; -- tWH
        tpw_SCK_quadpg_posedge  : VitalDelayType := UnitDelay; -- tWH
        tpw_SCK_serial_negedge  : VitalDelayType := UnitDelay; -- tWL
        tpw_SCK_dual_negedge    : VitalDelayType := UnitDelay; -- tWL
        tpw_SCK_fast_negedge    : VitalDelayType := UnitDelay; -- tWL
        tpw_SCK_quadpg_negedge  : VitalDelayType := UnitDelay; -- tWL
        tpw_CSNeg_read_posedge  : VitalDelayType := UnitDelay; -- tCS
        tpw_CSNeg_pgers_posedge : VitalDelayType := UnitDelay; -- tCS
        tpw_RSTNeg_negedge      : VitalDelayType := UnitDelay; -- tRP
        tpw_RSTNeg_posedge      : VitalDelayType := UnitDelay; -- tRS
            -- DDR operation values
        tpw_SCK_DDR_posedge     : VitalDelayType := UnitDelay; -- tWH(66MHz)
        tpw_SCK_DDR_negedge     : VitalDelayType := UnitDelay; -- tWL(66Hz)
        tpw_SCK_DDR80_posedge   : VitalDelayType := UnitDelay; -- tWH(80MHz)
        tpw_SCK_DDR80_negedge   : VitalDelayType := UnitDelay; -- tWL(80Hz)

        -- tperiod min (calculated as 1/max freq)
        tperiod_SCK_serial_rd   : VitalDelayType := UnitDelay; --fSCK=50MHz
        tperiod_SCK_fast_rd     : VitalDelayType := UnitDelay; --fSCK=133MHz
        tperiod_SCK_dual_rd     : VitalDelayType := UnitDelay; --fSCK=104MHz
        tperiod_SCK_quadpg      : VitalDelayType := UnitDelay; --fSCK=80MHz
            -- DDR operation values
        tperiod_SCK_DDR_rd      : VitalDelayType := UnitDelay; --fSCK=66MHz
        tperiod_SCK_DDR80_rd    : VitalDelayType := UnitDelay; --fSCK=80MHz

        -- tdevice values: values for internal delays
        --timing values that are internal to the model and not associated
        --with any port.
        tdevice_PP              : VitalDelayType := 750 us;  --tPP
        -- Typical Byte Programming Time
        tdevice_BP              : VitalDelayType := 400 us;  --tBP
        -- Sector Erase Operation
        tdevice_SE              : VitalDelayType := 1875 ms; --tSE
        -- Bulk Erase Operation
        tdevice_BE              : VitalDelayType := 460 sec; --tBE
        -- WRR Cycle Time
        tdevice_WRR             : VitalDelayType := 200 ms;  --tW
        -- Erase Suspend/Erase Resume Time
        tdevice_ERSSUSP         : VitalDelayType := 40 us;   --tESL
        -- Program Suspend/Program Resume Time
        tdevice_PRGSUSP         : VitalDelayType := 40 us;   --tPSL
        -- VCC (min) to CS# Low
        tdevice_PU              : VitalDelayType := 300 us;  --tPU
      -- PPB Erase Time
        tdevice_PPBERASE         :VitalDelayType := 15 ms;
      -- Password Unlock Time
        tdevice_PASSULCK         :VitalDelayType := 1 us;
      -- Password Unlock Time
        tdevice_PASSACC          :VitalDelayType := 100 us;
        -- Data In Setup Max time
        tdevice_TSU              :VitalDelayType := 300 ns;

    ---------------------------------------------------------------------------
    -- CONTROL GENERICS:
    ---------------------------------------------------------------------------
        -- generic control parameters
        InstancePath      : STRING    := DefaultInstancePath;
        TimingChecksOn    : BOOLEAN   := DefaultTimingChecks;
        MsgOn             : BOOLEAN   := DefaultMsgOn;
        XOn               : BOOLEAN   := DefaultXon;
        -- memory file to be loaded
        mem_file_name     : STRING    := "s25fl512s.mem";
        otp_file_name     : STRING    := "s25fl512sOTP.mem";

        UserPreload       : BOOLEAN   := TRUE; --TRUE;
        LongTimming       : BOOLEAN   := TRUE;


        -- For FMF SDF technology file usage
        TimingModel       : STRING
    );
    PORT (
        -- -- Data Inputs/Outputs
        -- SI                : INOUT std_ulogic := 'U'; -- serial data input/IO0
        -- SO                : INOUT std_ulogic := 'U'; -- serial data output/IO1
        -- -- Controls
        -- SCK               : IN    std_ulogic := 'U'; -- serial clock input
        -- CSNeg             : IN    std_ulogic := 'U'; -- chip select input
        -- RSTNeg            : IN    std_ulogic := 'U'; -- hardware reset pin
        -- WPNeg             : INOUT std_ulogic := 'U'; -- write protect input/IO2
        -- HOLDNeg           : INOUT std_ulogic := 'U'  -- hold input/IO3
        -- Data Inputs/Outputs
        oSI               : OUT std_ulogic; -- serial data input/IO0
        oSO               : OUT std_ulogic; -- serial data output/IO1
        SI                : IN std_ulogic; -- serial data input/IO0
        SO                : IN std_ulogic; -- serial data output/IO1
        -- Controls
        SCK               : IN    std_ulogic := 'U'; -- serial clock input
        CSNeg             : IN    std_ulogic := 'U'; -- chip select input
        RSTNeg            : IN    std_ulogic := 'U'; -- hardware reset pin
        oWPNeg            : OUT std_ulogic; -- write protect input/IO2
        oHOLDNeg          : OUT std_ulogic;  -- hold input/IO3
        WPNeg             : IN std_ulogic; -- write protect input/IO2
        HOLDNeg           : IN std_ulogic  -- hold input/IO3

    );

    ATTRIBUTE VITAL_LEVEL0 of s25fl512s : ENTITY IS TRUE;
END s25fl512s;

-------------------------------------------------------------------------------
-- ARCHITECTURE DECLARATION
-------------------------------------------------------------------------------
-- ARCHITECTURE vhdl_behavioral_dynamic_memory_allocation of s25fl512s IS
--     ATTRIBUTE VITAL_LEVEL0 OF
--     vhdl_behavioral_dynamic_memory_allocation : ARCHITECTURE IS TRUE;

--     ---------------------------------------------------------------------------
--     -- CONSTANT AND SIGNAL DECLARATION
--     ---------------------------------------------------------------------------
--     --Declaration of constants - memory characteristics
--         -- The constant declared here are used to enable the creation of models
--         -- of memories within a family with a minimum amount of editing

--     CONSTANT PartID        : STRING  := "s25fl512s";
--     CONSTANT MaxData       : NATURAL := 16#FF#;        --255;
--     CONSTANT MemSize       : NATURAL := 16#3FFFFFF#;
--     CONSTANT SecNum        : NATURAL := 255;
--     CONSTANT SecSize       : NATURAL := 16#3FFFF#;     --256KB
--     CONSTANT PageNum       : NATURAL := 16#1FFFF#;
--     CONSTANT PageSize      : NATURAL := 512;
--     CONSTANT AddrRANGE     : NATURAL := 16#3FFFFFF#;
--     CONSTANT HiAddrBit     : NATURAL := 31;
--     CONSTANT OTPSize       : NATURAL := 1023;
--     CONSTANT OTPLoAddr     : NATURAL := 16#000#;
--     CONSTANT OTPHiAddr     : NATURAL := 16#3FF#;
--     CONSTANT BYTE          : NATURAL := 8;
--     CONSTANT SFDPHiAddr    : NATURAL := 16#11E6#;
--     CONSTANT IDCFILength   : NATURAL := 16#1E6#;

--     --Manufacturer Identification
--     CONSTANT Manuf_ID      : NATURAL := 16#01#;
--     CONSTANT DeviceID      : NATURAL := 16#19#;
--     --Electronic Signature
--     CONSTANT ESignature    : NATURAL := 16#19#;
--     --Device ID
--     --Manufacturer Identification && Memory Type && Memory Capacity
--     CONSTANT Jedec_ID      : NATURAL := 16#01#; -- first byte of Device ID
--     CONSTANT DeviceID1     : NATURAL := 16#02#;
--     CONSTANT DeviceID2     : NATURAL := 16#20#;
--     CONSTANT ExtendedBytes : NATURAL := 16#4D#;
--     CONSTANT ExtendedID    : NATURAL := 16#00#;
--     CONSTANT DieRev        : NATURAL := 16#00#;
--     CONSTANT MaskRev       : NATURAL := 16#00#;
--     CONSTANT HOLD_CSNeg_RSTNeg : TIME := 34800 ns;

--     -- Declaration of signals that will hold the delayed values of ports
--     SIGNAL SI_ipd          : std_ulogic := 'U';
--     SIGNAL SO_ipd          : std_ulogic := 'U';
--     SIGNAL SCK_ipd         : std_ulogic := 'U';
--     SIGNAL CSNeg_ipd       : std_ulogic := 'U';
--     SIGNAL RSTNeg_ipd      : std_ulogic := 'U';
--     SIGNAL WPNeg_ipd       : std_ulogic := 'U';
--     SIGNAL HOLDNeg_ipd     : std_ulogic := 'U';
--     SIGNAL HOLDNeg_pullup  : std_ulogic := 'U';
--     SIGNAL WPNeg_pullup    : std_ulogic := 'U';
--     SIGNAL RSTNeg_pullup   : std_ulogic := 'U';

--     -- internal delays
--     SIGNAL PP_in           : std_ulogic := '0';
--     SIGNAL PP_out          : std_ulogic := '0';
--     SIGNAL BP_in           : std_ulogic := '0';
--     SIGNAL BP_out          : std_ulogic := '0';
--     SIGNAL SE_in           : std_ulogic := '0';
--     SIGNAL SE_out          : std_ulogic := '0';
--     SIGNAL BE_in           : std_ulogic := '0';
--     SIGNAL BE_out          : std_ulogic := '0';
--     SIGNAL WRR_in          : std_ulogic := '0';
--     SIGNAL WRR_out         : std_ulogic := '0';
--     SIGNAL ERSSUSP_in      : std_ulogic := '0';
--     SIGNAL ERSSUSP_out     : std_ulogic := '0';
--     SIGNAL ERSSUSP_out_sdf : std_ulogic := '0';
--     SIGNAL PRGSUSP_in      : std_ulogic := '0';
--     SIGNAL PRGSUSP_out     : std_ulogic := '0';
--     SIGNAL PRGSUSP_out_sdf     : std_ulogic := '0';
--     SIGNAL PU_in           : std_ulogic := '0';
--     SIGNAL PU_out          : std_ulogic := '0';
--     SIGNAL RST_in          : std_ulogic := '0';-- Hardware Reset Timeout
--     SIGNAL RST_out         : std_ulogic := '1';--
--     SIGNAL PPBERASE_in     : std_ulogic := '0';
--     SIGNAL PPBERASE_out    : std_ulogic := '0';
--     SIGNAL PPBERASE_out_sdf    : std_ulogic := '0';
--     SIGNAL PASSULCK_in     : std_ulogic := '0';
--     SIGNAL PASSULCK_out    : std_ulogic := '0';
--     SIGNAL PASSULCK_out_sdf    : std_ulogic := '0';
--     SIGNAL PASSACC_in      : std_ulogic := '0';
--     SIGNAL PASSACC_out     : std_ulogic := '0';
--     SIGNAL PASSACC_out_sdf     : std_ulogic := '0';

--     ---------------------------------------------------------------------------
--     -- Memory data initial value.
--     -- Default value may be overridden by conigure_memory procedure
--     ---------------------------------------------------------------------------
--     SHARED VARIABLE max_data     : NATURAL := 16#FF#;



--     ---------------------------------------------------------------------------
--     -- Handle dynamic memory allocation
--     ---------------------------------------------------------------------------
--     -- Partition dynamically allocated space for performance

--     SHARED VARIABLE corrupt_Sec : std_logic_vector(SecNum downto 0)
--                                                             :=(OTHERS=>'0');

--     -- ------------------------------------------------------------------------
--     -- Data types required to implement link list structure
--     -- ------------------------------------------------------------------------
--     TYPE mem_data_t;
--     TYPE mem_data_pointer_t IS ACCESS mem_data_t;
--     TYPE mem_data_t IS RECORD
--         key_address  :  INTEGER;
--         val_data     :  INTEGER;
--         successor    :  mem_data_pointer_t;
--     END RECORD;

--     -- ---------------------------------------------------------------------
--     -- Array of linked lists.
--     -- Support memory region partitioning for faster access.
--     -- ---------------------------------------------------------------------
--     TYPE mem_data_pointer_array_t IS
--         ARRAY(NATURAL RANGE <>) OF mem_data_pointer_t;

--     SHARED VARIABLE linked_list       :
--                          mem_data_pointer_array_t(0 TO SecNum);

--     -- ---------------------------------------------------------------------
--     -- Override mechanism provided for default parameter values
--     -- ---------------------------------------------------------------------
--     PROCEDURE configure_memory(
--         max_data_c   :  IN INTEGER) IS
--     BEGIN
--         max_data := max_data_c;
--     END PROCEDURE configure_memory;

--     -- Asure proper initialization
--     PROCEDURE initialize IS
--         VARIABLE I  :  INTEGER;
--     BEGIN
--         FOR I IN 0 TO SecNum LOOP
--             linked_list(I) := NULL;
--         END LOOP;
--     END PROCEDURE initialize;

--     -- ---------------------------------------------------------------------
--     -- Create linked listed
--     -- ---------------------------------------------------------------------
--     PROCEDURE create_list(
--         key_address  :  IN INTEGER;
--         val_data     :  IN INTEGER;
--         root         :  INOUT mem_data_pointer_t) IS
--     BEGIN
--         root := NEW mem_data_t;
--         root.successor := NULL;
--         root.key_address := key_address;
--         root.val_data := val_data;
--     END PROCEDURE create_list;

--     -- --------------------------------------------------------------------
--     -- Iterate through linked listed comapring key values
--     -- Stop when key value greater or equal
--     -- --------------------------------------------------------------------
--     PROCEDURE position_list(
--         key_address  :  IN INTEGER;
--         root         :  INOUT mem_data_pointer_t;
--         found        :  INOUT mem_data_pointer_t;
--         prev         :  INOUT mem_data_pointer_t) IS
--     BEGIN
--         found := root;
--         prev := NULL;
--         WHILE ((found /= NULL) AND (found.key_address < key_address)) LOOP
--             prev := found;
--             found := found.successor;
--         END LOOP;
--     END PROCEDURE position_list;

--     -- -------------------------------------------------------------------
--     -- Add new element to a linked list
--     -- -------------------------------------------------------------------
--     PROCEDURE insert_list(
--         key_address  :  IN INTEGER;
--         val_data     :  IN INTEGER;
--         root         :  INOUT mem_data_pointer_t) IS

--         VARIABLE new_element  :  mem_data_pointer_t;
--         VARIABLE found        :  mem_data_pointer_t;
--         VARIABLE prev         :  mem_data_pointer_t;
--     BEGIN
--         position_list(key_address, root, found, prev);

--         -- Insert at list tail
--         IF (found = NULL) THEN
--             prev.successor := NEW mem_data_t;
--             prev.successor.key_address := key_address;
--             prev.successor.val_data := val_data;
--             prev.successor.successor := NULL;
--         ELSE
--             -- Element exists, update memory data value
--             IF (found.key_address = key_address) THEN
--                 found.val_data := val_data;
--             ELSE
--                 -- No element found, allocate and link
--                 new_element := NEW mem_data_t;
--                 new_element.key_address := key_address;
--                 new_element.val_data := val_data;
--                 new_element.successor := found;
--                 -- Possible root position
--                 IF (prev /= NULL) THEN
--                     prev.successor := new_element;
--                 ELSE
--                     root := new_element;
--                 END IF;
--             END IF;
--         END IF;
--     END PROCEDURE insert_list;

--     -- --------------------------------------------------------------------
--     -- Remove element from a linked list
--     -- --------------------------------------------------------------------
--     PROCEDURE remove_list(
--         key_address  :  IN INTEGER;
--         root         :  INOUT mem_data_pointer_t) IS

--         VARIABLE found      :  mem_data_pointer_t;
--         VARIABLE prev       :  mem_data_pointer_t;
--     BEGIN
--         position_list(key_address, root, found, prev);
--         IF (found /= NULL) THEN
--             -- Key value match
--             IF (found.key_address = key_address) THEN
--                 -- Handle root position removal
--                 IF (prev /= NULL) THEN
--                     prev.successor := found.successor;
--                 ELSE
--                     root := found.successor;
--                 END IF;
--                 DEALLOCATE(found);
--             END IF;
--         END IF;
--     END PROCEDURE remove_list;

--     -- -------------------------------------------------------------------
--     -- Remove range of elements from a linked list
--     -- Higher performance than one-by-one removal
--     -- -------------------------------------------------------------------
--     PROCEDURE remove_list_range(
--         address_low  :  IN INTEGER;
--         address_high :  IN INTEGER;
--         root         :  INOUT mem_data_pointer_t) IS

--         VARIABLE iter          :  mem_data_pointer_t;
--         VARIABLE prev          :  mem_data_pointer_t;
--         VARIABLE link_element  :  mem_data_pointer_t;
--     BEGIN
--         iter := root;
--         prev := NULL;
--         -- Find first linked list element belonging to
--         -- a specified address range [address_low, address_high]
--         WHILE ((iter /= NULL) AND NOT (
--         (iter.key_address >= address_low) AND
--         (iter.key_address <= address_high))) LOOP
--             prev := iter;
--             iter := iter.successor;
--         END LOOP;
--         -- Continue until address_high reached
--         -- Deallocate linked list elements pointed by iterator
--         IF (iter /= NULL) THEN
--             WHILE ((iter /= NULL) AND
--             (iter.key_address >= address_low) AND
--             (iter.key_address <= address_high)) LOOP
--                 link_element := iter.successor;
--                 DEALLOCATE(iter);
--                 iter := link_element;
--             END LOOP;
--             -- Handle possible root value change
--             IF prev /= NULL THEN
--                 prev.successor := link_element;
--             ELSE
--                 root := link_element;
--             END IF;
--         END IF;
--     END PROCEDURE remove_list_range;

--     -- ---------------------------------------------------------------------
--     -- Address range to be erased
--     -- ---------------------------------------------------------------------
--     PROCEDURE erase_mem(
--         address_low      :  IN INTEGER;
--         address_high     :  IN INTEGER;
--         linked_list      :  INOUT mem_data_pointer_t) IS

--     BEGIN
--         remove_list_range(
--             address_low,
--             address_high,
--             linked_list
--             );
--     END PROCEDURE erase_mem;

--     -- --------------------------------------------------------------------
--     -- Memory READ operation performed above dynamically allocated space
--     -- --------------------------------------------------------------------
--     PROCEDURE read_mem(
--         linked_list  :  INOUT mem_data_pointer_t;
--         data         :  INOUT INTEGER;
--         address      :  IN INTEGER) IS

--         VARIABLE found     :  mem_data_pointer_t;
--         VARIABLE prev      :  mem_data_pointer_t;
--         VARIABLE mem_data  :  INTEGER;
--     BEGIN
--         IF (linked_list = NULL) THEN
--             -- Not allocated, not written, initial value
--             mem_data := max_data ;
--         ELSE
--             position_list(address, linked_list, found, prev);
--             IF (found /= NULL) THEN
--                 IF found.key_address = address THEN
--                     -- Allocated, val_data stored
--                     mem_data := found.val_data;
--                 ELSE
--                     -- Not allocated, not written, initial value
--                     mem_data := max_data ;
--                 END IF;
--             ELSE
--                 -- Not allocated, not written, initial value
--                 mem_data := max_data ;
--             END IF;
--         END IF;
--         data := mem_data;
--     END PROCEDURE read_mem;

--     -- ------------------------------------------------------------------
--     -- Memory WRITE operation performed above dynamically allocated space
--     -- ------------------------------------------------------------------
--     PROCEDURE write_mem(
--         linked_list  :  INOUT mem_data_pointer_t;
--         address      :  IN INTEGER;
--         data         :  IN INTEGER) IS

--     BEGIN
--         IF (data /= max_data ) THEN
--             -- Handle possible root value update
--             IF (linked_list /= NULL) THEN
--                 insert_list(address, data, linked_list);
--             ELSE
--                 create_list(address, data, linked_list);
--             END IF;
--         ELSE
--             -- Deallocate if initial value written
--             -- No linked list, NOP, initial value implicit
--             IF (linked_list /= NULL) THEN
--                 remove_list(address, linked_list);
--             END IF;
--         END IF;
--     END PROCEDURE write_mem;

--     PROCEDURE READ_DATA(
--             sectoraddr     : IN NATURAL RANGE 0 TO Secnum;
--             addressinsec   : IN NATURAL RANGE 0 TO SecSize;
--             ReadData       : INOUT INTEGER) IS
--     BEGIN
--         read_mem(linked_list(sectoraddr),
--                  ReadData,
--                  addressinsec
--                 );
--         IF (ReadData = MaxData AND Corrupt_Sec(sectoraddr) = '1') THEN
--             ReadData := -1;
--         ELSIF (ReadData = MaxData+1) AND Corrupt_Sec(sectoraddr) = '1' THEN
--             ReadData := MaxData;
--         END IF;
--     END READ_DATA;

--     PROCEDURE WRITE_DATA(
--         sectoraddr     : IN NATURAL RANGE 0 TO Secnum;
--         addressinsec   : IN NATURAL RANGE 0 TO SecSize;
--         WriteData      : IN INTEGER) IS
--     BEGIN
--         IF (WriteData = MaxData AND Corrupt_Sec(sectoraddr) = '1') THEN
--             write_mem(linked_list(sectoraddr),
--                       addressinsec,
--                       WriteData+1
--                       );
--         ELSE
--             write_mem(linked_list(sectoraddr),
--                       addressinsec,
--                       WriteData
--                       );
--         END IF;
--     END WRITE_DATA;

--     FUNCTION ReturnSectorID(ADDR : NATURAL) RETURN NATURAL IS
--             VARIABLE result : NATURAL;
--         BEGIN
--             IF ADDR <= AddrRange THEN
--                 result := ADDR / (SecSize+1);
--             ELSE
--                 result := SecNum+1;
--             END IF;
--             RETURN result;
--         END ReturnSectorID;

--     FUNCTION ReturnSectorIDRdPswdMd(TBPROT : std_logic) RETURN NATURAL IS
--             VARIABLE result : NATURAL;
--         BEGIN
--             result := SecNum;
--             IF TBPROT = '0' THEN
--                 result := 0;
--             END IF;
--             RETURN result;
--         END ReturnSectorIDRdPswdMd;

-- BEGIN

--     ---------------------------------------------------------------------------
--     -- Internal Delays
--     ---------------------------------------------------------------------------
--     -- Artificial VITAL primitives to incorporate internal delays
--     -- Because a tdevice generics is used, there must be a VITAL_primitives
--     -- assotiated with them
--     PP        :VitalBuf(PP_out,      PP_in,      (tdevice_PP      ,UnitDelay));
--     BP        :VitalBuf(BP_out,      BP_in,      (tdevice_BP      ,UnitDelay));
--     SE        :VitalBuf(SE_out,      SE_in,      (tdevice_SE      ,UnitDelay));
--     BE        :VitalBuf(BE_out,      BE_in,      (tdevice_BE      ,UnitDelay));
--     WRR       :VitalBuf(WRR_out,     WRR_in,     (tdevice_WRR     ,UnitDelay));
--     ERSSUSP   :VitalBuf(ERSSUSP_out_sdf, ERSSUSP_in, (tdevice_ERSSUSP ,UnitDelay));
--     PRGSUSP   :VitalBuf(PRGSUSP_out_sdf, PRGSUSP_in, (tdevice_PRGSUSP ,UnitDelay));
--     PU        :VitalBuf(PU_out,      PU_in,      (tdevice_PU      ,UnitDelay));
--     PPBERASE  :VitalBuf(PPBERASE_out_sdf,PPBERASE_in,(tdevice_PPBERASE,UnitDelay));
--     PASSULCK  :VitalBuf(PASSULCK_out_sdf,PASSULCK_in,(tdevice_PASSULCK,UnitDelay));
--     PASSACC   :VitalBuf(PASSACC_out_sdf, PASSACC_in, (tdevice_PASSACC ,UnitDelay));


--     ---------------------------------------------------------------------------
--     -- Wire Delays
--     ---------------------------------------------------------------------------
--     WireDelay : BLOCK
--     BEGIN

--         w_1 : VitalWireDelay (SI_ipd,      SI,      tipd_SI);
--         w_2 : VitalWireDelay (SO_ipd,      SO,      tipd_SO);
--         w_3 : VitalWireDelay (SCK_ipd,     SCK,     tipd_SCK);
--         w_4 : VitalWireDelay (CSNeg_ipd,   CSNeg,   tipd_CSNeg);
--         w_5 : VitalWireDelay (RSTNeg_ipd,  RSTNeg,  tipd_RSTNeg);
--         w_6 : VitalWireDelay (WPNeg_ipd,   WPNeg,   tipd_WPNeg);
--         w_7 : VitalWireDelay (HOLDNeg_ipd, HOLDNeg, tipd_HOLDNeg);

--     END BLOCK;

--     ---------------------------------------------------------------------------
--     -- Main Behavior Block
--     ---------------------------------------------------------------------------
--     Behavior: BLOCK

--         PORT (
--             SIIn           : IN    std_ulogic := 'U';
--             SIOut          : OUT   std_ulogic := 'U';
--             SOIn           : IN    std_logic  := 'U';
--             SOOut          : OUT   std_logic  := 'U';
--             SCK            : IN    std_ulogic := 'U';
--             CSNeg          : IN    std_ulogic := 'U';
--             RSTNeg         : IN    std_ulogic := 'U';
--             HOLDNegIn      : IN    std_ulogic := 'U';
--             HOLDNegOut     : OUT   std_ulogic := 'U';
--             WPNegIn        : IN    std_ulogic := 'U';
--             WPNegOut       : OUT   std_ulogic := 'U'
--         );

--         PORT MAP (
--              SIIn       => SI_ipd,
--              SIOut      => oSI,
--              SOIn       => SO_ipd,
--              SOOut      => oSO,
--              SCK        => SCK_ipd,
--              CSNeg      => CSNeg_ipd,
--              RSTNeg     => RSTNeg_ipd,
--              HOLDNegIn  => HOLDNeg_ipd,
--              HOLDNegOut => oHOLDNeg,
--              WPNegIn    => WPNeg_ipd,
--              WPNegOut   => oWPNeg
--         );

--         -- State Machine : State_Type
--         TYPE state_type IS (IDLE,
--                             RESET_STATE,
--                             AUTOBOOT,
--                             WRITE_SR,
--                             PAGE_PG,
--                             OTP_PG,
--                             PG_SUSP,
--                             SECTOR_ERS,
--                             BULK_ERS,
--                             ERS_SUSP,
--                             ERS_SUSP_PG,
--                             ERS_SUSP_PG_SUSP,
--                             PASS_PG,
--                             PASS_UNLOCK,
--                             PPB_PG,
--                             PPB_ERS,
--                             AUTOBOOT_PG,
--                             ASP_PG,
--                             PLB_PG,
--                             DYB_PG,
--                             NVDLR_PG
--                             );

--         -- Instruction Type
--         TYPE instruction_type IS ( NONE,
--                                    WREN,       -- Write Enable
--                                    WRDI,       -- Write Disable
--                                    WRR,        -- Write Register
--                                    READ,       -- Read Normal (3Byte Address)
--                                    RD4,        -- Read Normal (4Byte +)
--                                    OTPR,       -- OTP Read
--                                    RDSR,       -- Read Status Register 1
--                                    RDSR2,      -- Read Status Register 2
--                                    RDCR,       -- Read Configuration Register 1
--                                    REMS,       -- Read ID (SST)
--                                    RDID,       -- Read ID JEDEC
--                                    RES,        -- Read ID
--                                    FSTRD,      -- Fast Read (3Byte Address)
--                                    FSTRD4,     -- Fast Read (4Byte +)
--                                    DDRFR,      -- Fast Read DDR (3Byte Address)
--                                    DDRFR4,     -- Fast Read DDR (4Byte +)
--                                    DOR,        -- Read Dual Out (3Byte Address)
--                                    DOR4,       -- Read Dual Out (4Byte +)
--                                    DIOR,       -- Read Dual I/O (3Byte Address)
--                                    DIOR4,      -- Read Dual I/O (4Byte +)
--                                    DDRDIOR,    -- Read DDR Dual I/O (3Byte)
--                                    DDRDIOR4,   -- Read DDR Dual I/O (4Byte +)
--                                    QOR,        -- Read Quad Out (3Byte Address)
--                                    QOR4,       -- Read Quad Out (4Byte +)
--                                    QIOR,       -- Read Quad I/O (3Byte Address)
--                                    QIOR4,      -- Read Quad I/O (4Byte +)
--                                    DDRQIOR,    -- Read DDR Quad I/O (3Byte)
--                                    DDRQIOR4,   -- Read DDR Quad I/O (4Byte +)
--                                    PP,         -- Program Page (3Byte Address)
--                                    PP4,        -- Program Page (4Byte +)
--                                    QPP,        -- Quad Page Program (3Byte)
--                                    QPP4,       -- Quad Page Program (4Byte +)
--                                    OTPP,       -- OTP Program
--                                    PGSP,       -- Program Suspend
--                                    PGRS,       -- Program Resume
--                                    BE,         -- Bulk Erase
--                                    SE,         -- Erase 128/256KB (3Byte)
--                                    SE4,        -- Erase 128/256KB (4Byte +)
--                                    P4E,        -- 4KB-sector Erase (3Byte Addr)
--                                    P4E4,       -- 4KB-sector Erase (4Byte Addr)
--                                    ERSP,       -- Erase Suspend
--                                    ERRS,       -- Erase Resume
--                                    ABRD,       -- AutoBoot Register Read
--                                    ABWR,       -- AutoBoot Register Write
--                                    BRRD,       -- Bank Register Read
--                                    BRWR,       -- Bank Register Write
--                                    BRAC,       -- Bank Register Access
--                                    ECCRD,      -- ECC Register Read
--                                    DLPRD,      -- Read Data Learning Pattern
--                                    PNVDLR,     -- Program NVDLP Reg
--                                    WVDLR,      -- Write Volatile DLP Reg
--                                    ASPRD,      -- ASP Read
--                                    ASPP,       -- ASP Program
--                                    DYBRD,      -- DYB Read
--                                    DYBWR,      -- DYB Write
--                                    PPBRD,      -- PPB Read
--                                    PPBP,       -- PPB Program
--                                    PPBERS,     -- PPB Erase
--                                    PLBWR,      -- PPB Lock Bit Write
--                                    PLBRD,      -- PPB Lock Bit Read
--                                    PASSRD,     -- Password Read
--                                    PASSP,      -- Password Program
--                                    PASSU,      -- Password Unlock
--                                    RESET,      -- Reset
--                                    MBR,        -- Mode Bit Reset
--                                    MPM,        -- Multi-I/O-High Perf Mode
--                                    CLSR,       -- Clear Status Register
--                                    RSFDP
--                                 );

--         TYPE WByteType IS ARRAY (0 TO 511) OF INTEGER RANGE -1 TO MaxData;
--         -- OTP Memory Array
--         TYPE OTPArray IS ARRAY (OTPLoAddr TO OTPHiAddr) OF INTEGER
--                                                     RANGE -1 TO MaxData;
--         --SFDP Array
--         TYPE SFDPtype  IS ARRAY (16#00# TO SFDPHiAddr) OF
--                                               INTEGER RANGE 0 TO 16#FF#;
--         -----------------------------------------------------------------------
--         --  memory declaration
--         -----------------------------------------------------------------------
--         -- OTP Sector
--         SHARED VARIABLE OTPMem       : OTPArray  := (OTHERS => MaxData);
--         --SFDP Array
--         SHARED VARIABLE SFDP_array    : SFDPtype   := (OTHERS => 0);
--         -- Programming Buffer
--         SIGNAL WByte                 : WByteType := (OTHERS => MaxData);



--         -- states
--         SIGNAL current_state         : state_type := RESET_STATE;
--         SIGNAL next_state            : state_type := RESET_STATE;

--         SIGNAL Instruct              : instruction_type;
--         --zero delay signal
--         SIGNAL SOOut_zd              : std_logic := 'Z';
--         SIGNAL SIOut_zd              : std_logic := 'Z';
--         SIGNAL HOLDNegOut_zd         : std_logic := 'Z';
--         SIGNAL WPNegOut_zd           : std_logic := 'Z';
--         --HOLD delay on output data
--         SIGNAL SOOut_z               : std_logic := 'Z';
--         SIGNAL SIOut_z               : std_logic := 'Z';
--         -- powerup
--         SIGNAL PoweredUp             : std_logic := '0';

--         -----------------------------------------------------------------------
--         -- Registers
--         -----------------------------------------------------------------------
--         --     ***  Status Register 1  ***
--         SHARED VARIABLE Status_reg1   : std_logic_vector(7 downto 0)
--                                                 := (others => '0');
--         SIGNAL Status_reg1_in         : std_logic_vector(7 downto 0)
--                                                 := (others => '0');

--         -- Status Register Write Disable Bit
--         ALIAS SRWD      :std_logic IS Status_reg1(7);
--         -- Status Register Programming Error Bit
--         ALIAS P_ERR     :std_logic IS Status_reg1(6);
--         -- Status Register Erase Error Bit
--         ALIAS E_ERR     :std_logic IS Status_reg1(5);
--         -- Status Register Block Protection Bits
--         ALIAS BP2       :std_logic IS Status_reg1(4);
--         ALIAS BP1       :std_logic IS Status_reg1(3);
--         ALIAS BP0       :std_logic IS Status_reg1(2);
--         -- Status Register Write Enable Latch Bit
--         ALIAS WEL       :std_logic IS Status_reg1(1);
--         -- Status Register Write In Progress Bit
--         ALIAS WIP       :std_logic IS Status_reg1(0);

--         --     ***  Status Register 2  ***
--         SHARED VARIABLE Status_reg2   : std_logic_vector(7 downto 0)
--                                                 := (others => '0');
--         SIGNAL Status_reg2_in         : std_logic_vector(7 downto 0)
--                                                 := (others => '0');

--         -- Status Register Write Enable Latch Bit
--         ALIAS ES        :std_logic IS Status_reg2(1);
--         -- Status Register Write In Progress Bit
--         ALIAS PS        :std_logic IS Status_reg2(0);

--         --      ***  Configuration Register 1  ***
--         SHARED VARIABLE Config_reg1   : std_logic_vector(7 downto 0)
--                                                 := (others => '0');
--         SIGNAL Config_reg1_in         : std_logic_vector(7 downto 0)
--                                                 := (others => '0');

--         -- Latency code
--         ALIAS LC1       :std_logic IS Config_reg1(7);
--         ALIAS LC0       :std_logic IS Config_reg1(6);
--         -- Configuration Register TBPROT bit
--         ALIAS TBPROT    :std_logic IS Config_reg1(5);
-- --        -- Configuration Register LOCK bit
-- --        ALIAS LOCK      :std_logic IS Config_reg1(4);
--         -- Configuration Register BPNV bit
--         ALIAS BPNV      :std_logic IS Config_reg1(3);
--         -- Configuration Register QUAD bit
--         ALIAS QUAD      :std_logic IS Config_reg1(1);
--         -- Configuration Register FREEZE bit
--         ALIAS FREEZE    :std_logic IS Config_reg1(0);

--         --      ***  VDLR Register  ***
--         SHARED VARIABLE VDLR_reg      : std_logic_vector(7 downto 0)
--                                                 := (others => '0');
--         SIGNAL VDLR_reg_in            : std_logic_vector(7 downto 0)
--                                                 := (others => '0');

--         --      ***  NVDLR Register  ***
--         SHARED VARIABLE NVDLR_reg     : std_logic_vector(7 downto 0)
--                                                 := (others => '0');
--         SIGNAL NVDLR_reg_in           : std_logic_vector(7 downto 0)
--                                                 := (others => '0');

--         --      ***  AutoBoot Register  ***
--         SHARED VARIABLE AutoBoot_reg   : std_logic_vector(31 downto 0)
--                                                 := (others => '0');
--         SIGNAL AutoBoot_reg_in         : std_logic_vector(31 downto 0)
--                                                 := (others => '0');
--         --AutoBoot Enable Bit
--         ALIAS ABE       :std_logic IS AutoBoot_reg(0);

--         --      ***  Bank Address Register  ***
--         SHARED VARIABLE Bank_Addr_reg  : std_logic_vector(7 downto 0)
--                                                 := (others => '0');
--         SIGNAL Bank_Addr_reg_in        : std_logic_vector(7 downto 0)
--                                                 := (others => '0');
--         --Bank Address Register EXTADD bit
--         ALIAS EXTADD    :std_logic IS Bank_Addr_reg(7);
--         --      ***  ECC Status Register  ***
--         SHARED VARIABLE ECCSR     : std_logic_vector(7 downto 0)
--                                                 := (others => '0');
--         ALIAS EECC        :std_logic IS ECCSR(2);
--         ALIAS EECCD       :std_logic IS ECCSR(1);
--         ALIAS ECCDI       :std_logic IS ECCSR(0);

--         --      ***  ASP Register  ***
--         SHARED VARIABLE ASP_reg        : std_logic_vector(15 downto 0);
--         SIGNAL ASP_reg_in              : std_logic_vector(15 downto 0)
--                                                      := (others => '1');
--         --Read Password Mode Enable Bit
--         ALIAS RPME      :std_logic IS ASP_reg(5);
--         --PPB OTP Bit
--         ALIAS PPBOTP    :std_logic IS ASP_reg(3);
--         -- Password Protection Mode Lock Bit
--         ALIAS PWDMLB    :std_logic IS ASP_reg(2);
--         --Persistent Protection Mode Lock Bit
--         ALIAS PSTMLB    :std_logic IS ASP_reg(1);

--         --      ***  Password Register  ***
--         SHARED VARIABLE Password_reg   : std_logic_vector(63 downto 0)
--                                                 := (others => '1');
--         SIGNAL Password_reg_in         : std_logic_vector(63 downto 0)
--                                                 := (others => '1');
--         --      ***  PPB Lock Register  ***
--         SHARED VARIABLE PPBL           : std_logic_vector(7 downto 0)
--                                                 := (others => '0');
--         SIGNAL PPBL_in                 : std_logic_vector(7 downto 0)
--                                                 := (others => '0');
--         --Persistent Protection Mode Lock Bit
--         ALIAS PPB_LOCK                  : std_logic IS PPBL(0);
--         SIGNAL PPB_LOCK_temp            : std_ulogic := '0';

--         --      ***  PPB Access Register  ***
--         SHARED VARIABLE PPBAR          : std_logic_vector(7 downto 0)
--                                                 := (others => '1');
--         SIGNAL PPBAR_in                : std_logic_vector(7 downto 0)
--                                                 := (others => '1');
--         -- PPB_bits(Sec)
--         SHARED VARIABLE PPB_bits       : std_logic_vector(SecNum downto 0)
--                                                 := (OTHERS => '1');
--         --      ***  DYB Access Register  ***
--         SHARED VARIABLE DYBAR          : std_logic_vector(7 downto 0)
--                                                 := (others => '1');
--         SIGNAL DYBAR_in                : std_logic_vector(7 downto 0)
--                                                 := (others => '1');
--         -- DYB(Sec)
--         SHARED VARIABLE DYB_bits       : std_logic_vector(SecNum downto 0);

--         -- The Lock Protection Registers for OTP Memory space
--         SHARED VARIABLE LOCK_BYTE1 :std_logic_vector(7 downto 0);
--         SHARED VARIABLE LOCK_BYTE2 :std_logic_vector(7 downto 0);
--         SHARED VARIABLE LOCK_BYTE3 :std_logic_vector(7 downto 0);
--         SHARED VARIABLE LOCK_BYTE4 :std_logic_vector(7 downto 0);

--         --Command Register
--         SIGNAL write              : std_logic := '0';
--         SIGNAL cfg_write          : std_logic := '0';
--         SIGNAL read_out           : std_logic := '0';

--         SIGNAL rd                 : boolean   := false;
--         SIGNAL dual               : boolean   := false;
--         SIGNAL fast_rd            : boolean   := true;
--         SIGNAL ddr                : boolean   := false;
--         SIGNAL ddr80              : boolean   := false;
--         SIGNAL any_read           : boolean   := false;

--         SIGNAL quadpg             : boolean   := false;

--         SIGNAL oe                 : boolean   := false;
--         SIGNAL oe_z               : boolean   := false;

--         SHARED VARIABLE hold_mode : boolean := false;

--         --FSM control signals
--         SIGNAL PDONE              : std_logic := '1'; --Page Prog. Done
--         SIGNAL PSTART             : std_logic := '0'; --Start Page Programming
--         SIGNAL PGSUSP             : std_logic := '0'; --Suspend Program
--         SIGNAL PGRES              : std_logic := '0'; --Resume Program

--         SIGNAL TSU                : std_logic := '0'; --Resume Program

--         SIGNAL RES_TO_SUSP_MIN_TIME : std_logic := '0';--Resume to Suspend Flag
--         SIGNAL RES_TO_SUSP_TYP_TIME : std_logic := '0';--Resume to Suspend Flag

--         SIGNAL WDONE              : std_logic := '1'; --Write operation Done
--         SIGNAL WSTART             : std_logic := '0'; --Start Write operation

--         SIGNAL ESTART             : std_logic := '0'; --Start Erase operation
--         SIGNAL EDONE              : std_logic := '1'; --Erase operation Done
--         SIGNAL ESUSP              : std_logic := '0'; --Suspend Erase
--         SIGNAL ERES               : std_logic := '0'; --Resume Erase

--         --reset timing
--         SIGNAL RST                 : std_logic := '0';
--         SIGNAL Reseted             : std_logic := '0'; --Reset Timing Control
--         --Lock Bit is enabled for customer programming
-- --        SIGNAL WRLOCKENABLE       : BOOLEAN   := TRUE;
--         --Flag that mark if ASP Register is allready programmed
--         SIGNAL ASPOTPFLAG         : BOOLEAN   := FALSE;
--         SIGNAL ASP_INIT           : NATURAL RANGE 0 TO 1;
--         SIGNAL INITIAL_CONFIG     : std_logic := '0';

--         SHARED VARIABLE SecAddr   : NATURAL RANGE 0 TO SecNum:= 0;

--         SHARED VARIABLE Sec_addr  : NATURAL   := 0;

--         SHARED VARIABLE Page_addr : NATURAL;
--         SHARED VARIABLE pgm_page  : NATURAL;

-- --        SHARED VARIABLE QPP_page       : std_logic_vector(PageNum64 downto 0)
-- --                                                       := (OTHERS => '0');

--         --Flag for Password unlock command
--         SIGNAL PASS_UNLOCKED      : boolean   := FALSE;
--         SIGNAL PASS_TEMP          : std_logic_vector(63 downto 0)
--                                                 := (others => '1');

--         SHARED VARIABLE DOUBLE    : BOOLEAN := FALSE;
--         SHARED VARIABLE EHP       : BOOLEAN := FALSE;

--         SHARED VARIABLE read_cnt  : NATURAL := 0;
--         SHARED VARIABLE byte_cnt  : NATURAL := 1;
--         SHARED VARIABLE read_addr : NATURAL RANGE 0 TO AddrRANGE ;
--         SHARED VARIABLE read_addr_tmp : NATURAL;

--         SHARED VARIABLE start_delay : NATURAL RANGE 0 TO 7;
--         SHARED VARIABLE ABSD        : NATURAL RANGE 0 TO 7;
--         SIGNAL start_autoboot       : std_logic := '0';

--         SIGNAL change_addr        : std_logic := '0';
--         SIGNAL Address            : NATURAL RANGE 0 TO AddrRANGE := 0;
--         SIGNAL SectorSuspend      : NATURAL RANGE 0 TO SecNum := 0;

--         -- Sector address
--         SIGNAL SA                 : NATURAL RANGE 0 TO SecNum := 0;

--         -- Sector is protect if Sec_Prot(SecNum) = '1'
--         SHARED VARIABLE Sec_Prot  : std_logic_vector(SecNum downto 0) :=
--                                                    (OTHERS => '0');

--         SIGNAL change_BP          : std_logic := '0';
--         SHARED VARIABLE BP_bits   : std_logic_vector(2 downto 0) := "000";

--         SHARED VARIABLE CFI_array_tmp    : std_logic_vector(647 downto 0);

--         SIGNAL Byte_number        : NATURAL RANGE 0 TO 511    := 0;

--         TYPE bus_cycle_type IS (STAND_BY,
--                                 OPCODE_BYTE,
--                                 ADDRESS_BYTES,
--                                 DUMMY_BYTES,
--                                 MODE_BYTE,
--                                 DATA_BYTES
--                                 );
--         SHARED VARIABLE bus_cycle_state    : bus_cycle_type;
--         -- switch between Data bytes and Dummy bytes
--         SHARED VARIABLE DummyBytes_act     : X01 := '0';
--         SIGNAL dummy_cnt_act_temp          : NATURAL := 0;
--         SIGNAL dummy_cnt_act               : NATURAL := 0;
--         --Read Password Protection Mode Active flag
--         SIGNAL RdPswdProtMode              : std_ulogic := '0';
--         --Read Password Protection Mode Support flag
--         SIGNAL RdPswdProtEnable            : std_ulogic := '0';
--         SIGNAL BAR_ACC                     : std_ulogic := '0';

--         SHARED VARIABLE Latency_code       : NATURAL RANGE 0 TO 7;
--         SHARED VARIABLE opcode_cnt         : NATURAL := 0;
--         SHARED VARIABLE addr_cnt           : NATURAL := 0;
--         SHARED VARIABLE mode_cnt           : NATURAL := 0;
--         SHARED VARIABLE dummy_cnt          : NATURAL := 0;
--         SHARED VARIABLE data_cnt           : NATURAL := 0;

--         -- timing check violation
--         SIGNAL Viol               : X01 := '0';

--         PROCEDURE ADDRHILO_SEC(
--             VARIABLE   AddrLOW  : INOUT NATURAL RANGE 0 to ADDRRange;
--             VARIABLE   AddrHIGH : INOUT NATURAL RANGE 0 to ADDRRange;
--             VARIABLE   Addr     : NATURAL) IS
--             VARIABLE   sector   : NATURAL RANGE 0 TO SecNum;
--         BEGIN
--             sector   := Addr/16#40000#;
--             AddrLOW  := sector*16#40000#;
--             AddrHIGH := sector*16#40000# + 16#3FFFF#;
--         END ADDRHILO_SEC;

--         PROCEDURE ADDRHILO_PG(
--             VARIABLE   AddrLOW  : INOUT NATURAL RANGE 0 to ADDRRange;
--             VARIABLE   AddrHIGH : INOUT NATURAL RANGE 0 to ADDRRange;
--             VARIABLE   Sec_address : INOUT NATURAL RANGE 0 TO SecSize;
--             VARIABLE   Page_address : INOUT NATURAL;
--             VARIABLE   Addr     : NATURAL) IS
--             VARIABLE   page     : NATURAL RANGE 0 TO PageNum;
--             VARIABLE   Sec      : NATURAL RANGE 0 TO SecNum;
--             VARIABLE   Sec_addr_tmp : NATURAL RANGE 0 TO ADDRRange;
--         BEGIN
--             Sec      := Addr/(SecSize+1);
--             Sec_addr_tmp := Sec*(SecSize+1);
--             page     := Addr/PageSize;
--             AddrLOW  := Page*PageSize;
--             AddrLOW  := AddrLOW - Sec_addr_tmp;
--             AddrHIGH := AddrLOW + PageSize-1;
--             Sec_address := Addr - Sec_addr_tmp;
--             Page_address := page;
--         END ADDRHILO_PG;

--     BEGIN
--     ---------------------------------------------------------------------------
--     --Power Up time
--     ---------------------------------------------------------------------------

--     PoweredUp <= '1' AFTER tdevice_PU;

-- --     LongT: PROCESS
-- --     BEGIN
-- --         IF LongTimming = TRUE THEN
-- --             LongCheck := 1;
-- --          ELSE
-- --             LongCheck := 100;
-- --          END IF;
-- --     END PROCESS;


--     TimingModelSel: PROCESS
--     BEGIN
--         --Enhanced High Performance Flag
--         IF (TimingModel(15) = '0' OR TimingModel(15) = '2' OR
--             TimingModel(15) = '3' OR TimingModel(15) = 'R' OR
--             TimingModel(15) = 'A' OR TimingModel(15) = 'B' OR
--             TimingModel(15) = 'C' OR TimingModel(15) = 'D' OR
--             TimingModel(15) = 'Y' OR TimingModel(15) = 'Z' OR
--             TimingModel(15) = 'S' OR TimingModel(15) = 'T' OR
--             TimingModel(15) = 'K' OR TimingModel(15) = 'L') THEN
--             EHP := TRUE;
--             IF (TimingModel(15) = 'Y' OR TimingModel(15) = 'Z' OR
--                 TimingModel(15) = 'S' OR TimingModel(15) = 'T' OR
--                 TimingModel(15) = 'K' OR TimingModel(15) = 'L')   THEN
--                 RdPswdProtEnable <= '1';
--             END IF;
--         ELSIF (TimingModel(15) = '4' OR TimingModel(15) = '6' OR
--                TimingModel(15) = '7' OR TimingModel(15) = '8' OR
--                TimingModel(15) = '9' OR TimingModel(15) = 'Q') THEN
--             EHP := FALSE;
--         END IF;

--         IF (TimingModel(15) ='0' OR TimingModel(15) ='2' OR
--             TimingModel(15) ='3' OR TimingModel(15) ='R' OR
--             TimingModel(15) ='A' OR TimingModel(15) ='B' OR
--             TimingModel(15) ='C' OR TimingModel(15) ='D' OR
--             TimingModel(15) ='4' OR TimingModel(15) ='6' OR
--             TimingModel(15) ='7' OR TimingModel(15) ='8' OR
--             TimingModel(15) ='9' OR TimingModel(15) ='Q') THEN
--             ASP_INIT   <= 1;
--         ELSIF (TimingModel(15) ='Y' OR TimingModel(15) ='Z' OR
--                TimingModel(15) ='S' OR TimingModel(15) ='T' OR
--                TimingModel(15) ='K' OR TimingModel(15) ='L') THEN
--             ASP_INIT   <= 0;
--         END IF;
--         WAIT;
--     END PROCESS;

--     RSTtiming: PROCESS(RSTNeg_pullup,Instruct)
--     BEGIN
--         IF falling_edge(RSTNeg_pullup) THEN
--             RST <= '1', '0' AFTER 200 ns;
--         ELSIF Instruct = RESET THEN
--             Reseted <= '0', '1' AFTER 10 ns;
--         END IF;
--     END PROCESS;

--     DUMMYcnt: PROCESS(dummy_cnt_act_temp)
--     BEGIN
--         dummy_cnt_act <= dummy_cnt_act_temp;
--     END PROCESS;

--     ReadPasswordProtectionMode: PROCESS(PPB_LOCK_temp,
--     ASP_reg_in(2), ASP_reg_in(5))
--     BEGIN
--         IF (PPB_LOCK = '0' AND PWDMLB = '0' AND RPME = '0' AND
--             RdPswdProtEnable = '1') THEN
--             RdPswdProtMode <= '1';
--             ABE := '0';
--         ELSE
--             RdPswdProtMode <= '0';
--         END IF;
--     END PROCESS;
--     ---------------------------------------------------------------------------
--     -- autoboot control logic
--     ---------------------------------------------------------------------------
--     AutoBootControl: PROCESS(SCK_ipd, current_state)
--     BEGIN
--         IF (current_state = AUTOBOOT) THEN
--             IF rising_edge(SCK_ipd) THEN
--                 IF (start_delay > 0) THEN
--                     start_delay := start_delay - 1;
--                 END IF;
--             END IF;

--             IF (start_delay = 0) THEN
--                 start_autoboot <= '1';
--             ELSE
--                 start_autoboot <= '0';
--             END IF;
--         END IF;
--     END PROCESS;

--     ---------------------------------------------------------------------------
--     -- VITAL Timing Checks Procedures
--     ---------------------------------------------------------------------------
--     VITALTimingCheck: PROCESS(SIIn, SOIn, SCK_ipd, CSNeg_ipd, RSTNeg_ipd,
--                               HOLDNegIn, WPNegIn)

--         -- Timing Check Variables
--         -- Setup/Hold Checks variables
--         VARIABLE Tviol_CSNeg_SCK_normal  : X01 := '0';
--         VARIABLE TD_CSNeg_SCK_normal     : VitalTimingDataType;

--         VARIABLE Tviol_CSNeg_SCK_DDR     : X01 := '0';
--         VARIABLE TD_CSNeg_SCK_DDR        : VitalTimingDataType;

--         VARIABLE Tviol_CSNeg_RSTNeg      : X01 := '0';
--         VARIABLE TD_CSNeg_RSTNeg         : VitalTimingDataType;

--         VARIABLE Tviol_SI_SCK            : X01 := '0';
--         VARIABLE TD_SI_SCK               : VitalTimingDataType;

--         VARIABLE Tviol_WPNeg_CSNeg_setup : X01 := '0';
--         VARIABLE TD_WPNeg_CSNeg_setup    : VitalTimingDataType;

--         VARIABLE Tviol_WPNeg_CSNeg_hold  : X01 := '0';
--         VARIABLE TD_WPNeg_CSNeg_hold     : VitalTimingDataType;

--         VARIABLE Tviol_HOLDNeg_SCK       : X01 := '0';
--         VARIABLE TD_HOLDNeg_SCK          : VitalTimingDataType;

--         VARIABLE Tviol_SI_SCK_DDR_R      : X01 := '0';
--         VARIABLE TD_SI_SCK_DDR_R         : VitalTimingDataType;

--         VARIABLE Tviol_SI_SCK_DDR_F      : X01 := '0';
--         VARIABLE TD_SI_SCK_DDR_F         : VitalTimingDataType;

--         VARIABLE Tviol_RSTNeg_CSNeg      : X01 := '0';
--         VARIABLE TD_RSTNeg_CSNeg         : VitalTimingDataType;

--         --Pulse Width and Period Check Variables
--         VARIABLE Pviol_SCK_serial  : X01 := '0';
--         VARIABLE PD_SCK_serial     : VitalPeriodDataType:= VitalPeriodDataInit;

--         VARIABLE Pviol_SCK_dual    : X01 := '0';
--         VARIABLE PD_SCK_dual       : VitalPeriodDataType:= VitalPeriodDataInit;

--         VARIABLE Pviol_SCK_DDR80   : X01 := '0';
--         VARIABLE PD_SCK_DDR80      : VitalPeriodDataType:= VitalPeriodDataInit;

--         VARIABLE Pviol_SCK_fast    : X01 := '0';
--         VARIABLE PD_SCK_fast       : VitalPeriodDataType:= VitalPeriodDataInit;

--         VARIABLE Pviol_SCK_quadpg  : X01 := '0';
--         VARIABLE PD_SCK_quadpg     : VitalPeriodDataType:= VitalPeriodDataInit;

--         VARIABLE Pviol_SCK_DDR     : X01 := '0';
--         VARIABLE PD_SCK_DDR        : VitalPeriodDataType:= VitalPeriodDataInit;

--         VARIABLE Pviol_CSNeg_read  : X01 := '0';
--         VARIABLE PD_CSNeg_read     : VitalPeriodDataType:= VitalPeriodDataInit;

--         VARIABLE Pviol_CSNeg_pgers : X01 := '0';
--         VARIABLE PD_CSNeg_pgers    : VitalPeriodDataType:= VitalPeriodDataInit;

--         VARIABLE Pviol_RSTNeg      : X01 := '0';
--         VARIABLE PD_RSTNeg         : VitalPeriodDataType:= VitalPeriodDataInit;

--         VARIABLE Pviol_SCK_serial_rd : X01 := '0';
--         VARIABLE PD_SCK_serial_rd  : VitalPeriodDataType:= VitalPeriodDataInit;

--         VARIABLE Pviol_SCK_fast_rd : X01 := '0';
--         VARIABLE PD_SCK_fast_rd    : VitalPeriodDataType:=VitalPeriodDataInit;

--         VARIABLE Pviol_SCK_dual_rd : X01 := '0';
--         VARIABLE PD_SCK_dual_rd    : VitalPeriodDataType:=VitalPeriodDataInit;

--         VARIABLE Pviol_SCK_DDR_rd  : X01 := '0';
--         VARIABLE PD_SCK_DDR_rd     : VitalPeriodDataType:= VitalPeriodDataInit;

--         VARIABLE Pviol_SCK_DDR80_rd  : X01 := '0';
--         VARIABLE PD_SCK_DDR80_rd     : VitalPeriodDataType:= VitalPeriodDataInit;

--         VARIABLE Pviol_SCK_quad_pg : X01 := '0';
--         VARIABLE PD_SCK_quad_pg    : VitalPeriodDataType:= VitalPeriodDataInit;

--         VARIABLE Violation         : X01 := '0';

--     BEGIN
--     ---------------------------------------------------------------------------
--     -- Timing Check Section
--     ---------------------------------------------------------------------------
--         IF (TimingChecksOn) THEN

--         -- Setup/Hold Check between CS# and SCK
--         VitalSetupHoldCheck (
--             TestSignal      => CSNeg_ipd,
--             TestSignalName  => "CS#",
--             RefSignal       => SCK_ipd,
--             RefSignalName   => "SCK",
--             SetupHigh       => tsetup_CSNeg_SCK_normal_noedge_posedge,
--             SetupLow        => tsetup_CSNeg_SCK_normal_noedge_posedge,
--             HoldHigh        => thold_CSNeg_SCK_normal_noedge_posedge,
--             HoldLow         => thold_CSNeg_SCK_normal_noedge_posedge,
--             CheckEnabled    => ddr = false,
--             RefTransition   => '/',
--             HeaderMsg       => InstancePath & PartID,
--             TimingData      => TD_CSNeg_SCK_normal,
--             Violation       => Tviol_CSNeg_SCK_normal
--         );

--         -- Setup/Hold Check between CS# and SCK
--         VitalSetupHoldCheck (
--             TestSignal      => CSNeg_ipd,
--             TestSignalName  => "CS#",
--             RefSignal       => SCK_ipd,
--             RefSignalName   => "SCK",
--             SetupHigh       => tsetup_CSNeg_SCK_DDR_noedge_posedge,
--             SetupLow        => tsetup_CSNeg_SCK_DDR_noedge_posedge,
--             HoldHigh        => thold_CSNeg_SCK_DDR_noedge_posedge,
--             HoldLow         => thold_CSNeg_SCK_DDR_noedge_posedge,
--             CheckEnabled    => ddr,
--             RefTransition   => '/',
--             HeaderMsg       => InstancePath & PartID,
--             TimingData      => TD_CSNeg_SCK_DDR,
--             Violation       => Tviol_CSNeg_SCK_DDR
--         );

--         -- Hold Check between CSNeg and RSTNeg
--         VitalSetupHoldCheck (
--             TestSignal      => CSNeg,
--             TestSignalName  => "CSNeg",
--             RefSignal       => RSTNeg,
--             RefSignalName   => "RSTNeg",
--             HoldHigh        => thold_CSNeg_RSTNeg,
--             CheckEnabled    => TRUE,
--             RefTransition   => '\',
--             HeaderMsg       => InstancePath & PartID,
--             TimingData      => TD_CSNeg_RSTNeg,
--             Violation       => Tviol_CSNeg_RSTNeg
--         );

--         -- Setup/Hold Check between SI and SCK, serial mode
--         VitalSetupHoldCheck (
--             TestSignal      => SIIn,
--             TestSignalName  => "SI",
--             RefSignal       => SCK_ipd,
--             RefSignalName   => "SCK",
--             SetupHigh       => tsetup_SI_SCK_normal_noedge_posedge,
--             SetupLow        => tsetup_SI_SCK_normal_noedge_posedge,
--             HoldHigh        => thold_SI_SCK_normal_noedge_posedge,
--             HoldLow         => thold_SI_SCK_normal_noedge_posedge,
--             CheckEnabled    => NOT(DOUBLE) AND SIOut_z /= SIIn ,
--             RefTransition   => '/',
--             HeaderMsg       => InstancePath & PartID,
--             TimingData      => TD_SI_SCK,
--             Violation       => Tviol_SI_SCK
--         );

--         -- Setup Check between WP# and CS# \
--         VitalSetupHoldCheck (
--             TestSignal      => WPNegIn,
--             TestSignalName  => "WP#",
--             RefSignal       => CSNeg_ipd,
--             RefSignalName   => "CS#",
--             SetupHigh       => tsetup_WPNeg_CSNeg,
--             CheckEnabled    => true,
--             RefTransition   => '\',
--             HeaderMsg       => InstancePath & PartID,
--             TimingData      => TD_WPNeg_CSNeg_setup,
--             Violation       => Tviol_WPNeg_CSNeg_setup
--         );

--         -- Hold Check between WP# and CS# /
--         VitalSetupHoldCheck (
--             TestSignal      => WPNegIn,
--             TestSignalName  => "WP#",
--             RefSignal       => CSNeg_ipd,
--             RefSignalName   => "CS#",
--             HoldHigh        => thold_WPNeg_CSNeg,
--             CheckEnabled    => SRWD = '1' AND WEL = '1',
--             RefTransition   => '/',
--             HeaderMsg       => InstancePath & PartID,
--             TimingData      => TD_WPNeg_CSNeg_hold,
--             Violation       => Tviol_WPNeg_CSNeg_hold
--         );

--         -- Setup/Hold Check between HOLD# and SCK /
--         VitalSetupHoldCheck (
--             TestSignal      => HOLDNegIn,
--             TestSignalName  => "HOLD#",
--             RefSignal       => SCK_ipd,
--             RefSignalName   => "SCK",
--             SetupLow        => tsetup_HOLDNeg_SCK,
--             SetupHigh       => tsetup_HOLDNeg_SCK,
--             HoldLow         => thold_HOLDNeg_SCK,
--             HoldHigh        => thold_HOLDNeg_SCK,
--             CheckEnabled    => QUAD = '0'
--                                AND HOLDNegOut_zd /= HOLDNegIn,
--             RefTransition   => '/',
--             HeaderMsg       => InstancePath & PartID,
--             TimingData      => TD_HOLDNeg_SCK,
--             Violation       => Tviol_HOLDNeg_SCK
--         );

--         -- Setup/Hold Check between SI and SCK, DDR mode
--         VitalSetupHoldCheck (
--             TestSignal      => SIIn,
--             TestSignalName  => "SI",
--             RefSignal       => SCK_ipd,
--             RefSignalName   => "SCK",
--             SetupHigh       => tsetup_SI_SCK_DDR_noedge_posedge,
--             SetupLow        => tsetup_SI_SCK_DDR_noedge_posedge,
--             HoldHigh        => thold_SI_SCK_DDR_noedge_posedge,
--             HoldLow         => thold_SI_SCK_DDR_noedge_posedge,
--             CheckEnabled    => DOUBLE AND dual = false AND
--                                ddr80 = false AND SIOut_z /= SIIn,
--             RefTransition   => '/',
--             HeaderMsg       => InstancePath & PartID,
--             TimingData      => TD_SI_SCK_DDR_R,
--             Violation       => Tviol_SI_SCK_DDR_R
--         );

--         -- Setup/Hold Check between SI and SCK, DDR mode
--         VitalSetupHoldCheck (
--             TestSignal      => SIIn,
--             TestSignalName  => "SI",
--             RefSignal       => SCK_ipd,
--             RefSignalName   => "SCK",
--             SetupHigh       => tsetup_SI_SCK_DDR80_noedge_posedge,
--             SetupLow        => tsetup_SI_SCK_DDR80_noedge_posedge,
--             HoldHigh        => thold_SI_SCK_DDR80_noedge_posedge,
--             HoldLow         => thold_SI_SCK_DDR80_noedge_posedge,
--             CheckEnabled    => DOUBLE AND dual = false AND
--                                ddr80 = true AND SIOut_z /= SIIn,
--             RefTransition   => '/',
--             HeaderMsg       => InstancePath & PartID,
--             TimingData      => TD_SI_SCK_DDR_R,
--             Violation       => Tviol_SI_SCK_DDR_R
--         );

--         -- Setup/Hold Check between SI and SCK, DDR mode
--         VitalSetupHoldCheck (
--             TestSignal      => SIIn,
--             TestSignalName  => "SI",
--             RefSignal       => SCK_ipd,
--             RefSignalName   => "SCK",
--             SetupHigh       => tsetup_SI_SCK_DDR_noedge_negedge,
--             SetupLow        => tsetup_SI_SCK_DDR_noedge_negedge,
--             HoldHigh        => thold_SI_SCK_DDR_noedge_negedge,
--             HoldLow         => thold_SI_SCK_DDR_noedge_negedge,
--             CheckEnabled    => ddr AND dual = false
--                                AND SIOut_z /= SIIn ,
--             RefTransition   => '\',
--             HeaderMsg       => InstancePath & PartID,
--             TimingData      => TD_SI_SCK_DDR_F,
--             Violation       => Tviol_SI_SCK_DDR_F
--         );


--         -- Setup/Hold Check between SI and SCK, DDR mode
--         VitalSetupHoldCheck (
--             TestSignal      => SIIn,
--             TestSignalName  => "SI",
--             RefSignal       => SCK_ipd,
--             RefSignalName   => "SCK",
--             SetupHigh       => tsetup_SI_SCK_DDR80_noedge_negedge,
--             SetupLow        => tsetup_SI_SCK_DDR80_noedge_negedge,
--             HoldHigh        => thold_SI_SCK_DDR80_noedge_negedge,
--             HoldLow         => thold_SI_SCK_DDR80_noedge_negedge,
--             CheckEnabled    => ddr AND dual = false AND
--                                ddr80 = false AND SIOut_z /= SIIn ,
--             RefTransition   => '\',
--             HeaderMsg       => InstancePath & PartID,
--             TimingData      => TD_SI_SCK_DDR_F,
--             Violation       => Tviol_SI_SCK_DDR_F
--         );

--         -- Setup Check between RSTNeg and SCK, DDR fast mode
--         VitalSetupHoldCheck (
--             TestSignal      => RSTNeg,
--             TestSignalName  => "RSTNeg",
--             RefSignal       => CSNeg,
--             RefSignalName   => "CSNeg",
--             SetupHigh       => tsetup_RSTNeg_CSNeg,
--             CheckEnabled    => TRUE,
--             RefTransition   => '\',
--             HeaderMsg       => InstancePath & PartID,
--             TimingData      => TD_RSTNeg_CSNeg,
--             Violation       => Tviol_RSTNeg_CSNeg
--         );

--         --Pulse Width and Period Check Variables
--         -- Pulse Width Check SCK for READ, serial mode
--         VitalPeriodPulseCheck (
--             TestSignal      =>  SCK_ipd,
--             TestSignalName  =>  "SCK",
--             PulseWidthLow   =>  tpw_SCK_serial_negedge,
--             PulseWidthHigh  =>  tpw_SCK_serial_posedge,
--             PeriodData      =>  PD_SCK_serial,
--             XOn             =>  XOn,
--             MsgOn           =>  MsgOn,
--             Violation       =>  Pviol_SCK_serial,
--             HeaderMsg       =>  InstancePath & PartID,
--             CheckEnabled    =>  rd);

--         -- Pulse Width Check SCK for DUAL_READ, serial mode
--         VitalPeriodPulseCheck (
--             TestSignal      =>  SCK_ipd,
--             TestSignalName  =>  "SCK",
--             PulseWidthLow   =>  tpw_SCK_dual_negedge,
--             PulseWidthHigh  =>  tpw_SCK_dual_posedge,
--             PeriodData      =>  PD_SCK_dual,
--             XOn             =>  XOn,
--             MsgOn           =>  MsgOn,
--             Violation       =>  Pviol_SCK_dual,
--             HeaderMsg       =>  InstancePath & PartID,
--             CheckEnabled    =>  dual);

--         -- Pulse Width Check SCK for DUAL_READ, serial mode
--         VitalPeriodPulseCheck (
--             TestSignal      =>  SCK_ipd,
--             TestSignalName  =>  "SCK",
--             PulseWidthLow   =>  tpw_SCK_ddr80_negedge,
--             PulseWidthHigh  =>  tpw_SCK_ddr80_posedge,
--             PeriodData      =>  PD_SCK_dual,
--             XOn             =>  XOn,
--             MsgOn           =>  MsgOn,
--             Violation       =>  Pviol_SCK_dual,
--             HeaderMsg       =>  InstancePath & PartID,
--             CheckEnabled    =>  ddr80);

--         -- Pulse Width Check SCK for FAST_READ, serial mode
--         VitalPeriodPulseCheck (
--             TestSignal      =>  SCK_ipd,
--             TestSignalName  =>  "SCK",
--             PulseWidthLow   =>  tpw_SCK_fast_negedge,
--             PulseWidthHigh  =>  tpw_SCK_fast_posedge,
--             PeriodData      =>  PD_SCK_fast,
--             XOn             =>  XOn,
--             MsgOn           =>  MsgOn,
--             Violation       =>  Pviol_SCK_fast,
--             HeaderMsg       =>  InstancePath & PartID,
--             CheckEnabled    =>  fast_rd );

--         -- Pulse Width Check SCK for QPP
--         VitalPeriodPulseCheck (
--             TestSignal      =>  SCK_ipd,
--             TestSignalName  =>  "SCK",
--             PulseWidthLow   =>  tpw_SCK_quadpg_negedge,
--             PulseWidthHigh  =>  tpw_SCK_quadpg_posedge,
--             PeriodData      =>  PD_SCK_quadpg,
--             XOn             =>  XOn,
--             MsgOn           =>  MsgOn,
--             Violation       =>  Pviol_SCK_quadpg,
--             HeaderMsg       =>  InstancePath & PartID,
--             CheckEnabled    =>  quadpg);

--         -- Pulse Width Check CS# for READ, serial mode
--         VitalPeriodPulseCheck (
--             TestSignal      =>  CSNeg_ipd,
--             TestSignalName  =>  "CS#",
--             PulseWidthHigh  =>  tpw_CSNeg_read_posedge,
--             PeriodData      =>  PD_CSNeg_read,
--             XOn             =>  XOn,
--             MsgOn           =>  MsgOn,
--             Violation       =>  Pviol_CSNeg_read,
--             HeaderMsg       =>  InstancePath & PartID,
--             CheckEnabled    =>  any_read );

--         -- Pulse Width Check CS# for Program/Erase, serial mode
--         VitalPeriodPulseCheck (
--             TestSignal      =>  CSNeg_ipd,
--             TestSignalName  =>  "CS#",
--             PulseWidthHigh  =>  tpw_CSNeg_pgers_posedge,
--             PeriodData      =>  PD_CSNeg_pgers,
--             XOn             =>  XOn,
--             MsgOn           =>  MsgOn,
--             Violation       =>  Pviol_CSNeg_pgers,
--             HeaderMsg       =>  InstancePath & PartID,
--             CheckEnabled    =>  NOT(any_read));

--         -- Pulse Width Check RSTNeg
--         VitalPeriodPulseCheck (
--             TestSignal        => RSTNeg_ipd,
--             TestSignalName    => "RSTNeg",
--             PulseWidthLow     => tpw_RSTNeg_negedge,
--             PulseWidthHigh    => tpw_RSTNeg_posedge,
--             CheckEnabled      => TRUE,
--             HeaderMsg         => InstancePath & PartID,
--             PeriodData        => PD_RSTNeg,
--             Violation         => Pviol_RSTNeg
--             );

--         -- Pulse Width Check SCK for DDR READ
--         VitalPeriodPulseCheck (
--             TestSignal      =>  SCK_ipd,
--             TestSignalName  =>  "SCK",
--             PulseWidthLow   =>  tpw_SCK_DDR_negedge,
--             PulseWidthHigh  =>  tpw_SCK_DDR_posedge,
--             PeriodData      =>  PD_SCK_DDR,
--             XOn             =>  XOn,
--             MsgOn           =>  MsgOn,
--             Violation       =>  Pviol_SCK_DDR,
--             HeaderMsg       =>  InstancePath & PartID,
--             CheckEnabled    =>  ddr);

--         -- Pulse Width Check SCK for DDR READ
--         VitalPeriodPulseCheck (
--             TestSignal      =>  SCK_ipd,
--             TestSignalName  =>  "SCK",
--             PulseWidthLow   =>  tpw_SCK_DDR80_negedge,
--             PulseWidthHigh  =>  tpw_SCK_DDR80_posedge,
--             PeriodData      =>  PD_SCK_DDR80,
--             XOn             =>  XOn,
--             MsgOn           =>  MsgOn,
--             Violation       =>  Pviol_SCK_DDR80,
--             HeaderMsg       =>  InstancePath & PartID,
--             CheckEnabled    =>  ddr80);

--         -- Period Check SCK for READ, serial mode
--         VitalPeriodPulseCheck (
--             TestSignal      =>  SCK_ipd,
--             TestSignalName  =>  "SCK",
--             Period          =>  tperiod_SCK_serial_rd,
--             PeriodData      =>  PD_SCK_serial_rd,
--             XOn             =>  XOn,
--             MsgOn           =>  MsgOn,
--             Violation       =>  Pviol_SCK_serial_rd,
--             HeaderMsg       =>  InstancePath & PartID,
--             CheckEnabled    =>  rd );

--         -- Period Check SCK for FAST READ, serial mode
--         VitalPeriodPulseCheck (
--             TestSignal      =>  SCK_ipd,
--             TestSignalName  =>  "SCK",
--             Period          =>  tperiod_SCK_fast_rd,
--             PeriodData      =>  PD_SCK_fast_rd,
--             XOn             =>  XOn,
--             MsgOn           =>  MsgOn,
--             Violation       =>  Pviol_SCK_fast_rd,
--             HeaderMsg       =>  InstancePath & PartID,
--             CheckEnabled    =>  fast_rd );

--         -- Period Check SCK for DUAL READ, serial mode
--         VitalPeriodPulseCheck (
--             TestSignal      =>  SCK_ipd,
--             TestSignalName  =>  "SCK",
--             Period          =>  tperiod_SCK_dual_rd,
--             PeriodData      =>  PD_SCK_dual_rd,
--             XOn             =>  XOn,
--             MsgOn           =>  MsgOn,
--             Violation       =>  Pviol_SCK_dual_rd,
--             HeaderMsg       =>  InstancePath & PartID,
--             CheckEnabled    =>  dual );

--         -- Period Check SCK for QPP
--         VitalPeriodPulseCheck (
--             TestSignal      =>  SCK_ipd,
--             TestSignalName  =>  "SCK",
--             Period          =>  tperiod_SCK_quadpg,
--             PeriodData      =>  PD_SCK_quad_pg,
--             XOn             =>  XOn,
--             MsgOn           =>  MsgOn,
--             Violation       =>  Pviol_SCK_quad_pg,
--             HeaderMsg       =>  InstancePath & PartID,
--             CheckEnabled    =>  quadpg );

--         -- Period Check SCK for DDR READ
--         VitalPeriodPulseCheck (
--             TestSignal      =>  SCK_ipd,
--             TestSignalName  =>  "SCK",
--             Period          =>  tperiod_SCK_DDR_rd,
--             PeriodData      =>  PD_SCK_DDR_rd,
--             XOn             =>  XOn,
--             MsgOn           =>  MsgOn,
--             Violation       =>  Pviol_SCK_DDR_rd,
--             HeaderMsg       =>  InstancePath & PartID,
--             CheckEnabled    =>  ddr );

--         -- Period Check SCK for DDR READ
--         VitalPeriodPulseCheck (
--             TestSignal      =>  SCK_ipd,
--             TestSignalName  =>  "SCK",
--             Period          =>  tperiod_SCK_DDR80_rd,
--             PeriodData      =>  PD_SCK_DDR80_rd,
--             XOn             =>  XOn,
--             MsgOn           =>  MsgOn,
--             Violation       =>  Pviol_SCK_DDR80_rd,
--             HeaderMsg       =>  InstancePath & PartID,
--             CheckEnabled    =>  ddr80 );

--         Violation :=   Tviol_CSNeg_SCK_normal OR
--                        Tviol_CSNeg_SCK_DDR OR
--                        Tviol_CSNeg_RSTNeg OR
--                        Tviol_SI_SCK OR
--                        Tviol_WPNeg_CSNeg_setup OR
--                        Tviol_WPNeg_CSNeg_hold OR
--                        Tviol_HOLDNeg_SCK OR
--                        Tviol_SI_SCK_DDR_R OR
--                        Tviol_SI_SCK_DDR_F OR
--                        Tviol_RSTNeg_CSNeg OR
--                        Pviol_SCK_serial OR
--                        Pviol_SCK_dual OR
--                        Pviol_SCK_fast OR
--                        Pviol_SCK_quadpg OR
--                        Pviol_CSNeg_read OR
--                        Pviol_CSNeg_pgers OR
--                        Pviol_SCK_DDR OR
--                        Pviol_SCK_DDR80 OR
--                        Pviol_SCK_serial_rd OR
--                        Pviol_SCK_fast_rd OR
--                        Pviol_SCK_dual_rd OR
--                        Pviol_SCK_quad_pg OR
--                        Pviol_SCK_DDR_rd OR
--                        Pviol_SCK_DDR80_rd;

--             Viol <= Violation;

--             ASSERT Violation = '0'
--                 REPORT InstancePath & partID & ": simulation may be" &
--                     " inaccurate due to timing violations"
--                 SEVERITY WARNING;

--         END IF;
--     END PROCESS VITALTimingCheck;

--     ----------------------------------------------------------------------------
--     -- sequential process for FSM state transition
--     ----------------------------------------------------------------------------
--     StateTransition : PROCESS(next_state, RST, PoweredUp, RST_out)

--     BEGIN
--         IF PoweredUp = '1' THEN
--             IF RSTNeg_pullup = '1' and RST_out= '1' THEN
--                 IF next_state'EVENT THEN
--                     current_state <= next_state;
--                 END IF;
--             ELSIF RSTNeg_pullup = '0' AND falling_edge(RST) THEN
--                 --no state transition while RESET# low
--                 current_state <= RESET_STATE;
--                 RST_in <= '1', '0' AFTER 1 ns;
--             END IF;
--         END IF;
--     END PROCESS StateTransition;

--     Threset : PROCESS(RST_in)
--     BEGIN
--         IF rising_edge(RST_in) THEN
--             RST_out <= '0', '1' AFTER HOLD_CSNeg_RSTNeg;
--         END IF;
--     END PROCESS Threset;

--     ---------------------------------------------------------------------------
--     --  Write cycle decode
--     ---------------------------------------------------------------------------
--     BusCycleDecode : PROCESS(SCK_ipd, CSNeg_ipd, HOLDNeg_pullup, SIIn, RST_out,
--                              WPNeg_pullup, current_state)

--         TYPE quad_data_type IS ARRAY (0 TO 1023) OF INTEGER RANGE 0 TO 15;

--         VARIABLE bit_cnt            : NATURAL := 0;
--         VARIABLE Data_in            : std_logic_vector(4095 downto 0)
--                                                     := (others => '0');

--         VARIABLE opcode             : std_logic_vector(7 downto 0);
--         VARIABLE opcode_in          : std_logic_vector(7 downto 0);
--         VARIABLE addr_bytes         : std_logic_vector(31 downto 0);
--         VARIABLE hiaddr_bytes       : std_logic_vector(31 downto 0);
--         VARIABLE Address_in         : std_logic_vector(31 downto 0);
--         VARIABLE mode_bytes         : std_logic_vector(7 downto 0);
--         VARIABLE mode_in            : std_logic_vector(7 downto 0);
--         VARIABLE quad_data_in       : quad_data_type;
--         VARIABLE quad_nybble        : std_logic_vector(3 downto 0);
--         VARIABLE Quad_slv           : std_logic_vector(3 downto 0);
--         VARIABLE Byte_slv           : std_logic_vector(7 downto 0);

--         VARIABLE CLK_PER            : time;
--         VARIABLE LAST_CLK           : time;
--         VARIABLE Check_freq         : boolean := FALSE;

--     BEGIN

--         IF (rising_edge(CSNeg_ipd) AND NOT(bus_cycle_state = DATA_BYTES))
--         OR current_state = RESET_STATE THEN
--             bus_cycle_state := STAND_BY;
--         ELSE
--             CASE bus_cycle_state IS
--                 WHEN STAND_BY =>
--                     IF falling_edge(CSNeg_ipd) THEN
--                         Instruct  <= NONE;
--                         write     <= '1';
--                         cfg_write <= '0';
--                         opcode_cnt:= 0;
--                         addr_cnt  := 0;
--                         mode_cnt  := 0;
--                         dummy_cnt := 0;
--                         dummy_cnt_act_temp <= dummy_cnt;
--                         data_cnt  := 0;
--                         DOUBLE    := FALSE;
--                         CLK_PER   := 0 ns;
--                         LAST_CLK  := 0 ns;
--                         IF current_state = AUTOBOOT THEN
--                             bus_cycle_state := DATA_BYTES;
--                         ELSE
--                             bus_cycle_state := OPCODE_BYTE;
--                         END IF;
--                     END IF;

--                 WHEN OPCODE_BYTE =>
--                     IF rising_edge(SCK_ipd)  THEN

--                         Latency_code := to_nat(LC1 & LC0);
--                         CLK_PER  := NOW - LAST_CLK;
--                         LAST_CLK := NOW;
--                         IF Check_freq THEN
--                             IF (CLK_PER < 20 ns AND Latency_code = 3)  OR
--                                (CLK_PER < 12.5 ns AND Latency_code = 0) OR
--                                (CLK_PER < 11.1 ns AND Latency_code = 1) OR
--                                (CLK_PER < 9.6 ns AND Latency_code = 2) THEN
--                                 ASSERT FALSE
--                                 REPORT "More wait states are required for " &
--                                        "this clock frequency value"
--                                 SEVERITY warning;
--                                 IF (Instruct = DDRFR   OR Instruct = DDRFR4 OR
--                                     Instruct = DDRDIOR OR Instruct = DDRDIOR4 OR
--                                     Instruct = DDRQIOR OR Instruct = DDRQIOR4)  THEN
--                                     IF (CLK_PER < 12.5 ns) THEN
--                                         ddr80 <= TRUE;
--                                     ELSE
--                                         ddr80 <= FALSE;
--                                     END IF;
--                                 END IF;
--                             END IF;
--                         END IF;
--                         Check_freq := FALSE;

--                         IF ((HOLDNeg_pullup='1' AND QUAD='0') OR QUAD='1') THEN
--                         --One-byte of instruction opcode is shifted into the
--                         --device on the SI serial input pin with the most
--                         --significant bit (MSB) first.Each bit input on the
--                         --SI serial input pin is latched on the rising edge of
--                         --the SCK serial clock signal.
--                             opcode_in(opcode_cnt) := SIIn;
--                             opcode_cnt := opcode_cnt + 1;
--                             IF opcode_cnt = BYTE THEN
--                                 --MSB first
--                                 FOR I IN 7 DOWNTO 0 LOOP
--                                     opcode(i) := opcode_in(7-i);
--                                 END LOOP;

--                                 CASE opcode IS
--                                     WHEN "00000110"  => --06h
--                                         Instruct <= WREN;
--                                         bus_cycle_state := DATA_BYTES;
--                                     WHEN "00000100"  => --04h
--                                         Instruct <= WRDI;
--                                         bus_cycle_state := DATA_BYTES;
--                                     WHEN "00000001"  => --01h
--                                         Instruct <= WRR;
--                                         bus_cycle_state := DATA_BYTES;
--                                     WHEN "00000011"  => --03h
--                                         Instruct <= READ;
--                                         bus_cycle_state := ADDRESS_BYTES;
--                                     WHEN "00010011"  => --13h
--                                         Instruct <= RD4;
--                                         bus_cycle_state := ADDRESS_BYTES;
--                                     WHEN "01001011"  => --4Bh
--                                         Instruct <= OTPR;
--                                         bus_cycle_state := ADDRESS_BYTES;
--                                     WHEN "00000101"  => --05h
--                                         Instruct <= RDSR;
--                                         bus_cycle_state := DATA_BYTES;
--                                     WHEN "00000111"  => --07h
--                                         Instruct <= RDSR2;
--                                         bus_cycle_state := DATA_BYTES;
--                                     WHEN "00110101"  => --35h
--                                         Instruct <= RDCR;
--                                         bus_cycle_state := DATA_BYTES;
--                                     WHEN "10010000"  => --90h
--                                         Instruct <= REMS;
--                                         bus_cycle_state := ADDRESS_BYTES;
--                                     WHEN "10011111"  => --9Fh
--                                         Instruct <= RDID;
--                                         bus_cycle_state := DATA_BYTES;
--                                     WHEN "10101011"  => --ABh
--                                         Instruct <= RES;
--                                         bus_cycle_state := DUMMY_BYTES;
--                                     WHEN "00001011"  => --0Bh
--                                         Instruct <= FSTRD;
--                                         bus_cycle_state := ADDRESS_BYTES;
--                                         Check_freq := TRUE;
--                                     WHEN "00001100"  => --0Ch
--                                         Instruct <= FSTRD4;
--                                         bus_cycle_state := ADDRESS_BYTES;
--                                         Check_freq := TRUE;
--                                     WHEN "00001101"  => --0Dh
--                                         Instruct <= DDRFR;
--                                         bus_cycle_state := ADDRESS_BYTES;
--                                         Check_freq := TRUE;
--                                     WHEN "00001110"  => --0Eh
--                                         Instruct <= DDRFR4;
--                                         bus_cycle_state := ADDRESS_BYTES;
--                                         Check_freq := TRUE;
--                                     WHEN "00111011"  => --3Bh
--                                         Instruct <= DOR;
--                                         Check_freq := TRUE;
--                                         bus_cycle_state := ADDRESS_BYTES;
--                                     WHEN "00111100"  => --3Ch
--                                         Instruct <= DOR4;
--                                         bus_cycle_state := ADDRESS_BYTES;
--                                         Check_freq := TRUE;
--                                     WHEN "10111011"  => --BBh
--                                         Instruct <= DIOR;
--                                         bus_cycle_state := ADDRESS_BYTES;
--                                         Check_freq := TRUE;
--                                     WHEN "10111100"  => --BCh
--                                         Instruct <= DIOR4;
--                                         bus_cycle_state := ADDRESS_BYTES;
--                                         Check_freq := TRUE;
--                                     WHEN "10111101"  => --BDh
--                                         Instruct <= DDRDIOR;
--                                         bus_cycle_state := ADDRESS_BYTES;
--                                         Check_freq := TRUE;
--                                     WHEN "10111110"  => --BEh
--                                         Instruct <= DDRDIOR4;
--                                         bus_cycle_state := ADDRESS_BYTES;
--                                         Check_freq := TRUE;
--                                     WHEN "01101011"  => --6Bh
--                                         Instruct <= QOR;
--                                         bus_cycle_state := ADDRESS_BYTES;
--                                         Check_freq := TRUE;
--                                     WHEN "01101100"  => --6Ch
--                                         Instruct <= QOR4;
--                                         bus_cycle_state := ADDRESS_BYTES;
--                                         Check_freq := TRUE;
--                                     WHEN "11101011"  => --EBh
--                                         Instruct <= QIOR;
--                                         bus_cycle_state := ADDRESS_BYTES;
--                                         Check_freq := TRUE;
--                                     WHEN "11101100"  => --ECh
--                                         Instruct <= QIOR4;
--                                         bus_cycle_state := ADDRESS_BYTES;
--                                         Check_freq := TRUE;
--                                     WHEN "11101101"  => --EDh
--                                         Instruct <= DDRQIOR;
--                                         bus_cycle_state := ADDRESS_BYTES;
--                                         Check_freq := TRUE;
--                                     WHEN "11101110"  => --EEh
--                                         Instruct <= DDRQIOR4;
--                                         bus_cycle_state := ADDRESS_BYTES;
--                                         Check_freq := TRUE;
--                                     WHEN "00000010"  => --02h
--                                         Instruct <= PP;
--                                         bus_cycle_state := ADDRESS_BYTES;
--                                     WHEN "00010010"  => --12h
--                                         Instruct <= PP4;
--                                         bus_cycle_state := ADDRESS_BYTES;
--                                     WHEN "00110010"  => --32h
--                                         Instruct <= QPP;
--                                         bus_cycle_state := ADDRESS_BYTES;
--                                         quadpg   <= TRUE;
--                                     WHEN "00111000"  => --38h
--                                         Instruct <= QPP;
--                                         bus_cycle_state := ADDRESS_BYTES;
--                                         quadpg   <= TRUE;
--                                     WHEN "00110100"  => --34h
--                                         Instruct <= QPP4;
--                                         bus_cycle_state := ADDRESS_BYTES;
--                                         quadpg   <= TRUE;
--                                     WHEN "01000010"  => --42h
--                                         Instruct <= OTPP;
--                                         bus_cycle_state := ADDRESS_BYTES;
--                                     WHEN "10000101"  => --85h
--                                         Instruct <= PGSP;
--                                         bus_cycle_state := DATA_BYTES;
--                                     WHEN "10001010"  => --8Ah
--                                         Instruct <= PGRS;
--                                         bus_cycle_state := DATA_BYTES;
--                                     WHEN "11000111"  => --C7h
--                                         Instruct <= BE;
--                                         bus_cycle_state := DATA_BYTES;
--                                     WHEN "01100000"  => --60h
--                                         Instruct <= BE;
--                                         bus_cycle_state := DATA_BYTES;
--                                     WHEN "11011000"  => --D8h
--                                         Instruct <= SE;
--                                         bus_cycle_state := ADDRESS_BYTES;
--                                     WHEN "11011100"  => --DCh
--                                         Instruct <= SE4;
--                                         bus_cycle_state := ADDRESS_BYTES;
--                                     WHEN "00100000"  => --20h
--                                         Instruct <= P4E;
--                                         bus_cycle_state := ADDRESS_BYTES;
--                                     WHEN "00100001"  => --21h
--                                         Instruct <= P4E4;
--                                         bus_cycle_state := ADDRESS_BYTES;
--                                     WHEN "01110101"  => --75h
--                                         Instruct <= ERSP;
--                                         bus_cycle_state := DATA_BYTES;
--                                     WHEN "01111010"  => --7Ah
--                                         Instruct <= ERRS;
--                                         bus_cycle_state := DATA_BYTES;
--                                     WHEN "00010100"  => --14h
--                                         Instruct <= ABRD;
--                                         bus_cycle_state := DATA_BYTES;
--                                     WHEN "00010101"  => --15h
--                                         Instruct <= ABWR;
--                                         bus_cycle_state := DATA_BYTES;
--                                     WHEN "00010110"  => --16h
--                                         Instruct <= BRRD;
--                                         bus_cycle_state := DATA_BYTES;
--                                     WHEN "00010111"  => --17h
--                                         Instruct <= BRWR;
--                                         bus_cycle_state := DATA_BYTES;
--                                     WHEN "10111001"  => --B9h
--                                         Instruct <= BRAC;
--                                         bus_cycle_state := DATA_BYTES;
--                                     WHEN "00101011"  => --2Bh
--                                         Instruct <= ASPRD;
--                                         bus_cycle_state := DATA_BYTES;
--                                     WHEN "00101111"  => --2Fh
--                                         Instruct <= ASPP;
--                                         bus_cycle_state := DATA_BYTES;
--                                     WHEN "11100000"  => --E0h
--                                         Instruct <= DYBRD;
--                                         bus_cycle_state := ADDRESS_BYTES;
--                                     WHEN "11100001"  => --E1h
--                                         Instruct <= DYBWR;
--                                         bus_cycle_state := ADDRESS_BYTES;
--                                     WHEN "11100010"  => --E2h
--                                         Instruct <= PPBRD;
--                                         bus_cycle_state := ADDRESS_BYTES;
--                                     WHEN "11100011"  => --E3h
--                                         Instruct <= PPBP;
--                                         bus_cycle_state := ADDRESS_BYTES;
--                                     WHEN "00011000"  => --18h
--                                         Instruct <= ECCRD;
--                                         bus_cycle_state := ADDRESS_BYTES;
--                                     WHEN "11100100"  => --E4h
--                                         Instruct <= PPBERS;
--                                         bus_cycle_state := DATA_BYTES;
--                                     WHEN "10100110"  => --A6h
--                                         Instruct <= PLBWR;
--                                         bus_cycle_state := DATA_BYTES;
--                                     WHEN "10100111"  => --A7h
--                                         Instruct <= PLBRD;
--                                         bus_cycle_state := DATA_BYTES;
--                                     WHEN "11100111"  => --E7h
--                                         Instruct <= PASSRD;
--                                         bus_cycle_state := DATA_BYTES;
--                                     WHEN "11101000"  => --E8h
--                                         Instruct <= PASSP;
--                                         bus_cycle_state := DATA_BYTES;
--                                     WHEN "11101001"  => --E9h
--                                         Instruct <= PASSU;
--                                         bus_cycle_state := DATA_BYTES;
--                                     WHEN "11110000"  => --F0h
--                                         Instruct <= RESET;
--                                         bus_cycle_state := DATA_BYTES;
--                                     WHEN "11111111"  => --FFh
--                                         Instruct <= MBR;
--                                         bus_cycle_state := MODE_BYTE;
--                                     WHEN "01000001"  => -- 41h
--                                         Instruct <= DLPRD;
--                                         bus_cycle_state := DATA_BYTES;
--                                     WHEN "01000011"  => -- 43h
--                                         Instruct <= PNVDLR;
--                                         bus_cycle_state := DATA_BYTES;
--                                     WHEN "01001010"  => -- 4Ah
--                                         Instruct <= WVDLR;
--                                         bus_cycle_state := DATA_BYTES;
--                                     WHEN "00110000"  => --30h
--                                         Instruct <= CLSR;
--                                         bus_cycle_state := DATA_BYTES;
--                                     WHEN "01011010"  => --30h
--                                         Instruct <= RSFDP;
--                                         bus_cycle_state := ADDRESS_BYTES;
--                                     WHEN others =>
--                                         null;

--                                 END CASE;
--                             END IF;
--                         END IF;
--                     END IF;

--                 WHEN ADDRESS_BYTES =>
--                     IF Instruct= DDRFR OR Instruct= DDRFR4 OR Instruct= DDRDIOR
--                        OR Instruct = DDRDIOR4 OR Instruct = DDRQIOR OR
--                        Instruct = DDRQIOR4 THEN
--                        DOUBLE := TRUE;
--                     ELSE
--                        DOUBLE := FALSE;
--                     END IF;

--                     IF (rising_edge(SCK_ipd) AND NOT(DOUBLE) AND
--                        (CSNeg_ipd= '0')) THEN
--                         IF (((Instruct=FSTRD AND EXTADD= '0' )
--                          OR  (Instruct=DOR  AND EXTADD= '0')
--                          OR Instruct=OTPR OR Instruct=RSFDP) AND
--                          ((HOLDNeg_pullup='1' AND QUAD='0') OR QUAD='1'))
--                          OR  (Instruct=QOR  AND QUAD= '1' AND EXTADD='0') THEN
--                             --Instruction + 3 Bytes Address + Dummy Byte
--                             Address_in(addr_cnt) := SIIn;
--                             addr_cnt := addr_cnt + 1;
--                             IF addr_cnt = 3*BYTE THEN
--                                 FOR I IN 23 DOWNTO 0 LOOP
--                                     addr_bytes(23-i) := Address_in(i);
--                                 END LOOP;
--                                 addr_bytes(31 downto 26) := "000000";
--                                 addr_bytes(25 downto 24) :=
--                                            Bank_Addr_reg (1 downto 0);
--                                 Address <= to_nat(addr_bytes);
--                                 change_addr <= '1','0' AFTER 1 ns;
--                                 IF (Instruct=FSTRD OR Instruct=DOR OR
--                                     Instruct=QOR) THEN
--                                     IF (Latency_code = 3) THEN
--                                         bus_cycle_state := DATA_BYTES;
--                                     ELSE
--                                         bus_cycle_state := DUMMY_BYTES;
--                                     END IF;
--                                 ELSE
--                                     bus_cycle_state := DUMMY_BYTES;
--                                 END IF;
--                             END IF;
--                         ELSIF (Instruct=ECCRD) THEN
--                             --Instruction + 4 Bytes Address + Dummy Byte
--                             Address_in(addr_cnt) := SIIn;
--                             addr_cnt := addr_cnt + 1;
--                             IF addr_cnt = 4*BYTE THEN
--                                 FOR I IN 31 DOWNTO 0 LOOP
--                                     hiaddr_bytes(31-i) := Address_in(i);
--                                 END LOOP;
--                                 hiaddr_bytes(3 downto 0) := "0000";
--                                 Address<= to_nat(hiaddr_bytes);
--                                 change_addr <= '1','0' AFTER 1 ns;
--                                 bus_cycle_state := DUMMY_BYTES;
--                             END IF;
--                         ELSIF ((((Instruct=FSTRD4) OR (Instruct=DOR4) OR
--                             ((Instruct=FSTRD) AND EXTADD= '1') OR
--                             ((Instruct=DOR) AND EXTADD= '1')) AND
--                             ((HOLDNeg_pullup='1' AND QUAD='0') OR QUAD='1')) OR
--                             ((Instruct=QOR4) AND QUAD='1') OR
--                             ((Instruct=QOR) AND QUAD='1' AND EXTADD='1')) THEN
--                             --Instruction + 4 Bytes Address + Dummy Byte
--                             Address_in(addr_cnt) := SIIn;
--                             addr_cnt := addr_cnt + 1;
--                             IF addr_cnt = 4*BYTE THEN
--                                 FOR I IN 31 DOWNTO 0 LOOP
--                                     hiaddr_bytes(31-i) := Address_in(i);
--                                 END LOOP;
--                                 Address <= to_nat(hiaddr_bytes(25 downto 0));
--                                 change_addr <= '1','0' AFTER 1 ns;
--                                 IF Latency_code = 3 THEN
--                                     bus_cycle_state := DATA_BYTES;
--                                 ELSE
--                                     bus_cycle_state := DUMMY_BYTES;
--                                     IF (DOUBLE AND NOT(hold_mode) AND
--                                         VDLR_reg /= "00000000") THEN
--                                         read_out <= '1', '0' AFTER 1 ns;
--                                     END IF;
--                                 END IF;
--                             END IF;
--                         ELSIF Instruct = DIOR AND EXTADD= '0' AND
--                            ((HOLDNeg_pullup='1' AND QUAD='0') OR QUAD='1') THEN
--                             -- DUAL I/O High Performance Read (3Bytes Address)
--                             IF SOIn /= 'Z' THEN
--                                 Address_in(2*addr_cnt)   := SOIn;
--                                 Address_in(2*addr_cnt+1) := SIIn;
--                                 read_cnt := 0;
--                                 addr_cnt := addr_cnt + 1;
--                                 IF addr_cnt = (3*BYTE) / 2 THEN
--                                     addr_cnt := 0;
--                                     FOR I IN 23 DOWNTO 0 LOOP
--                                         addr_bytes(23-i) := Address_in(i);
--                                     END LOOP;
--                                     addr_bytes(31 downto 26) := "000000";
--                                     addr_bytes(25 downto 24) :=
--                                             Bank_Addr_reg (1 downto 0);
--                                     Address <= to_nat(addr_bytes);
--                                     change_addr <= '1','0' AFTER 1 ns;
--                                     IF EHP THEN
--                                         bus_cycle_state := MODE_BYTE;
--                                     ELSE
--                                         bus_cycle_state := DUMMY_BYTES;
--                                     END IF;
--                                 END IF;
--                             ELSE
--                                 bus_cycle_state := STAND_BY;
--                             END IF;
--                         ELSIF ((Instruct = DIOR4 OR
--                               (Instruct = DIOR AND EXTADD= '1')) AND
--                               ((HOLDNeg_pullup = '1' AND QUAD = '0') OR
--                                 QUAD = '1')) THEN
--                             -- DUAL I/O High Performance Read (4Bytes Address)
--                             IF SOIn /= 'Z' THEN
--                                 Address_in(2*addr_cnt)   := SOIn;
--                                 Address_in(2*addr_cnt+1) := SIIn;
--                                 read_cnt := 0;
--                                 addr_cnt := addr_cnt + 1;
--                                 IF addr_cnt = (4*BYTE) / 2 THEN
--                                     addr_cnt := 0;
--                                     FOR I IN 31 DOWNTO 0 LOOP
--                                         hiaddr_bytes(31-i) := Address_in(i);
--                                     END LOOP;
--                                     Address <= to_nat(hiaddr_bytes(25 downto 0));
--                                     change_addr <= '1','0' AFTER 1 ns;
--                                     IF EHP THEN
--                                         bus_cycle_state := MODE_BYTE;
--                                     ELSE
--                                         bus_cycle_state := DUMMY_BYTES;
--                                     END IF;
--                                 END IF;
--                             ELSE
--                                 bus_cycle_state := STAND_BY;
--                             END IF;
--                         ELSIF (Instruct = QIOR AND EXTADD= '0') THEN
--                             -- QUAD I/O High Performance Read (3Bytes Address)
--                             IF QUAD = '1' THEN
--                                 IF SOIn /= 'Z' THEN
--                                     Address_in(4*addr_cnt)   := HOLDNegIn;
--                                     Address_in(4*addr_cnt+1) := WPNegIn;
--                                     Address_in(4*addr_cnt+2) := SOIn;
--                                     Address_in(4*addr_cnt+3) := SIIn;
--                                     read_cnt := 0;
--                                     addr_cnt := addr_cnt + 1;
--                                     IF addr_cnt = (3*BYTE) / 4 THEN
--                                         addr_cnt := 0;
--                                         FOR I IN 23 DOWNTO 0 LOOP
--                                             addr_bytes(23-i) := Address_in(i);
--                                         END LOOP;
--                                         addr_bytes(31 downto 26) := "000000";
--                                         addr_bytes(25 downto 24) :=
--                                                 Bank_Addr_reg (1 downto 0);
--                                         Address <= to_nat(addr_bytes);
--                                         change_addr <= '1','0' AFTER 1 ns;
--                                         bus_cycle_state := MODE_BYTE;
--                                     END IF;
--                                 END IF;
--                             ELSE
--                                 bus_cycle_state := STAND_BY;
--                             END IF;
--                         ELSIF (Instruct = QIOR4 OR
--                               (Instruct = QIOR AND EXTADD= '1')) THEN
--                             -- QUAD I/O High Performance Read (4Bytes Address)
--                             IF QUAD = '1' THEN
--                                 IF SOIn /= 'Z' THEN
--                                     Address_in(4*addr_cnt)   := HOLDNegIn;
--                                     Address_in(4*addr_cnt+1) := WPNegIn;
--                                     Address_in(4*addr_cnt+2) := SOIn;
--                                     Address_in(4*addr_cnt+3) := SIIn;
--                                     read_cnt := 0;
--                                     addr_cnt := addr_cnt + 1;
--                                     IF addr_cnt = (4*BYTE) / 4 THEN
--                                         addr_cnt := 0;
--                                         FOR I IN 31 DOWNTO 0 LOOP
--                                             hiaddr_bytes(31-i)
--                                                         := Address_in(i);
--                                         END LOOP;
--                                         Address <= to_nat(hiaddr_bytes(25 downto 0));
--                                         change_addr <= '1','0' AFTER 1 ns;
--                                         bus_cycle_state := MODE_BYTE;
--                                     END IF;
--                                 END IF;
--                             ELSE
--                                 bus_cycle_state := STAND_BY;
--                             END IF;
--                         ELSIF (((Instruct = RD4  OR Instruct = PP4 OR
--                               Instruct = SE4 OR Instruct = PPBRD OR
--                               Instruct = DYBRD OR Instruct = DYBWR OR
--                               Instruct = PPBP OR Instruct = P4E4 OR
--                               (Instruct = READ AND EXTADD= '1' ) OR
--                               (Instruct = PP AND EXTADD= '1' ) OR
--                               (Instruct = P4E AND EXTADD= '1' ) OR
--                               (Instruct = SE AND EXTADD= '1' )) AND
--                               ((HOLDNeg_pullup='1' AND QUAD='0') OR QUAD='1'))
--                               OR (QUAD ='1' AND (Instruct=QPP4 OR
--                               (Instruct = QPP AND EXTADD= '1' ))))THEN
--                             Address_in(addr_cnt) := SIIn;
--                             addr_cnt := addr_cnt + 1;
--                             IF addr_cnt = 4*BYTE THEN
--                                 FOR I IN 31 DOWNTO 0 LOOP
--                                     hiaddr_bytes(31-i) := Address_in(i);
--                                 END LOOP;
--                                 Address <= to_nat(hiaddr_bytes(25 downto 0));
--                                 change_addr <= '1','0' AFTER 1 ns;
--                                 bus_cycle_state := DATA_BYTES;
--                             END IF;
--                         ELSIF (((HOLDNeg_pullup='1' AND QUAD='0')
--                                  OR QUAD='1') AND EXTADD= '0') THEN
--                             Address_in(addr_cnt) := SIIn;
--                             addr_cnt := addr_cnt + 1;
--                             IF addr_cnt = 3*BYTE THEN
--                                 FOR I IN 23 DOWNTO 0 LOOP
--                                     addr_bytes(23-i) := Address_in(i);
--                                 END LOOP;
--                                 addr_bytes(31 downto 26) := "000000";
--                                 addr_bytes(25 downto 24) :=
--                                            Bank_Addr_reg (1 downto 0);
--                                 Address <= to_nat(addr_bytes);
--                                 change_addr <= '1','0' AFTER 1 ns;
--                                 bus_cycle_state := DATA_BYTES;
--                             END IF;
--                         END IF;
--                     ELSIF (SCK_ipd'EVENT AND DOUBLE AND addr_cnt /= 0 ) OR
--                        (rising_edge(SCK_ipd) AND DOUBLE AND addr_cnt = 0 ) THEN
--                         IF (Instruct=DDRFR AND EXTADD= '0' ) THEN
--                             --Fast DDR Read Mode
--                             Address_in(addr_cnt) := SIIn;
--                             addr_cnt := addr_cnt + 1;
--                             read_cnt := 0;
--                             IF addr_cnt = 3*BYTE THEN
--                                 addr_cnt := 0;
--                                 FOR I IN 23 DOWNTO 0 LOOP
--                                     addr_bytes(23-i) := Address_in(i);
--                                 END LOOP;
--                                 addr_bytes(31 downto 26) := "000000";
--                                 addr_bytes(25 downto 24) :=
--                                            Bank_Addr_reg (1 downto 0);
--                                 Address <= to_nat(addr_bytes);
--                                 change_addr <= '1','0' AFTER 1 ns;
--                                 IF EHP THEN
--                                     bus_cycle_state := MODE_BYTE;
--                                 ELSE
--                                     bus_cycle_state := DUMMY_BYTES;
--                                 END IF;
--                             END IF;
--                         ELSIF (Instruct=DDRFR4 OR
--                               (Instruct=DDRFR AND EXTADD= '1' )) THEN
--                             Address_in(addr_cnt) := SIIn;
--                             addr_cnt := addr_cnt + 1;
--                             read_cnt := 0;
--                             IF addr_cnt = 4*BYTE THEN
--                                 addr_cnt := 0;
--                                 FOR I IN 31 DOWNTO 0 LOOP
--                                     addr_bytes(31-i) := Address_in(i);
--                                 END LOOP;
--                                 Address <= to_nat(addr_bytes(25 downto 0));
--                                 change_addr <= '1','0' AFTER 1 ns;
--                                 IF EHP THEN
--                                     bus_cycle_state := MODE_BYTE;
--                                 ELSE
--                                     bus_cycle_state := DUMMY_BYTES;
--                                     IF ((DOUBLE AND NOT(hold_mode)) AND
--                                          VDLR_reg /= "00000000") THEN
--                                         read_out <= '1', '0' AFTER 1 ns;
--                                     END IF;
--                                 END IF;
--                             END IF;
--                         ELSIF (Instruct= DDRDIOR AND EXTADD= '0' ) THEN
--                             --Dual I/O DDR Read Mode
--                             Address_in(2*addr_cnt)  := SOIn;
--                             Address_in(2*addr_cnt+1):= SIIn;
--                             addr_cnt := addr_cnt + 1;
--                             read_cnt := 0;
--                             IF addr_cnt = (3*BYTE)/2 THEN
--                                 addr_cnt := 0;
--                                 FOR I IN 23 DOWNTO 0 LOOP
--                                     addr_bytes(23-i) := Address_in(i);
--                                 END LOOP;
--                                 addr_bytes(31 downto 26) := "000000";
--                                 addr_bytes(25 downto 24) :=
--                                            Bank_Addr_reg (1 downto 0);
--                                 Address <= to_nat(addr_bytes);
--                                 change_addr <= '1','0' AFTER 1 ns;
--                                 IF EHP THEN
--                                     bus_cycle_state := MODE_BYTE;
--                                 ELSE
--                                     bus_cycle_state := DUMMY_BYTES;
--                                 END IF;
--                             END IF;
--                         ELSIF (Instruct=DDRDIOR4 OR
--                               (Instruct=DDRDIOR AND EXTADD= '1' )) THEN
--                              --Dual I/O DDR Read Mode
--                             Address_in(2*addr_cnt)   := SOIn;
--                             Address_in(2*addr_cnt+1) := SIIn;
--                             addr_cnt := addr_cnt + 1;
--                             read_cnt := 0;
--                             IF addr_cnt = (4*BYTE)/2 THEN
--                                 addr_cnt := 0;
--                                 FOR I IN 31 DOWNTO 0 LOOP
--                                     addr_bytes(31-i) := Address_in(i);
--                                 END LOOP;
--                                 Address <= to_nat(addr_bytes(25 downto 0));
--                                 change_addr <= '1','0' AFTER 1 ns;
--                                 IF EHP THEN
--                                     bus_cycle_state := MODE_BYTE;
--                                 ELSE
--                                     bus_cycle_state := DUMMY_BYTES;
--                                     IF ((DOUBLE AND NOT(hold_mode)) AND
--                                          VDLR_reg /= "00000000") THEN
--                                         read_out <= '1', '0' AFTER 1 ns;
--                                     END IF;
--                                 END IF;
--                             END IF;
--                         ELSIF (Instruct=DDRQIOR AND EXTADD= '0' ) AND
--                                QUAD = '1' THEN
--                             --Quad I/O DDR Read Mode
--                             Address_in(4*addr_cnt)   := HOLDNegIn;
--                             Address_in(4*addr_cnt+1) := WPNegIn;
--                             Address_in(4*addr_cnt+2) := SOIn;
--                             Address_in(4*addr_cnt+3) := SIIn;
--                             addr_cnt := addr_cnt + 1;
--                             read_cnt := 0;
--                             IF addr_cnt = (3*BYTE)/4 THEN
--                                 addr_cnt := 0;
--                                 FOR I IN 23 DOWNTO 0 LOOP
--                                     addr_bytes(23-i) := Address_in(i);
--                                 END LOOP;
--                                 addr_bytes(31 downto 26) := "000000";
--                                 addr_bytes(25 downto 24) :=
--                                            Bank_Addr_reg (1 downto 0);
--                                 Address <= to_nat(addr_bytes);
--                                 change_addr <= '1','0' AFTER 5 ns;
--                                 bus_cycle_state := MODE_BYTE;
--                             END IF;
--                         ELSIF QUAD = '1' AND (Instruct=DDRQIOR4 OR
--                               (Instruct=DDRQIOR AND EXTADD= '1' )) THEN
--                             Address_in(4*addr_cnt)   := HOLDNegIn;
--                             Address_in(4*addr_cnt+1) := WPNegIn;
--                             Address_in(4*addr_cnt+2) := SOIn;
--                             Address_in(4*addr_cnt+3) := SIIn;
--                             addr_cnt := addr_cnt + 1;
--                             read_cnt := 0;
--                             IF addr_cnt = (4*BYTE)/4 THEN
--                                 addr_cnt := 0;
--                                 FOR I IN 31 DOWNTO 0 LOOP
--                                     addr_bytes(31-i) := Address_in(i);
--                                 END LOOP;
--                                 Address <= to_nat(addr_bytes(25 downto 0));
--                                 change_addr <= '1','0' AFTER 1 ns;
--                                 bus_cycle_state := MODE_BYTE;
--                             END IF;
--                         END IF;
--                     END IF;

--                 WHEN MODE_BYTE =>
--                     IF rising_edge(SCK_ipd) THEN
--                         IF ((Instruct=DIOR OR Instruct = DIOR4) AND
--                          ((HOLDNeg_pullup='1' AND QUAD='0') OR QUAD='1')) THEN
--                             mode_in(2*mode_cnt)   := SOIn;
--                             mode_in(2*mode_cnt+1) := SIIn;
--                             mode_cnt := mode_cnt + 1;
--                             IF mode_cnt = BYTE/2 THEN
--                                 mode_cnt := 0;
--                                 FOR I IN 7 DOWNTO 0 LOOP
--                                     mode_bytes(i) := mode_in(7-i);
--                                 END LOOP;
--                                 IF Latency_code = 0 OR Latency_code = 3 THEN
--                                     bus_cycle_state := DATA_BYTES;
--                                 ELSE
--                                     bus_cycle_state := DUMMY_BYTES;
--                                 END IF;
--                             END IF;
--                         ELSIF (Instruct=QIOR OR Instruct = QIOR4)
--                                AND QUAD = '1' THEN
--                             mode_in(4*mode_cnt)   := HOLDNegIn;
--                             mode_in(4*mode_cnt+1) := WPNegIn;
--                             mode_in(4*mode_cnt+2) := SOIn;
--                             mode_in(4*mode_cnt+3) := SIIn;
--                             mode_cnt := mode_cnt + 1;
--                             IF mode_cnt = BYTE/4 THEN
--                                 mode_cnt := 0;
--                                 FOR I IN 7 DOWNTO 0 LOOP
--                                     mode_bytes(i) := mode_in(7-i);
--                                 END LOOP;
--                                 bus_cycle_state := DUMMY_BYTES;
--                             END IF;
--                         ELSIF Instruct=DDRFR OR Instruct = DDRFR4 THEN
--                             mode_in(2*mode_cnt)   := SIIn;
--                         ELSIF Instruct=DDRDIOR OR Instruct = DDRDIOR4 THEN
--                             mode_in(4*mode_cnt)   := SOIn;
--                             mode_in(4*mode_cnt+1) := SIIn;
--                         ELSIF (Instruct=DDRQIOR OR Instruct = DDRQIOR4)
--                              AND QUAD = '1' THEN
--                             mode_in(0) := HOLDNegIn;
--                             mode_in(1) := WPNegIn;
--                             mode_in(2) := SOIn;
--                             mode_in(3) := SIIn;
--                         END IF;
--                         dummy_cnt := 0;
--                         dummy_cnt_act_temp <= dummy_cnt;
--                     ELSIF falling_edge(SCK_ipd) THEN
--                         IF Instruct=DDRFR OR Instruct = DDRFR4 THEN
--                             mode_in(2*mode_cnt+1)   := SIIn;
--                             mode_cnt := mode_cnt + 1;
--                             IF mode_cnt = BYTE/2 THEN
--                                 mode_cnt := 0;
--                                 FOR I IN 7 DOWNTO 0 LOOP
--                                     mode_bytes(i) := mode_in(7-i);
--                                 END LOOP;
--                                 bus_cycle_state := DUMMY_BYTES;
--                             END IF;
--                         ELSIF Instruct=DDRDIOR OR Instruct = DDRDIOR4 THEN
--                             mode_in(4*mode_cnt+2) := SOIn;
--                             mode_in(4*mode_cnt+3) := SIIn;
--                             mode_cnt := mode_cnt + 1;
--                             IF mode_cnt = BYTE/4 THEN
--                                 mode_cnt := 0;
--                                 FOR I IN 7 DOWNTO 0 LOOP
--                                     mode_bytes(i) := mode_in(7-i);
--                                 END LOOP;
--                                 bus_cycle_state := DUMMY_BYTES;
--                             END IF;
--                         ELSIF (Instruct=DDRQIOR OR Instruct = DDRQIOR4)
--                                AND QUAD = '1' THEN
--                             mode_in(4) := HOLDNegIn;
--                             mode_in(5) := WPNegIn;
--                             mode_in(6) := SOIn;
--                             mode_in(7) := SIIn;
--                             FOR I IN 7 DOWNTO 0 LOOP
--                                 mode_bytes(i) := mode_in(7-i);
--                             END LOOP;
--                             bus_cycle_state := DUMMY_BYTES;
--                         END IF;
--                     END IF;

--                 WHEN DUMMY_BYTES =>
--                     IF rising_edge(SCK_ipd) THEN
--                         IF (((Instruct=FSTRD OR Instruct=FSTRD4 OR
--                               Instruct=DOR OR Instruct=DOR4 OR
--                               Instruct=OTPR OR Instruct=RSFDP) AND
--                            ((HOLDNeg_pullup='1' AND QUAD='0') OR QUAD='1')) OR
--                            ((Instruct = QOR OR Instruct = QOR4) AND
--                              QUAD = '1'))THEN
--                             dummy_cnt := dummy_cnt + 1;
--                             dummy_cnt_act_temp <= dummy_cnt;
--                             IF dummy_cnt = BYTE THEN
--                                 bus_cycle_state := DATA_BYTES;
--                             END IF;
--                         ELSIF (Instruct=DDRFR OR Instruct=DDRFR4) THEN
--                             dummy_cnt := dummy_cnt + 1;
--                             dummy_cnt_act_temp <= dummy_cnt;
--                             IF EHP THEN
--                                 IF ((Latency_code = 3 AND dummy_cnt=1) OR
--                                 (Latency_code = 0 AND dummy_cnt=2) OR
--                                 (Latency_code = 1 AND dummy_cnt=4) OR
--                                 (Latency_code = 2 AND dummy_cnt=5)) THEN
--                                     bus_cycle_state := DATA_BYTES;
--                                 END IF;
--                                 IF VDLR_reg/="00000000" AND Latency_code=2 THEN
--                                     IF dummy_cnt>1 THEN
--                                         read_out <= '1', '0' AFTER 1 ns;
--                                         IF dummy_cnt=5 THEN
--                                             DummyBytes_act  := '1';
--                                         END IF;
--                                     END IF;
--                                 END IF;
--                             ELSE
--                                 IF ((Latency_code = 3 AND dummy_cnt = 4) OR
--                                 (Latency_code = 0 AND dummy_cnt = 5) OR
--                                 (Latency_code = 1 AND dummy_cnt = 6) OR
--                                 (Latency_code = 2 AND dummy_cnt = 7)) THEN
--                                     bus_cycle_state := DATA_BYTES;
--                                  END IF;
--                                 IF VDLR_reg/="00000000" THEN
--                                     IF Latency_code=0 THEN
--                                         IF dummy_cnt>1 THEN
--                                             read_out <= '1', '0' AFTER 1 ns;
--                                             IF dummy_cnt=5 THEN
--                                                 DummyBytes_act  := '1';
--                                             END IF;
--                                         END IF;
--                                     ELSIF Latency_code = 1 THEN
--                                         IF dummy_cnt>2 THEN
--                                             read_out <= '1', '0' AFTER 1 ns;
--                                             IF dummy_cnt=6 THEN
--                                                 DummyBytes_act  := '1';
--                                             END IF;
--                                         END IF;
--                                     ELSIF Latency_code = 2 THEN
--                                         IF dummy_cnt>3 THEN
--                                             read_out <= '1', '0' AFTER 1 ns;
--                                             IF dummy_cnt=7 THEN
--                                                 DummyBytes_act  := '1';
--                                             END IF;
--                                         END IF;
--                                     END IF;
--                                 END IF;
--                              END IF;
--                         ELSIF Instruct=RES THEN
--                             dummy_cnt := dummy_cnt + 1;
--                             dummy_cnt_act_temp <= dummy_cnt;
--                             IF dummy_cnt = 3*BYTE THEN
--                                 bus_cycle_state := DATA_BYTES;
--                             END IF;
--                         ELSIF Instruct=ECCRD THEN
--                             dummy_cnt := dummy_cnt + 1;
--                             dummy_cnt_act_temp <= dummy_cnt;
--                             IF dummy_cnt = BYTE THEN
--                                 bus_cycle_state := DATA_BYTES;
--                             END IF;
--                         ELSIF (Instruct = DIOR OR Instruct = DIOR4) AND
--                            ((HOLDNeg_pullup='1' AND QUAD='0') OR QUAD='1') THEN
--                             dummy_cnt := dummy_cnt + 1;
--                             dummy_cnt_act_temp <= dummy_cnt;
--                             IF EHP THEN
--                                 IF ((Latency_code = 1 AND dummy_cnt = 1) OR
--                                     (Latency_code = 2 AND dummy_cnt = 2)) THEN
--                                     bus_cycle_state := DATA_BYTES;
--                                 END IF;
--                             ELSE
--                                 IF ((Latency_code = 3 AND dummy_cnt = 4) OR
--                                     (Latency_code = 0 AND dummy_cnt = 4) OR
--                                     (Latency_code = 1 AND dummy_cnt = 5) OR
--                                     (Latency_code = 2 AND dummy_cnt = 6)) THEN
--                                     bus_cycle_state := DATA_BYTES;
--                                 END IF;
--                             END IF;
--                         ELSIF Instruct = DDRDIOR OR Instruct = DDRDIOR4 THEN
--                             dummy_cnt := dummy_cnt + 1;
--                             dummy_cnt_act_temp <= dummy_cnt;
--                             IF EHP THEN
--                                 IF ((Latency_code = 3 AND dummy_cnt = 2) OR
--                                 (Latency_code = 0 AND dummy_cnt = 4) OR
--                                 (Latency_code = 1 AND dummy_cnt = 5) OR
--                                 (Latency_code = 2 AND dummy_cnt = 6)) THEN
--                                     bus_cycle_state := DATA_BYTES;
--                                 END IF;
--                                 IF VDLR_reg/="00000000" THEN
--                                     IF Latency_code=1 THEN
--                                         IF dummy_cnt>1 THEN
--                                             read_out <= '1', '0' AFTER 1 ns;
--                                             IF dummy_cnt=5 THEN
--                                                 DummyBytes_act  := '1';
--                                             END IF;
--                                         END IF;
--                                     ELSIF Latency_code=2 THEN
--                                         IF dummy_cnt>2 THEN
--                                             read_out <= '1', '0' AFTER 1 ns;
--                                             IF dummy_cnt=6 THEN
--                                                 DummyBytes_act  := '1';
--                                             END IF;
--                                         END IF;
--                                     END IF;
--                                 END IF;
--                             ELSE
--                                 IF ((Latency_code = 3 AND dummy_cnt = 4) OR
--                                 (Latency_code = 0 AND dummy_cnt = 6) OR
--                                 (Latency_code = 1 AND dummy_cnt = 7) OR
--                                 (Latency_code = 2 AND dummy_cnt = 8)) THEN
--                                     bus_cycle_state := DATA_BYTES;
--                                 END IF;
--                                 IF VDLR_reg/="00000000" THEN
--                                     IF Latency_code=0  THEN
--                                         IF dummy_cnt>2 THEN
--                                             read_out <= '1', '0' AFTER 1 ns;
--                                             IF dummy_cnt=6 THEN
--                                                 DummyBytes_act  := '1';
--                                             END IF;
--                                         END IF;
--                                     ELSIF Latency_code = 1 THEN
--                                         IF dummy_cnt>3 THEN
--                                             read_out <= '1', '0' AFTER 1 ns;
--                                             IF dummy_cnt=7 THEN
--                                                 DummyBytes_act  := '1';
--                                             END IF;
--                                         END IF;
--                                     ELSIF Latency_code = 2 THEN
--                                         IF dummy_cnt>4 THEN
--                                             read_out <= '1', '0' AFTER 1 ns;
--                                             IF dummy_cnt=8 THEN
--                                                 DummyBytes_act  := '1';
--                                             END IF;
--                                         END IF;
--                                     END IF;
--                                 END IF;
--                             END IF;
--                         ELSIF ((Instruct = QIOR OR Instruct = QIOR4) AND
--                               QUAD = '1') THEN
--                             dummy_cnt := dummy_cnt + 1;
--                             dummy_cnt_act_temp <= dummy_cnt;
--                             IF ((Latency_code = 3 AND dummy_cnt = 1) OR
--                                 (Latency_code = 0 AND dummy_cnt = 4) OR
--                                 (Latency_code = 1 AND dummy_cnt = 4) OR
--                                 (Latency_code = 2 AND dummy_cnt = 5)) THEN
--                                 bus_cycle_state := DATA_BYTES;
--                             END IF;
--                         ELSIF (Instruct = DDRQIOR OR Instruct = DDRQIOR4)
--                               AND QUAD = '1' THEN
--                             dummy_cnt := dummy_cnt + 1;
--                             dummy_cnt_act_temp <= dummy_cnt;
--                             IF ((Latency_code = 3 AND dummy_cnt = 3) OR
--                             (Latency_code = 0 AND dummy_cnt = 6) OR
--                             (Latency_code = 1 AND dummy_cnt = 7) OR
--                             (Latency_code = 2 AND dummy_cnt = 8)) THEN
--                                 bus_cycle_state := DATA_BYTES;
--                             END IF;
--                             IF VDLR_reg/="00000000" THEN
--                                IF Latency_code = 0 THEN
--                                     IF dummy_cnt>2 THEN
--                                         read_out <= '1', '0' AFTER 1 ns;
--                                         IF dummy_cnt=6 THEN
--                                             DummyBytes_act  := '1';
--                                         END IF;
--                                     END IF;
--                                 ELSIF Latency_code = 1 THEN
--                                     IF dummy_cnt>3 THEN
--                                         read_out <= '1', '0' AFTER 1 ns;
--                                         IF dummy_cnt=7 THEN
--                                             DummyBytes_act  := '1';
--                                         END IF;
--                                     END IF;
--                                 ELSIF Latency_code = 2 THEN
--                                     IF dummy_cnt>4 THEN
--                                         read_out <= '1', '0' AFTER 1 ns;
--                                         IF dummy_cnt=8 THEN
--                                             DummyBytes_act  := '1';
--                                         END IF;
--                                     END IF;
--                                 END IF;
--                             END IF;
--                         END IF;
--                     ELSIF falling_edge(SCK_ipd) THEN
--                         IF VDLR_reg /= "00000000" THEN
--                             IF (Instruct=DDRFR OR Instruct=DDRFR4) THEN
--                                 IF EHP THEN
--                                     IF (Latency_code = 2 AND dummy_cnt>=1) THEN
--                                         read_out <= '1', '0' AFTER 1 ns;
--                                     END IF;
--                                 ELSE
--                                     IF (Latency_code = 0 AND dummy_cnt>=1) THEN
--                                         read_out <= '1', '0' AFTER 1 ns;
--                                     ELSIF (Latency_code = 1 AND dummy_cnt>=2) THEN
--                                         read_out <= '1', '0' AFTER 1 ns;
--                                     ELSIF (Latency_code = 2 AND dummy_cnt>=3) THEN
--                                         read_out <= '1', '0' AFTER 1 ns;
--                                     END IF;
--                                 END IF;
--                             ELSIF (Instruct=DDRDIOR OR Instruct=DDRDIOR4) THEN
--                                 IF EHP THEN
--                                     IF (Latency_code = 1 AND dummy_cnt>=1) THEN
--                                         read_out <= '1', '0' AFTER 1 ns;
--                                     ELSIF (Latency_code = 2 AND dummy_cnt>=2) THEN
--                                         read_out <= '1', '0' AFTER 1 ns;
--                                     END IF;
--                                 ELSE
--                                     IF (Latency_code = 0 AND dummy_cnt>=2) THEN
--                                         read_out <= '1', '0' AFTER 1 ns;
--                                     ELSIF (Latency_code = 1 AND dummy_cnt>=3) THEN
--                                         read_out <= '1', '0' AFTER 1 ns;
--                                     ELSIF (Latency_code = 2 AND dummy_cnt>=4) THEN
--                                         read_out <= '1', '0' AFTER 1 ns;
--                                     END IF;
--                                 END IF;
--                             ELSIF (Instruct=DDRQIOR OR Instruct=DDRQIOR4) THEN
--                                 IF (Latency_code = 0 AND dummy_cnt>=2) THEN
--                                     read_out <= '1', '0' AFTER 1 ns;
--                                 ELSIF (Latency_code = 1 AND dummy_cnt>=3) THEN
--                                     read_out <= '1', '0' AFTER 1 ns;
--                                 ELSIF (Latency_code = 2 AND dummy_cnt>=4) THEN
--                                     read_out <= '1', '0' AFTER 1 ns;
--                                 END IF;
--                             END IF;
--                         END IF;
--                     END IF;

--                 WHEN DATA_BYTES =>
--                     IF rising_edge(CSNeg_ipd) THEN
--                         IF ((mode_bytes(7 downto 4) = "1010" AND
--                           (Instruct = DIOR OR Instruct = DIOR4 OR
--                            Instruct = QIOR OR Instruct = QIOR4)) OR
--                           ((mode_bytes(7 downto 4) =
--                                     NOT(mode_bytes(3 downto 0))) AND
--                           (Instruct = DDRFR  OR Instruct = DDRFR4  OR
--                            Instruct = DDRDIOR OR Instruct = DDRDIOR4 OR
--                            Instruct = DDRQIOR OR Instruct = DDRQIOR4))) THEN
--                             bus_cycle_state := ADDRESS_BYTES;
--                         ELSE
--                             bus_cycle_state := STAND_BY;
--                         END IF;
--                     END IF;
--                     IF falling_edge(SCK_ipd) AND CSNeg_ipd = '0' AND
--                        NOT(DOUBLE) THEN
--                         IF ((Instruct = READ OR Instruct=RD4 OR
--                             Instruct = FSTRD OR Instruct = FSTRD4 OR
--                             Instruct = RDSR  OR Instruct = RDSR2 OR
--                             Instruct = RDCR  OR Instruct = OTPR OR
--                             Instruct=RSFDP OR
--                             Instruct = DOR  OR Instruct = DOR4 OR
--                             Instruct = DIOR OR Instruct = DIOR4 OR
--                             Instruct = ABRD  OR Instruct = BRRD OR
--                             Instruct = ASPRD OR Instruct = DYBRD OR
--                             Instruct = PPBRD OR Instruct = ECCRD OR
--                             Instruct = PASSRD OR Instruct = RDID OR
--                             Instruct = RES  OR Instruct = REMS OR
--                             Instruct = PLBRD OR Instruct = DLPRD)
--                            AND ((HOLDNeg_pullup='1' AND QUAD='0') OR QUAD='1'))
--                            OR (current_state = AUTOBOOT AND start_delay = 0)OR
--                           ((Instruct=QOR OR Instruct=QIOR OR Instruct=QOR4
--                             OR Instruct= QIOR4) AND QUAD = '1') THEN
--                             read_out <= '1', '0' AFTER 1 ns;
--                         END IF;
--                     ELSIF SCK_ipd'EVENT AND CSNeg_ipd = '0' AND DOUBLE THEN
--                         read_out <= '1', '0' AFTER 1 ns;
--                     END IF;

--                     IF rising_edge(SCK_ipd) THEN
--                         IF QUAD = '1' AND (Instruct=QPP OR Instruct = QPP4)THEN
--                             quad_nybble := HOLDNegIn & WPNegIn & SOIn & SIIn;
--                             IF data_cnt > (PageSize*2-1) THEN
--                             --In case of quad mode and QPP,
--                             --if more than 512 bytes are sent to the device
--                                 FOR I IN 0 TO (PageSize*2-2) LOOP
--                                     quad_data_in(i) := quad_data_in(i+1);
--                                 END LOOP;
--                                 quad_data_in((PageSize*2-1)) :=
--                                                     to_nat(quad_nybble);
--                                 data_cnt := data_cnt +1;
--                             ELSE
--                                 IF quad_nybble /= "ZZZZ" THEN
--                                     quad_data_in(data_cnt) :=
--                                     to_nat(quad_nybble);
--                                 END IF;
--                                     data_cnt := data_cnt +1;
--                             END IF;
--                         ELSIF ((HOLDNeg_pullup='1' AND QUAD='0')
--                                 OR QUAD='1') THEN
--                             IF data_cnt > (PageSize*8-1) THEN
--                             --In case of serial mode and PP,
--                             -- if more than 512 bytes are sent to the device
--                             -- previously latched data are discarded and last
--                             -- 512 data bytes are guaranteed to be programmed
--                             -- correctly within the same page.
--                                 IF bit_cnt = 0 THEN
--                                     FOR I IN 0 TO ((PageSize-1)*BYTE - 1) LOOP
--                                         Data_in(i) := Data_in(i+8);
--                                     END LOOP;
--                                 END IF;
--                                 Data_in((PageSize-1)*BYTE + bit_cnt) := SIIn;
--                                 bit_cnt := bit_cnt + 1;
--                                 IF bit_cnt = 8 THEN
--                                     bit_cnt := 0;
--                                 END IF;
--                                 data_cnt := data_cnt + 1;
--                             ELSE
--                                 Data_in(data_cnt) := SIIn;
--                                 data_cnt := data_cnt + 1;
--                                 bit_cnt := 0;
--                             END IF;
--                         END IF;
--                     END IF;

--                     IF rising_edge(CSNeg_ipd) THEN
--                     CASE Instruct IS
--                         WHEN WREN | WRDI | BE | SE | SE4 | CLSR | RESET |
--                              PPBP | PPBERS | PGSP | PGRS | ERSP | BRAC |
--                              ERRS | P4E | P4E4 | PLBWR =>
--                             IF ((HOLDNeg_pullup='1' AND QUAD='0')
--                                 OR QUAD='1') THEN
--                                 IF data_cnt = 0 THEN
--                                     write <= '0';
--                                 END IF;
--                             END IF;

--                         WHEN WRR =>
--                             IF ((HOLDNeg_pullup='1' AND QUAD='0')
--                                 OR QUAD='1') THEN
--                                 IF ((data_cnt mod 8) = 0 AND data_cnt > 0) THEN
--                                     IF data_cnt = 8 THEN
--                                     --If CS# is driven high after eight
--                                     --cycle,only the Status Register is
--                                     --written to.
--                                         write <= '0';
--                                         IF BAR_ACC = '0' THEN
--                                             FOR i IN 0 TO 7 LOOP
--                                                 Status_reg1_in(i) <=
--                                                                    Data_in(7-i);
--                                             END LOOP;
--                                         ELSIF P_ERR = '0' AND E_ERR = '0' THEN
--                                             FOR i IN 0 TO 7 LOOP
--                                                 Bank_Addr_reg_in(i) <=
--                                                                    Data_in(7-i);
--                                             END LOOP;
--                                         END IF;
--                                     ELSIF data_cnt = 16 THEN
--                                     --After the 16th cycle both the
--                                     --Status and Configuration Registers
--                                     --are written to.
--                                         write <= '0';
--                                         IF BAR_ACC = '0' THEN
--                                             FOR i IN 0 TO 7 LOOP
--                                                 cfg_write  <= '1';
--                                                 FOR i IN 0 TO 7 LOOP
--                                                     Status_reg1_in(i) <=
--                                                                    Data_in(7-i);
--                                                     Config_reg1_in(i) <=
--                                                                   Data_in(15-i);
--                                                 END LOOP;
--                                             END LOOP;
--                                         ELSIF P_ERR = '0' AND E_ERR = '0' THEN
--                                             FOR i IN 0 TO 7 LOOP
--                                                 Bank_Addr_reg_in(i) <=
--                                                                    Data_in(7-i);
--                                             END LOOP;
--                                         END IF;
--                                     END IF;
--                                 END IF;
--                             END IF;

--                         WHEN PP | PP4 | OTPP =>
--                             IF ((HOLDNeg_pullup='1' AND QUAD='0') OR
--                                  QUAD='1') THEN
--                                 IF data_cnt > 0 THEN
--                                     IF (data_cnt mod 8) = 0 THEN
--                                         write <= '0';
--                                         FOR I IN 0 TO (PageSize-1) LOOP
--                                             FOR J IN 7 DOWNTO 0 LOOP
--                                                 IF Data_in((i*8) + (7-j))
--                                                                   /= 'X' THEN
--                                                     Byte_slv(j) :=
--                                                     Data_in((i*8) + (7-j));
--                                                 END IF;
--                                             END LOOP;
--                                             WByte(i) <=
--                                                 to_nat(Byte_slv);
--                                         END LOOP;
--                                         IF data_cnt > PageSize*BYTE THEN
--                                             Byte_number <= PageSize-1;
--                                         ELSE
--                                             Byte_number <= data_cnt/8-1;
--                                         END IF;
--                                     END IF;
--                                 END IF;
--                             END IF;

--                         WHEN QPP | QPP4=>
--                             IF data_cnt > 0 THEN
--                                 IF data_cnt mod 2 = 0 THEN
--                                     quadpg   <= FALSE;
--                                     write <= '0';
--                                     FOR I IN 0 TO (PageSize-1) LOOP
--                                         FOR J IN 1 DOWNTO 0 LOOP
--                                             Quad_slv :=
--                                             to_slv(quad_data_in((i*2) +
--                                               (1-j)),4);

--                                             Byte_slv(4*j+3 DOWNTO 4*j) :=
--                                                 Quad_slv;
--                                         END LOOP;
--                                         WByte(i) <= to_nat(Byte_slv);
--                                     END LOOP;
--                                     IF data_cnt > PageSize*2 THEN
--                                         Byte_number <= PageSize-1;
--                                     ELSE
--                                         Byte_number <= data_cnt/2-1;
--                                     END IF;
--                                 END IF;
--                             END IF;

--                         WHEN ABWR =>
--                             IF ((HOLDNeg_pullup='1' AND QUAD='0') OR
--                                  QUAD='1') THEN
--                                 IF data_cnt = 32 THEN
--                                     write <= '0';
--                                     FOR J IN 0 TO 31 LOOP
--                                         AutoBoot_reg_in(J) <=
--                                                 Data_in(31-J);
--                                     END LOOP;
--                                 END IF;
--                             END IF;

--                         WHEN BRWR =>
--                             IF ((HOLDNeg_pullup='1' AND QUAD='0') OR
--                                  QUAD='1') THEN
--                                 IF data_cnt = 8 THEN
--                                     write <= '0';
--                                     FOR J IN 0 TO 7 LOOP
--                                         Bank_Addr_reg_in(J) <=
--                                               Data_in(7-J);
--                                     END LOOP;
--                                 END IF;
--                             END IF;

--                         WHEN ASPP =>
--                             IF ((HOLDNeg_pullup='1' AND QUAD='0') OR
--                                  QUAD='1') THEN
--                                 IF data_cnt = 16 THEN
--                                     write <= '0';
--                                     FOR J IN 0 TO 15 LOOP
--                                         ASP_reg_in(J) <=
--                                               Data_in(15-J);
--                                     END LOOP;
--                                 END IF;
--                             END IF;

--                         WHEN DYBWR =>
--                             IF ((HOLDNeg_pullup='1' AND QUAD='0') OR
--                                  QUAD='1') THEN
--                                 IF data_cnt = 8 THEN
--                                     write <= '0';
--                                     FOR J IN 0 TO 7 LOOP
--                                         DYBAR_in(J) <=
--                                               Data_in(7-J);
--                                     END LOOP;
--                                 END IF;
--                             END IF;

--                         WHEN PNVDLR =>
--                             IF ((HOLDNeg_pullup='1' AND QUAD='0') OR
--                                  QUAD='1') THEN
--                                 IF data_cnt = 8 THEN
--                                     write <= '0';
--                                     FOR J IN 0 TO 7 LOOP
--                                         NVDLR_reg_in(J) <= Data_in(7-J);
--                                     END LOOP;
--                                 END IF;
--                             END IF;

--                         WHEN WVDLR =>
--                             IF ((HOLDNeg_pullup='1' AND QUAD='0') OR
--                                  QUAD='1') THEN
--                                 IF data_cnt = 8 THEN
--                                     write <= '0';
--                                     FOR J IN 0 TO 7 LOOP
--                                         VDLR_reg_in(J) <= Data_in(7-J);
--                                     END LOOP;
--                                 END IF;
--                             END IF;

--                         WHEN PASSP =>
--                             IF ((HOLDNeg_pullup='1' AND QUAD='0') OR
--                                  QUAD='1') THEN
--                                 IF data_cnt = 64 THEN
--                                     write <= '0';
--                                     FOR J IN 1 TO 8 LOOP
--                                         FOR K IN 1 TO 8 LOOP
--                                             Password_reg_in(J*8-K) <=
--                                                        Data_in(8*(J-1)+K-1);
--                                         END LOOP;
--                                     END LOOP;
--                                 END IF;
--                             END IF;

--                         WHEN PASSU =>
--                             IF ((HOLDNeg_pullup='1' AND QUAD='0') OR
--                                  QUAD='1') THEN
--                                 IF data_cnt = 64 THEN
--                                     write <= '0';
--                                     FOR J IN 1 TO 8 LOOP
--                                         FOR K IN 1 TO 8 LOOP
--                                             PASS_TEMP(J*8-K) <=
--                                                        Data_in(8*(J-1)+K-1);
--                                         END LOOP;
--                                     END LOOP;
--                                 END IF;
--                             END IF;

--                         WHEN others =>
--                             null;

--                     END CASE;
--                 END IF;
--             END CASE;
--         END IF;

--     END PROCESS BusCycleDecode;

--     ---------------------------------------------------------------------------
--     -- Timing control for the Page Program
--     ---------------------------------------------------------------------------
--     ProgTime : PROCESS(PSTART, PGSUSP, PGRES, RST_in, Reseted)
--         VARIABLE pob      : time;
--         VARIABLE elapsed  : time;
--         VARIABLE start    : time;
--         VARIABLE duration : time;
--     BEGIN
--         IF rising_edge(PSTART) AND PDONE = '1' THEN
--             IF Instruct = PP OR Instruct = PP4 OR Instruct = OTPP OR
--                Instruct = QPP OR Instruct = QPP4 THEN
--                IF LongTimming THEN
--                    pob  := tdevice_PP;
--                ELSE
--                    pob  := tdevice_PP / 10;
--                END IF;
--             ELSE
--                 IF LongTimming THEN
--                    pob  := tdevice_BP;
--                ELSE
--                    pob  := tdevice_BP / 10;
--                END IF;
--             END IF;
--             elapsed := 0 ns;
--             start := NOW;
--             PDONE <= '0', '1' AFTER pob;
--         ELSIF PGSUSP'EVENT AND PGSUSP = '1' AND PDONE /= '1' THEN
--             elapsed  := NOW - start;
--             duration := pob - elapsed;
--             PDONE <= '0';
--         ELSIF PGRES'EVENT AND PGRES = '1' AND PDONE /= '1'THEN
--             start := NOW;
--             PDONE <= '0', '1' AFTER duration;
--         END IF;

--         IF rising_edge(RST_in) THEN
--             PDONE <= '1';  -- reset done, programing terminated
--         ELSIF rising_edge(Reseted) THEN
--             PDONE <= '1';  -- reset done, programing terminated
--         END IF;

--     END PROCESS ProgTime;

--     ---------------------------------------------------------------------------
--     -- Timing control for the Max Setup Time
--     ---------------------------------------------------------------------------
--     SetupTime : PROCESS(SI, SCK)
--         VARIABLE start_tsu       : time;
--         VARIABLE elapsed_tsu     : time;
--         VARIABLE duration_tsu    : time;
--     BEGIN
--         IF rising_edge(SI) OR falling_edge(SI) THEN
--             IF Instruct = PGSP OR Instruct = PGRS OR
--                Instruct = ERSP OR Instruct = ERRS THEN
--                start_tsu := NOW;
--             END IF;
--         END IF;

--         IF rising_edge(SCK) THEN
--             IF Instruct = PGSP OR Instruct = PGRS OR
--                Instruct = ERSP OR Instruct = ERRS THEN
--                elapsed_tsu := NOW - start_tsu;
--                duration_tsu := tdevice_TSU - elapsed_tsu;
--                IF (duration_tsu < 0 ns) THEN
--                    REPORT "Warning!" &
--                     "tSU max time violation"
--                    SEVERITY WARNING;
--                END IF;
--             END IF;
--         END IF;

--     END PROCESS SetupTime;
--     ---------------------------------------------------------------------------
--     -- Timing control for the Write Status Register
--     ---------------------------------------------------------------------------
--     WriteTime : PROCESS(WSTART, RST_in)
--         VARIABLE wob      : time;
--     BEGIN
--         IF LongTimming THEN
--             wob  := tdevice_WRR;
--         ELSE
--             wob  := tdevice_WRR / 1000;
--         END IF;
--         IF rising_edge(WSTART) AND WDONE = '1' THEN
--             WDONE <= '0', '1' AFTER wob;
--         END IF;

--         IF rising_edge(RST_in) THEN
--             WDONE <= '1';  -- reset done, programing terminated
--         ELSIF rising_edge(Reseted) THEN
--             WDONE <= '1';  -- reset done, programing terminated
--         END IF;

--     END PROCESS WriteTime;

--     ---------------------------------------------------------------------------
--     -- Timing control for the Bulk Erase
--     ---------------------------------------------------------------------------
--     ErsTime : PROCESS(ESTART, ESUSP, ERES, RST_in, Reseted)
--         VARIABLE seo      : time;
--         VARIABLE beo      : time;
--         VARIABLE elapsed  : time;
--         VARIABLE start    : time;
--         VARIABLE duration : time;
--     BEGIN
--         IF LongTimming THEN
--             seo := tdevice_SE;
--             beo := tdevice_BE;
--         ELSE
--             seo := tdevice_SE / 1000;
--             beo := tdevice_BE / 100000;
--         END IF;
--         IF rising_edge(ESTART) AND EDONE = '1' THEN
--             IF Instruct = BE THEN
--                 duration := beo;
--             ELSE --Instruct = SE OR SE4
--                 duration := seo;
--             END IF;
--             elapsed := 0 ns;
--             EDONE <= '0', '1' AFTER duration;
--             start := NOW;
--         ELSIF ESUSP'EVENT AND ESUSP = '1' AND EDONE /= '1' THEN
--             elapsed  := NOW - start;
--             duration := duration - elapsed;
--             EDONE <= '0';
--         ELSIF ERES'EVENT AND ERES = '1' AND EDONE /= '1' THEN
--             start := NOW;
--             EDONE <= '0', '1' AFTER duration;
--         END IF;

--         IF rising_edge(RST_in) THEN
--             EDONE <= '1';  -- reset done, eras terminated
--         ELSIF rising_edge(Reseted) THEN
--             EDONE <= '1';  -- reset done, erase terminated
--         END IF;

--     END PROCESS ErsTime;

--     CheckCEOnPowerUP :PROCESS(CSNeg_ipd)
--     BEGIN
--         IF (PoweredUp = '0' AND falling_edge(CSNeg_ipd)) THEN
--             REPORT InstancePath & partID &
--             ": Device is selected during Power Up"
--             SEVERITY WARNING;
--         END IF;
--     END PROCESS;

--     ---------------------------------------------------------------------------
--     -- Main Behavior Process
--     -- combinational process for next state generation
--     ---------------------------------------------------------------------------
--     StateGen :PROCESS(PoweredUp, write, CSNeg_ipd, RSTNeg_pullup, WDONE, PDONE,
--                       ERSSUSP_out, PRGSUSP_out, EDONE, RST_out, PPBERASE_in,
--                       PASSULCK_in)

--     VARIABLE sect      : NATURAL RANGE 0 TO SecNum;

--     BEGIN

--         IF rising_edge(PoweredUp) AND RSTNeg = '1' AND RST_out = '1' THEN
--             IF ABE = '1' AND RPME /= '0' THEN
--             --Autoboot is enabled and The Read Password feature is not enabled
--                 next_state <= AUTOBOOT;
--                 read_cnt    := 0;
--                 byte_cnt    := 1;
--                 read_addr   := to_nat(AutoBoot_reg(31 DOWNTO 9)&"000000000");
--                 start_delay := to_nat(AutoBoot_reg(8 DOWNTO 1));
--                 ABSD        := to_nat(AutoBoot_reg(8 DOWNTO 1));
--             ELSE
--                 next_state <= IDLE;
--             END IF;
--         ELSIF PoweredUp = '1' THEN
--             IF RST_out= '0' then
--                 next_state <= current_state;
--             ELSIF falling_edge(write) AND Instruct = RESET THEN
--                 IF ABE = '1' AND RPME /= '0' THEN
--                     read_cnt   := 0;
--                     byte_cnt    := 1;
--                     read_addr  := to_nat(AutoBoot_reg(31 DOWNTO 9)&
--                                                       "000000000");
--                     start_delay:= to_nat(AutoBoot_reg(8 DOWNTO 1));
--                     ABSD       := to_nat(AutoBoot_reg(8 DOWNTO 1));
--                     next_state <= AUTOBOOT;
--                 ELSE
--                     next_state <= IDLE;
--                 END IF;
--             ELSE
--             CASE current_state IS
--                 WHEN  RESET_STATE =>
--                     IF (rising_edge(RST_out) AND RSTNeg = '1') OR
--                     (rising_edge(RSTNeg_pullup) AND RST_out = '1') THEN
--                         IF ABE = '1' AND RPME /= '0'
--                         AND RdPswdProtMode = '0' THEN
--                             next_state <= AUTOBOOT;
--                             read_cnt    := 0;
--                             byte_cnt    := 1;
--                             read_addr   := to_nat(AutoBoot_reg(31 DOWNTO 9)&
--                                                                "000000000");
--                             start_delay := to_nat(AutoBoot_reg(8 DOWNTO 1));
--                             ABSD        := to_nat(AutoBoot_reg(8 DOWNTO 1));
--                         ELSE
--                             next_state <= IDLE;
--                         END IF;
--                     END IF;

--                 WHEN  IDLE =>
--                     IF falling_edge(write) AND RdPswdProtMode = '0' THEN

--                         IF (Instruct = WRR AND WEL = '1' AND BAR_ACC = '0') THEN
--                             IF ((not(SRWD = '1' AND WPNeg_pullup = '0') AND
--                                      QUAD = '0') OR QUAD = '1') THEN
--                                 -- can not execute if HPM is entered
--                                 -- or if WEL bit is zero
--                                 IF ((TBPROT='1' AND Config_reg1_in(5)='0') OR
--                                     (BPNV  ='1' AND Config_reg1_in(3)='0')) AND
--                                     cfg_write = '1' THEN
--                                     ASSERT cfg_write = '0'
--                                     REPORT "Changing value of Configuration " &
--                                            "Register OTP bit from 1 to 0 is " &
--                                            "not allowed!!!"
--                                     SEVERITY WARNING;
--                                 ELSE
--                                     next_state <= WRITE_SR;
--                                 END IF;
--                             END IF;
--                         ELSIF (Instruct = PP OR Instruct = QPP OR
--                                 Instruct = PP4 OR Instruct = QPP4) AND
--                                 WEL = '1' THEN
--                             sect := Address/(SecSize+1);
--                             pgm_page := Address/PageSize;
--                             IF (Sec_Prot(sect) = '0' AND
--                                 PPB_bits(sect)='1' AND DYB_bits(sect)='1') THEN
-- --                                IF (Instruct = QPP OR Instruct = QPP4)  AND
-- --                                    QPP_page(pgm_page) = '1' THEN
-- --                                    REPORT "WARNING:The same page must not be" &
-- --                                           "programmed more than once!!!"
-- --                                   SEVERITY WARNING;
--                                     next_state <=  PAGE_PG;
-- --                                ELSE
-- --                                    next_state <=  PAGE_PG;
-- --                                END IF;
--                             END IF;
--                         ELSIF Instruct=OTPP AND WEL = '1' AND FREEZE = '0' THEN
--                             IF ((((Address >= 16#10# AND Address <= 16#13#) OR
--                                (Address >= 16#20# AND Address <= 16#FF#))
--                                AND LOCK_BYTE1(Address/32) = '1') OR
--                                ((Address >= 16#100# AND Address <= 16#1FF#)
--                                AND LOCK_BYTE2((Address-16#100#)/32) = '1') OR
--                                ((Address >= 16#200# AND Address <= 16#2FF#)
--                                AND LOCK_BYTE3((Address-16#200#)/32) = '1') OR
--                                ((Address >= 16#300# AND Address <= 16#3FF#)
--                                AND LOCK_BYTE4((Address-16#300#)/32) = '1')) AND
--                               (Address + Byte_number <= OTPHiAddr) THEN
--                                 next_state <=  OTP_PG;
--                             END IF;
--                         ELSIF (Instruct= SE OR Instruct= SE4) AND WEL = '1' THEN
--                             sect := Address/(SecSize+1);
--                             IF (Sec_Prot(sect) = '0' AND PPB_bits(sect)='1'
--                                 AND DYB_bits(sect)='1') THEN
--                                 next_state <=  SECTOR_ERS;
--                             END IF;
--                         ELSIF (Instruct=P4E OR Instruct=P4E4) AND WEL='1' THEN
--                             --A P4E instruction applied to a sector that
--                             --is larger than 4 KB will not be executed
--                             --and will not set the E_ERR status.
--                             REPORT "Warning! Parameter 4KB-sector Erase "&
--                                    "Instruction is not allowed for "&
--                                    "512Mbit memory size!!! "&
--                                    "Instruction is ignored!!!"
--                             SEVERITY warning;
--                         ELSIF Instruct = BE AND WEL = '1' AND
--                           (BP0='0' AND BP1='0' AND BP2='0') THEN
--                             next_state <= BULK_ERS;
--                         ELSIF Instruct = ABWR   AND WEL = '1' THEN
--                         --Autoboot Register Write Command
--                             next_state <= AUTOBOOT_PG;
--                         ELSIF Instruct = ASPP   AND WEL = '1' THEN
--                         --ASP Register Program Command
--                             IF not(ASPOTPFLAG) THEN
--                                 next_state <= ASP_PG;
--                             END IF;
--                         ELSIF Instruct = PLBWR  AND WEL = '1' AND
--                             RdPswdProtEnable = '0' THEN
--                             next_state <= PLB_PG;
--                         ELSIF Instruct = PASSP  AND WEL = '1' THEN
--                             IF not(PWDMLB='0' AND PSTMLB='1') THEN
--                                 next_state <= PASS_PG;
--                             END IF;
--                         ELSIF Instruct = PASSU  AND WEL= '1' AND WIP= '0' THEN
--                             next_state <= PASS_UNLOCK;
--                         ELSIF Instruct = PPBP   AND WEL = '1' THEN
--                             next_state <= PPB_PG;
--                         ELSIF (Instruct=PPBERS AND WEL='1' AND PPBOTP='1') THEN
--                             next_state <= PPB_ERS;
--                         ELSIF Instruct = DYBWR  AND WEL = '1' THEN
--                             next_state <= DYB_PG;
--                         ELSIF Instruct = PNVDLR  AND WEL = '1' THEN
--                             next_state <= NVDLR_PG;
--                         ELSE
--                             next_state <= IDLE;
--                         END IF;
--                     END IF;
--                     IF falling_edge(write) AND RdPswdProtMode = '1' AND
--                        WIP = '0' THEN
--                         IF Instruct = PASSU THEN
--                             next_state <= PASS_UNLOCK;
--                         END IF;
--                     END IF;

--                 WHEN  AUTOBOOT =>
--                     IF rising_edge(CSNeg_ipd) THEN
--                         next_state <= IDLE;
--                     END IF;

--                 WHEN WRITE_SR       =>
--                     IF rising_edge(WDONE) THEN
--                         next_state <= IDLE;
--                     END IF;

--                 WHEN PAGE_PG        =>
--                     IF  PRGSUSP_out'EVENT AND PRGSUSP_out = '1' THEN
--                         next_state <= PG_SUSP;
--                     ELSIF rising_edge(PDONE) THEN
--                         next_state <= IDLE;
--                     END IF;

--                 WHEN PG_SUSP      =>
--                     IF falling_edge(write) THEN
--                         IF Instruct = PGRS THEN
--                             next_state <=  PAGE_PG;
--                         ELSE
--                             next_state <= PG_SUSP;
--                         END IF;
--                     ELSE
--                         next_state <= PG_SUSP;
--                     END IF;

--                 WHEN OTP_PG         =>
--                     IF rising_edge(PDONE) THEN
--                         next_state <= IDLE;
--                     END IF;

--                 WHEN BULK_ERS       =>
--                     IF rising_edge(EDONE) THEN
--                         next_state <= IDLE;
--                     END IF;

--                 WHEN SECTOR_ERS     =>
--                     IF ERSSUSP_out'EVENT AND ERSSUSP_out = '1' THEN
--                         next_state <= ERS_SUSP;
--                     ELSIF rising_edge(EDONE) THEN
--                         next_state <= IDLE;
--                     END IF;

--                 WHEN ERS_SUSP      =>
--                     IF falling_edge(write) THEN
--                         IF (Instruct = PP OR Instruct = QPP OR
--                             Instruct = PP4 OR Instruct = QPP4) AND
--                             WEL = '1' THEN
--                             IF SectorSuspend /= (Address/(SecSize+1)) THEN
--                                 pgm_page := Address / PageSize;
--                                 IF PPB_bits(Address/SecSize)='1' AND
--                                    DYB_bits(Address/SecSize)='1' THEN
--                                     next_state <=  ERS_SUSP_PG;
--                                 END IF;
--                             END IF;
--                         ELSIF (Instruct = DYBWR AND WEL = '1') THEN
--                             next_state <=  DYB_PG;
--                         ELSIF Instruct = ERRS THEN
--                             next_state <=  SECTOR_ERS;
--                         END IF;
--                     ELSE
--                         next_state <= ERS_SUSP;
--                     END IF;

--                 WHEN ERS_SUSP_PG         =>
--                     IF PRGSUSP_out'EVENT AND PRGSUSP_out = '1' THEN
--                         next_state <= ERS_SUSP_PG_SUSP;
--                     ELSIF rising_edge(PDONE) THEN
--                         next_state <= ERS_SUSP;
--                     END IF;

--                 WHEN ERS_SUSP_PG_SUSP      =>
--                     IF falling_edge(write) THEN
--                         IF Instruct = PGRS THEN
--                             next_state <=  ERS_SUSP_PG;
--                         ELSE
--                             next_state <= ERS_SUSP_PG_SUSP;
--                         END IF;
--                     ELSE
--                         next_state <= ERS_SUSP_PG_SUSP;
--                     END IF;

--                 WHEN PASS_PG        =>
--                     IF rising_edge(PDONE) THEN
--                         next_state <= IDLE;
--                     END IF;

--                 WHEN PASS_UNLOCK    =>
--                     IF falling_edge(PASSULCK_in) THEN
--                         next_state <= IDLE;
--                     END IF;

--                 WHEN PPB_PG         =>
--                     IF rising_edge(PDONE) THEN
--                         next_state <= IDLE;
--                     END IF;

--                 WHEN PPB_ERS        =>
--                     IF falling_edge(PPBERASE_in) THEN
--                         next_state <= IDLE;
--                     END IF;

--                 WHEN AUTOBOOT_PG    =>
--                     IF rising_edge(PDONE) THEN
--                         next_state <= IDLE;
--                     END IF;

--                 WHEN PLB_PG         =>
--                     IF rising_edge(PDONE) THEN
--                         next_state <= IDLE;
--                     END IF;

--                 WHEN DYB_PG         =>
--                     IF rising_edge(PDONE) THEN
--                         IF ES = '1' THEN
--                             next_state <= ERS_SUSP;
--                         ELSE
--                             next_state <= IDLE;
--                         END IF;
--                     END IF;

--                 WHEN ASP_PG         =>
--                     IF rising_edge(PDONE) THEN
--                         next_state <= IDLE;
--                     END IF;

--                 WHEN NVDLR_PG         =>
--                     IF rising_edge(PDONE) THEN
--                         next_state <= IDLE;
--                     END IF;

--                 END CASE;
--             END IF;
--         END IF;

--     END PROCESS StateGen;

--     ReadEnable: PROCESS (read_out)
--     BEGIN
--         oe_z <= rising_edge(read_out) AND PoweredUp = '1';

--         IF read_out'EVENT AND read_out = '0' AND PoweredUp = '1' THEN
--             oe   <= TRUE, FALSE AFTER 1 ns;
--         END IF;
--     END PROCESS ReadEnable;
--     ---------------------------------------------------------------------------
--     --FSM Output generation and general funcionality
--     ---------------------------------------------------------------------------
--     Functional : PROCESS(write,current_state,start_autoboot,CSNeg_ipd,
--                          HOLDNeg_pullup, Instruct, Address, WByte,change_addr,
--                          PoweredUp, WPNeg_pullup, WDONE, PDONE, EDONE,
--                          PRGSUSP_out,ERSSUSP_out, PASSACC_out, oe, oe_z)

--         VARIABLE WData          : WByteType:= (OTHERS => MaxData);

--         VARIABLE AddrLo         : NATURAL;
--         VARIABLE AddrHi         : NATURAL;
--         VARIABLE Addr           : NATURAL;
--         VARIABLE Addr_tmp       : NATURAL;

--         VARIABLE data_out       : std_logic_vector(7 downto 0);
--         VARIABLE ident_out      : std_logic_vector(647 downto 0);

--         VARIABLE ExtendedID     : NATURAL;

--         VARIABLE old_bit        : std_logic_vector(7 downto 0);
--         VARIABLE new_bit        : std_logic_vector(7 downto 0);
--         VARIABLE old_int        : INTEGER RANGE -1 to MaxData;
--         VARIABLE new_int        : INTEGER RANGE -1 to MaxData;
--         VARIABLE old_pass       : std_logic_vector(63 downto 0);
--         VARIABLE new_pass       : std_logic_vector(63 downto 0);
--         VARIABLE wr_cnt         : NATURAL RANGE 0 TO 511;
--         --Data Learning Pattern Enable
--         VARIABLE dlp_act        : BOOLEAN   := FALSE;

--         VARIABLE sect           : NATURAL RANGE 0 TO SecNum;
--         VARIABLE cnt            : NATURAL RANGE 0 TO 512 := 0;
--         VARIABLE mem_data       : INTEGER;

--     BEGIN

--         -----------------------------------------------------------------------
--         -- Functionality Section
--         -----------------------------------------------------------------------

--         IF Instruct'EVENT THEN
--             read_cnt := 0;
--             read_addr_tmp := 0;
--             byte_cnt := 1;
--             fast_rd  <= true;
--             dual     <= false;
--             rd       <= false;
--             any_read <= false;
--         END IF;

--         IF PASSACC_out'EVENT AND PASSACC_out = '1' THEN
--             WIP := '0';
--             PASSACC_in <= '0';
--         END IF;

--         IF rising_edge(PoweredUp) THEN
--             --the default condition after power-up
--             --The Bank Address Register is loaded to all zeroes
--             Bank_Addr_reg := (others => '0');
--             --The Configuration Register FREEZE bit is cleared.
--             FREEZE := '0';
--             --The WEL bit is cleared.
--             WEL := '0';
--             --When BPNV is set to '1'. the BP2-0 bits in Status Register are
--             --volatile and will be reset binary 111 after power-on reset
--             IF BPNV = '1' AND FREEZE = '0' THEN
--                 BP0 := '1';
--                 BP1 := '1';
--                 BP2 := '1';
--                 BP_bits := BP2 & BP1 & BP0;
--                 change_BP <= '1', '0' AFTER 1 ns;
--             END IF;
--             --As shipped from the factory, all devices default ASP to the
--             --Persistent Protection mode, with all sectors unprotected,
--             --when power is applied. The device programmer or host system must
--             --then choose which sector protection method to use.
--             --For Persistent Protection mode, PPBLOCK defaults to "1"
--             PPB_LOCK := '1';
--             PPB_LOCK_temp <= '1';
--             DYB_bits := (OTHERS => '1');

--         END IF;

--         IF falling_edge(write) AND Instruct = RESET THEN
--             --The Configuration Register is set for Address mode
--             Bank_Addr_reg := (others => '0');
--             --P_ERR bit is cleared
--             P_ERR := '0';
--             --E_ERR bit is cleared
--             E_ERR := '0';
--             --The WEL bit is cleared.
--             WEL   := '0';
--             --The WIP bit is cleared.
--             WIP   := '0';
--             --The ES bit is cleared.
--             ES    := '0';
--             --The PS bit is cleared.
--             PS    := '0';

--             DummyBytes_act := '0';
--             --When BPNV is set to '1'. the BP2-0 bits in Status
--             --Register are volatile and will be reseted after
--             --reset command
--             IF BPNV = '1' AND FREEZE = '0' THEN
--                 BP0 := '1';
--                 BP1 := '1';
--                 BP2 := '1';
--                 BP_bits := BP2 & BP1 & BP0;
--                 change_BP <= '1', '0' AFTER 1 ns;
--             END IF;
--         END IF;

--         IF change_addr'EVENT THEN
--             read_addr := Address;
--         END IF;

--         CASE current_state IS
--             WHEN IDLE          =>
--                 fast_rd <= true;
--                 rd      <= false;
--                 dual    <= false;
--                 ddr     <= false;
--                 IF falling_edge(write) AND RdPswdProtMode = '1' THEN
--                     IF Instruct = PASSU THEN
--                         IF WIP = '0' THEN
--                             PASSULCK_in <= '1';
--                             IF LongTimming = TRUE THEN
--                                  PASSULCK_out <= '0', '1' AFTER (tdevice_PASSULCK);
--                               ELSE
--                                  PASSULCK_out <= '0', '1' AFTER (tdevice_PASSULCK/10);
--                             END IF;
--                         ELSE
--                             REPORT "The PASSU command cannot be accepted"&
--                                    " any faster than once every 100us"
--                             SEVERITY WARNING;
--                         END IF;
--                     ELSIF Instruct = CLSR THEN
--                     --The Clear Status Register Command resets bit SR1[5]
--                     --(Erase Fail Flag) and bit SR1[6] (Program Fail Flag)
--                        E_ERR := '0';
--                        P_ERR := '0';
--                     END IF;
--                 END IF;
--                 IF falling_edge(write) AND RdPswdProtMode = '0' THEN
--                     read_cnt := 0;
--                     byte_cnt    := 1;
--                     IF Instruct = WREN THEN
--                         WEL := '1';
--                     ELSIF Instruct = WRDI THEN
--                         WEL := '0';
--                     ELSIF Instruct = WRR AND WEL = '1' AND BAR_ACC = '0' THEN
--                         IF (not(SRWD = '1' AND WPNeg_pullup = '0') AND
--                                 QUAD ='0') OR QUAD ='1' THEN
--                             -- can not execute if Hardware Protection Mode
--                             -- is entered or if WEL bit is zero

--                             IF ((TBPROT='1' AND Config_reg1_in(5)='0') OR
--                                 (BPNV  ='1' AND Config_reg1_in(3)='0')) AND
--                                  cfg_write = '1' THEN
--                                 P_ERR := '1';
--                             ELSE
--                                 WSTART <= '1', '0' AFTER 5 ns;
--                                 WIP := '1';
--                             END IF;
--                         ELSE
--                             WEL := '0';
--                         END IF;
--                     ELSIF (Instruct = PP OR Instruct = PP4) AND WEL = '1' THEN
--                         sect := Address/(SecSize+1);
--                         pgm_page := Address/PageSize;
--                         IF (Sec_Prot(sect) = '0' AND
--                            PPB_bits(sect)='1' AND DYB_bits(sect)='1') THEN
--                             PSTART <= '1', '0' AFTER 5 ns;
--                             PGSUSP  <= '0';
--                             PGRES   <= '0';
--                             INITIAL_CONFIG <= '1';
--                             WIP := '1';
--                             SA <= sect;
--                             Addr := Address;
--                             Addr_tmp := Address;
--                             wr_cnt := Byte_number;
--                             FOR I IN wr_cnt DOWNTO 0 LOOP
--                                 IF Viol /= '0' THEN
--                                     WData(i) := -1;
--                                 ELSE
--                                     WData(i) := WByte(i);
--                                 END IF;
--                             END LOOP;
--                         ELSE
--                             P_ERR := '1';
--                             WEL   := '0';
--                         END IF;
--                     ELSIF (Instruct = QPP OR Instruct = QPP4) AND WEL = '1' THEN
--                         sect := Address/(SecSize+1);
--                         pgm_page := Address/PageSize;
--                         IF (Sec_Prot(sect)= '0' AND
--                            PPB_bits(sect)='1' AND DYB_bits(sect)='1') THEN
--                             PSTART <= '1', '0' AFTER 5 ns;
--                             PGSUSP  <= '0';
--                             PGRES   <= '0';
--                             INITIAL_CONFIG <= '1';
--                             WIP := '1';
--                             SA <= sect;
--                             Addr := Address;
--                             Addr_tmp := Address;
--                             wr_cnt := Byte_number;
--                             FOR I IN wr_cnt DOWNTO 0 LOOP
--                                 IF Viol /= '0' THEN
--                                     WData(i) := -1;
--                                 ELSE
--                                     WData(i) := WByte(i);
--                                 END IF;
--                             END LOOP;
--                         ELSE
--                             P_ERR := '1';
--                             WEL   := '0';
--                         END IF;
--                     ELSIF Instruct = OTPP AND WEL = '1' THEN
--                     -- As long as the FREEZE bit remains cleared to a logic '0'
--                     --the OTP address space is programmable.
--                         IF FREEZE = '0' THEN
--                             IF ((((Address >= 16#0010# AND Address <= 16#13#)OR
--                                 (Address >= 16#0020# AND Address <= 16#FF#))
--                             AND LOCK_BYTE1(Address/32) = '1') OR
--                             ((Address >= 16#100# AND Address <= 16#1FF#)
--                             AND LOCK_BYTE2((Address-16#100#)/32) = '1') OR
--                             ((Address >= 16#200# AND Address <= 16#2FF#)
--                             AND LOCK_BYTE3((Address-16#200#)/32) = '1') OR
--                             ((Address >= 16#300# AND Address <= 16#3FF#)
--                             AND LOCK_BYTE4((Address-16#300#)/32) = '1')) AND
--                             (Address + Byte_number <= OTPHiAddr) THEN
--                                 PSTART <= '1', '0' AFTER 1 ns;
--                                 WIP := '1';
--                                 Addr := Address;
--                                 Addr_tmp := Address;
--                                 wr_cnt := Byte_number;
--                                 FOR I IN wr_cnt DOWNTO 0 LOOP
--                                     IF Viol /= '0' THEN
--                                         WData(i) := -1;
--                                     ELSE
--                                         WData(i) := WByte(i);
--                                     END IF;
--                                 END LOOP;
--                             ELSIF (Address < 16#0010# OR (Address > 16#0013# AND
--                                 Address < 16#0020#) OR Address > 16#3FF# ) THEN
--                                 P_ERR := '1';
--                                 WEL   := '0';
--                                 IF Address < 16#0020# THEN
--                                     ASSERT false
--                                         REPORT "Given  address is in" &
--                                             "reserved address range"
--                                         SEVERITY warning;
--                                 ELSIF Address > 16#3FF# THEN
--                                     ASSERT false
--                                         REPORT "Given  address is out of" &
--                                             "OTP address range"
--                                         SEVERITY warning;
--                                 END IF;
--                             ELSE
--                                 WEL   := '0';
--                                 P_ERR := '1';
--                             END IF;
--                         ELSE
--                             WEL   := '0';
--                             P_ERR := '1';
--                         END IF;
--                     ELSIF (Instruct = SE OR Instruct = SE4) AND WEL = '1' THEN
--                         sect := Address/(SecSize+1);
--                         SectorSuspend <= sect;
--                         IF (Sec_Prot(sect) = '0' AND
--                             PPB_bits(sect)='1' AND DYB_bits(sect)='1') THEN
--                             ESTART <= '1', '0' AFTER 5 ns;
--                             ESUSP  <= '0';
--                             ERES   <= '0';
--                             INITIAL_CONFIG <= '1';
--                             WIP  := '1';
--                             Addr := Address;
--                         ELSE
--                             E_ERR := '1';
--                             WEL   := '0';
--                         END IF;
--                     ELSIF (Instruct = P4E OR Instruct = P4E4) AND WEL = '1' THEN
--                         WEL := '0';
--                     ELSIF Instruct = BE AND WEL = '1' THEN
--                         IF (BP0='0' AND BP1='0' AND BP2='0') THEN
--                             ESTART <= '1', '0' AFTER 5 ns;
--                             ESUSP  <= '0';
--                             ERES   <= '0';
--                             INITIAL_CONFIG <= '1';
--                             WIP := '1';
--                         ELSE
--                         --The Bulk Erase command will not set E_ERR if a
--                         --protected sector is found during the command
--                         --execution.
--                             WEL   := '0';
--                         END IF;
--                     ELSIF Instruct = PASSP AND WEL = '1' THEN
--                         IF not(PWDMLB='0' AND PSTMLB='1') THEN
--                             PSTART <= '1', '0' AFTER 5 ns;
--                             WIP := '1';
--                         ELSE
--                             REPORT "Password programming is not allowed" &
--                                    " in Password Protection Mode."
--                             SEVERITY warning;
--                         END IF;
--                     ELSIF Instruct = PASSU  AND WEL= '1' AND WIP= '0' THEN
--                         PASSULCK_in <= '1';
--                         IF LongTimming = TRUE THEN
--                                  PASSULCK_out <= '0', '1' AFTER (tdevice_PASSULCK);
--                               ELSE
--                                  PASSULCK_out <= '0', '1' AFTER (tdevice_PASSULCK/10);
--                             END IF;
--                     ELSIF Instruct = BRWR THEN
--                         Bank_Addr_reg(7) := Bank_Addr_reg_in(7);
--                         Bank_Addr_reg(1) := Bank_Addr_reg_in(1);
--                         Bank_Addr_reg(0) := Bank_Addr_reg_in(0);
--                     ELSIF Instruct = WRR AND BAR_ACC = '1' THEN
--                         IF (P_ERR = '0' AND E_ERR = '0') THEN
--                             Bank_Addr_reg(1) := Bank_Addr_reg_in(1);
--                             Bank_Addr_reg(0) := Bank_Addr_reg_in(0);
--                         END IF;
--                     ELSIF Instruct = ASPP AND WEL = '1' THEN
--                         IF not(ASPOTPFLAG) THEN
--                             PSTART <= '1', '0' AFTER 5 ns;
--                             WIP := '1';
--                         ELSE
--                             WEL := '0';
--                             P_ERR := '1';
--                             REPORT "Once the Protection Mode is selected," &
--                                    "no further changes to the ASP register" &
--                                    "is allowed."
--                             SEVERITY warning;
--                         END IF;
--                     ELSIF Instruct = ABWR AND WEL = '1' THEN
--                         PSTART <= '1', '0' AFTER 5 ns;
--                         WIP := '1';
--                     ELSIF Instruct = PPBP AND WEL = '1' THEN
--                         sect := Address/(SecSize+1);
--                         Addr := Address;
--                         pgm_page := Address/PageSize;

--                         PSTART <= '1', '0' AFTER 5 ns;
--                         WIP := '1';
--                     ELSIF Instruct = PPBERS AND WEL = '1' THEN
--                         IF PPBOTP = '1' THEN
--                             PPBERASE_in <= '1';
--                             IF LongTimming = TRUE THEN
--                                  PPBERASE_out <= '0', '1' AFTER (tdevice_PPBERASE);
--                               ELSE
--                                  PPBERASE_out <= '0', '1' AFTER (tdevice_PPBERASE/100);
--                             END IF;
--                             WIP := '1';
--                         ELSE
--                             E_ERR := '1';
--                         END IF;
--                     ELSIF Instruct = PLBWR  AND WEL = '1' AND
--                         RdPswdProtEnable = '0' THEN
--                         PSTART <= '1', '0' AFTER 5 ns;
--                         WIP := '1';
--                     ELSIF Instruct = DYBWR  AND WEL = '1' THEN
--                         sect := Address/(SecSize+1);
--                         Addr := Address;
--                         pgm_page := Address/PageSize;
--                         PSTART <= '1', '0' AFTER 5 ns;
--                         WIP := '1';
--                     ELSIF Instruct = PNVDLR  AND WEL = '1' THEN
--                         PSTART <= '1', '0' AFTER 5 ns;
--                         WIP := '1';
--                     ELSIF Instruct = WVDLR  AND WEL = '1' THEN
--                         VDLR_reg := VDLR_reg_in;
--                         WEL := '0';
--                     ELSIF Instruct = CLSR THEN
--                     --The Clear Status Register Command resets bit SR1[5]
--                     --(Erase Fail Flag) and bit SR1[6] (Program Fail Flag)
--                        E_ERR := '0';
--                        P_ERR := '0';
--                     END IF;

--                     IF Instruct = BRAC AND P_ERR = '0' AND E_ERR = '0' THEN
--                         BAR_ACC <= '1';
--                     ELSE
--                         BAR_ACC <= '0';
--                     END IF;
--                 ELSIF oe_z THEN
--                     IF Instruct = READ OR Instruct = RD4 OR
--                        Instruct = RES  OR Instruct = DLPRD THEN
--                         fast_rd <= false;
--                         rd      <= true;
--                         dual    <= false;
--                         ddr     <= false;
--                     ELSIF Instruct = DDRFR OR Instruct = DDRFR4 THEN
--                         fast_rd <= false;
--                         rd      <= false;
--                         dual    <= false;
--                         ddr     <= true;
--                     ELSIF Instruct = DDRDIOR OR Instruct = DDRDIOR4 THEN
--                         fast_rd <= false;
--                         rd      <= false;
--                         dual    <= true;
--                         ddr     <= true;
--                     ELSIF Instruct = DOR OR Instruct=DOR4 OR
--                           Instruct = DIOR OR Instruct = DIOR4 OR
--                           Instruct = QOR OR Instruct=QOR4 OR
--                           Instruct = QIOR OR Instruct= QIOR4 THEN
--                         fast_rd <= false;
--                         rd      <= false;
--                         dual    <= true;
--                         ddr     <= false;
--                     ELSIF Instruct = DDRQIOR OR Instruct = DDRQIOR4 THEN
--                         fast_rd <= false;
--                         rd      <= false;
--                         dual    <= true;
--                         ddr     <= true;
--                     ELSE
--                         fast_rd <= true;
--                         rd      <= false;
--                         dual    <= false;
--                         ddr     <= false;
--                     END IF;
--                     HOLDNegOut_zd <= 'Z';
--                     WPNegOut_zd   <= 'Z';
--                     SOOut_zd      <= 'Z';
--                     SIOut_zd      <= 'Z';
--                 ELSIF oe THEN
--                     any_read <= true;
--                     IF Instruct = RDSR THEN
--                         --Read Status Register 1
--                         SOOut_zd <= Status_reg1(7-read_cnt);
--                         read_cnt := read_cnt + 1;
--                         IF read_cnt = 8 THEN
--                             read_cnt := 0;
--                         END IF;
--                     ELSIF Instruct = RDSR2 THEN
--                         --Read Status Register 2
--                         SOOut_zd <= Status_reg2(7-read_cnt);
--                         read_cnt := read_cnt + 1;
--                         IF read_cnt = 8 THEN
--                             read_cnt := 0;
--                         END IF;
--                     ELSIF Instruct = RDCR THEN
--                         --Read Configuration Register 1
--                         SOOut_zd <= Config_reg1(7-read_cnt);
--                         read_cnt := read_cnt + 1;
--                         IF read_cnt = 8 THEN
--                             read_cnt := 0;
--                         END IF;
--                     ELSIF Instruct = READ OR Instruct = RD4 OR
--                           Instruct = FSTRD OR Instruct = FSTRD4 OR
--                           Instruct = DDRFR OR Instruct = DDRFR4 THEN
--                         --Read Memory array
--                         IF Instruct = READ OR Instruct = RD4 THEN
--                             fast_rd <= false;
--                             rd      <= true;
--                             dual    <= false;
--                             ddr     <= false;
--                         ELSIF Instruct = DDRFR OR Instruct = DDRFR4 THEN
--                             fast_rd <= false;
--                             rd      <= false;
--                             dual    <= false;
--                             ddr     <= true;
--                         ELSE
--                             fast_rd <= true;
--                             rd      <= false;
--                             dual    <= false;
--                             ddr     <= false;
--                         END IF;
--                         IF bus_cycle_state = DUMMY_BYTES OR
--                            DummyBytes_act='1' THEN
--                             DummyBytes_act := '0';
--                             IF (Instruct = DDRFR OR Instruct = DDRFR4) THEN
--                                 data_out := VDLR_reg;
--                                 SOOut_zd <= data_out(7-read_cnt);
--                                 read_cnt := read_cnt + 1;
--                                 IF read_cnt = 8 THEN
--                                     read_cnt := 0;
--                                 END IF;
--                             END IF;
--                         ELSE
--                             SecAddr := ReturnSectorID(read_addr);
--                             Sec_addr := read_addr - SecAddr*(SecSize+1);
--                             SecAddr := ReturnSectorIDRdPswdMd(TBPROT);
--                             IF RdPswdProtMode = '0' THEN
--                                 SecAddr := ReturnSectorID(read_addr);
--                             END IF;
--                             READ_DATA(SecAddr,Sec_addr,mem_data);
--                             IF mem_data /= -1 THEN
--                                 data_out := to_slv((mem_data),8);
--                                 SOOut_zd <= data_out(7-read_cnt);
--                             ELSE
--                                 SOOut_zd <= 'U';
--                             END IF;
--                             read_cnt := read_cnt + 1;
--                             IF read_cnt = 8 THEN
--                                 read_cnt := 0;
--                                 IF read_addr = AddrRANGE THEN
--                                     read_addr := 0;
--                                 ELSE
--                                     read_addr := read_addr + 1;
--                                 END IF;
--                             END IF;
--                         END IF;
--                     ELSIF Instruct = DOR OR Instruct=DOR4 OR
--                           Instruct = DIOR OR Instruct = DIOR4 OR
--                           Instruct = DDRDIOR OR Instruct = DDRDIOR4 THEN
--                         --Read Memory array
--                         IF Instruct = DDRDIOR OR Instruct = DDRDIOR4 THEN
--                             fast_rd <= false;
--                             rd      <= false;
--                             dual    <= true;
--                             ddr     <= true;
--                         ELSE
--                             fast_rd <= false;
--                             rd      <= false;
--                             dual    <= true;
--                             ddr     <= false;
--                         END IF;
--                         IF bus_cycle_state = DUMMY_BYTES
--                         OR DummyBytes_act = '1' THEN
--                             DummyBytes_act := '0';
--                             IF (Instruct = DDRDIOR OR Instruct = DDRDIOR4) THEN
--                                 data_out := VDLR_reg;
--                                 SOOut_zd <= data_out(7-read_cnt);
--                                 SIOut_zd <= data_out(7-read_cnt);
--                                 read_cnt := read_cnt + 1;
--                                 IF read_cnt = 8 THEN
--                                     read_cnt := 0;
--                                 END IF;
--                             END IF;
--                         ELSE
--                             SecAddr := ReturnSectorID(read_addr);
--                             Sec_addr := read_addr - SecAddr*(SecSize+1);
--                             SecAddr := ReturnSectorIDRdPswdMd(TBPROT);
--                             IF RdPswdProtMode = '0' THEN
--                                 SecAddr := ReturnSectorID(read_addr);
--                             END IF;
--                             READ_DATA(SecAddr,Sec_addr,mem_data);
--                             data_out := to_slv((mem_data),8);
--                             SOOut_zd <= data_out(7-2*read_cnt);
--                             SIOut_zd <= data_out(6-2*read_cnt);
--                             read_cnt := read_cnt + 1;
--                             IF read_cnt = 4 THEN
--                                 read_cnt := 0;
--                                 IF read_addr = AddrRANGE THEN
--                                     read_addr := 0;
--                                 ELSE
--                                     read_addr := read_addr + 1;
--                                 END IF;
--                             END IF;
--                         END IF;
--                     ELSIF (Instruct = QOR OR Instruct=QOR4 OR
--                            Instruct = QIOR OR Instruct= QIOR4 OR
--                            Instruct = DDRQIOR OR Instruct= DDRQIOR4)
--                            AND QUAD = '1' THEN
--                         --Read Memory array
--                         IF Instruct = DDRQIOR OR Instruct = DDRQIOR4 THEN
--                             fast_rd <= false;
--                             rd      <= false;
--                             dual    <= true;
--                             ddr     <= true;
--                         ELSE
--                             fast_rd <= false;
--                             rd      <= false;
--                             dual    <= true;
--                             ddr     <= false;
--                         END IF;

--                         IF bus_cycle_state=DUMMY_BYTES OR DummyBytes_act='1' THEN
--                             DummyBytes_act := '0';
--                             IF Instruct = DDRQIOR OR Instruct = DDRQIOR4 THEN
--                                 data_out := VDLR_reg;
--                                 HOLDNegOut_zd <= data_out(7-read_cnt);
--                                 WPNegOut_zd   <= data_out(7-read_cnt);
--                                 SOOut_zd      <= data_out(7-read_cnt);
--                                 SIOut_zd      <= data_out(7-read_cnt);
--                                 read_cnt := read_cnt + 1;
--                                 IF read_cnt = 8 THEN
--                                     read_cnt := 0;
--                                 END IF;
--                             END IF;
--                         ELSE
--                             SecAddr := ReturnSectorID(read_addr);
--                             Sec_addr := read_addr - SecAddr*(SecSize+1);
--                             SecAddr := ReturnSectorIDRdPswdMd(TBPROT);
--                             IF RdPswdProtMode = '0' THEN
--                                 SecAddr := ReturnSectorID(read_addr);
--                             END IF;
--                             READ_DATA(SecAddr,Sec_addr,mem_data);
--                             data_out := to_slv((mem_data),8);
--                             HOLDNegOut_zd   <= data_out(7-4*read_cnt);
--                             WPNegOut_zd     <= data_out(6-4*read_cnt);
--                             SOOut_zd        <= data_out(5-4*read_cnt);
--                             SIOut_zd        <= data_out(4-4*read_cnt);
--                             read_cnt := read_cnt + 1;
--                             IF read_cnt = 2 THEN
--                                 read_cnt := 0;
--                                 IF read_addr = AddrRANGE THEN
--                                     read_addr := 0;
--                                 ELSE
--                                     read_addr := read_addr + 1;
--                                 END IF;
--                             END IF;
--                         END IF;
--                     ELSIF Instruct = OTPR  THEN
--                         IF (read_addr>=OTPLoAddr) AND
--                            (read_addr<=OTPHiAddr) AND RdPswdProtMode = '0' THEN
--                             --Read OTP Memory array
--                             fast_rd <= true;
--                             rd      <= false;
--                             data_out := to_slv(OTPMem(read_addr),8);
--                             SOOut_zd <= data_out(7-read_cnt);
--                             read_cnt := read_cnt + 1;
--                             IF read_cnt = 8 THEN
--                                 read_cnt := 0;
--                                 read_addr := read_addr + 1;
--                             END IF;
--                         ELSIF (read_addr > OTPHiAddr)
--                         OR RdPswdProtMode = '1' THEN
--                         --OTP Read operation will not wrap to the
--                         --starting address after the OTP address is at
--                         --its maximum or Read Password Protection Mode
--                         --is selected; instead, the data beyond the
--                         --maximum OTP address will be undefined.
--                             SOOut_zd <= 'U';
--                             read_cnt := read_cnt + 1;
--                             IF read_cnt = 8 THEN
--                                 read_cnt := 0;
--                             END IF;
--                         END IF;
--                     ELSIF Instruct = REMS THEN
--                         --Read Manufacturer and Device ID
--                         IF read_addr MOD 2 = 0 THEN
--                             data_out := to_slv(Manuf_ID,8);
--                             SOOut_zd <= data_out(7 - read_cnt);
--                             read_cnt := read_cnt + 1;
--                             IF read_cnt = 8 THEN
--                                 read_cnt := 0;
--                                 read_addr := read_addr + 1;
--                             END IF;
--                         ELSE
--                             data_out := to_slv(DeviceID,8);
--                             SOOut_zd <= data_out(7 - read_cnt);
--                             read_cnt := read_cnt + 1;
--                             IF read_cnt = 8 THEN
--                                 read_cnt := 0;
--                                 read_addr := 0;
--                             END IF;
--                         END IF;

--                     ELSIF Instruct = RDID THEN
--                         IF (read_addr_tmp <= (SFDPHiAddr - 16#1000#)) THEN
--                             data_out := to_slv(SFDP_array(16#1000# + read_addr_tmp), 8);
--                             IF (read_addr_tmp <= IDCFILength) THEN
--                                 SOOut_zd      <= data_out(7 - read_cnt);
--                                 read_cnt := read_cnt + 1;
--                                 IF read_cnt = 8 THEN
--                                     read_cnt := 0;
--                                     read_addr_tmp := read_addr_tmp + 1;
--                                 END IF;
--                             ELSE
--                                 SOOut_zd      <= 'X';
--                             END IF;
--                         ELSE
--                             SOOut_zd      <= '1';
--                         END IF;

--                     ELSIF Instruct = RSFDP THEN
--                         data_out := to_slv(SFDP_array(read_addr + read_addr_tmp),8);
--                         IF (read_addr + read_addr_tmp) <= SFDPHiAddr THEN
--                             SOOut_zd      <= data_out(7 - read_cnt);
--                             read_cnt := read_cnt + 1;
--                             IF read_cnt = 8 THEN
--                                 read_cnt := 0;
--                                 read_addr_tmp := read_addr_tmp + 1;
--                             END IF;
--                         ELSE
--                             SOOut_zd      <= 'X';
--                         END IF;
--                     ELSIF Instruct = RES THEN
--                         fast_rd <= false;
--                         rd      <= true;
--                         dual    <= false;
--                         ddr     <= false;
--                         data_out := to_slv(ESignature,8);
--                         SOOut_zd <= data_out(7-read_cnt);
--                         read_cnt := read_cnt + 1;
--                         IF read_cnt = 8 THEN
--                             read_cnt := 0;
--                         END IF;
--                     ELSIF Instruct = DLPRD THEN
--                         fast_rd <= false;
--                         rd      <= true;
--                         dual    <= false;
--                         ddr     <= false;
--                         SOOut_zd <= VDLR_reg(7-read_cnt);
--                         read_cnt := read_cnt + 1;
--                         IF read_cnt = 8 THEN
--                             read_cnt := 0;
--                         END IF;
--                     ELSIF Instruct = ABRD THEN
--                         --Read AutoBoot register
--                         fast_rd <= true;
--                         rd      <= false;
--                         dual    <= false;
--                         ddr     <= false;
--                         SOOut_zd <= AutoBoot_reg_in(31-read_cnt);
--                         read_cnt := read_cnt + 1;
--                         IF read_cnt = 32 THEN
--                             read_cnt := 0;
--                         END IF;
--                     ELSIF Instruct = BRRD THEN
--                         --Read Bank Address Register
--                         fast_rd <= true;
--                         rd      <= false;
--                         dual    <= false;
--                         ddr     <= false;
--                         SOOut_zd <= Bank_Addr_reg(7-read_cnt);
--                         read_cnt := read_cnt + 1;
--                         IF read_cnt = 8 THEN
--                             read_cnt := 0;
--                         END IF;
--                     ELSIF Instruct = ASPRD THEN
--                         --Read ASP Register
--                         fast_rd <= true;
--                         rd      <= false;
--                         dual    <= false;
--                         ddr     <= false;
--                         SOOut_zd <= ASP_reg(15-read_cnt);
--                         read_cnt := read_cnt + 1;
--                         IF read_cnt = 16 THEN
--                             read_cnt := 0;
--                         END IF;
--                     ELSIF Instruct = PASSRD THEN
--                         --Read Password Register
--                         IF not (PWDMLB='0' AND PSTMLB='1') THEN
--                             fast_rd <= true;
--                             rd      <= false;
--                             dual    <= false;
--                             ddr     <= false;
--                             SOOut_zd <= Password_reg((8*byte_cnt-1)-read_cnt);
--                             read_cnt := read_cnt + 1;
--                             IF read_cnt = 8 THEN
--                                 read_cnt := 0;
--                                 byte_cnt := byte_cnt + 1;
--                                 IF byte_cnt = 9 THEN
--                                    byte_cnt := 1;
--                                 END IF;
--                             END IF;
--                         ELSE
--                             SOOut_zd <= 'U';
--                             read_cnt := read_cnt + 1;
--                             IF read_cnt = 64 THEN
--                                 read_cnt := 0;
--                             END IF;
--                             IF read_cnt = 0 THEN
--                                 ASSERT false
--                                 REPORT "Verification of password is not " &
--                                        " allowed in Password Protection Mode."
--                                 SEVERITY warning;
--                             END IF;
--                         END IF;
--                     ELSIF Instruct = PLBRD THEN
--                         --Read PPB Lock Register
--                         fast_rd <= true;
--                         rd      <= false;
--                         dual    <= false;
--                         ddr     <= false;
--                         SOOut_zd <= PPBL(7-read_cnt);
--                         read_cnt := read_cnt + 1;
--                         IF read_cnt = 8 THEN
--                             read_cnt := 0;
--                         END IF;
--                     ELSIF Instruct = DYBRD THEN
--                         --Read DYB Access Register
--                         sect := Address/(SecSize+1);
--                         fast_rd <= true;
--                         rd      <= false;
--                         dual    <= false;
--                         ddr     <= false;
--                         DYBAR(7 downto 0) := "UUUUUUUU";
--                         IF RdPswdProtMode = '0' THEN
--                             IF DYB_bits(sect) = '1' THEN
--                                 DYBAR(7 downto 0) := "11111111";
--                             ELSE
--                                 DYBAR(7 downto 0) := "00000000";
--                             END IF;
--                         END IF;
--                         SOOut_zd <= DYBAR(7-read_cnt);
--                         read_cnt := read_cnt + 1;
--                         IF read_cnt = 8 THEN
--                             read_cnt := 0;
--                         END IF;
--                     ELSIF Instruct = PPBRD THEN
--                         --Read PPB Access Register
--                         sect := Address/(SecSize+1);
--                         fast_rd <= true;
--                         rd      <= false;
--                         dual    <= false;
--                         ddr     <= false;
--                         PPBAR(7 downto 0) := "UUUUUUUU";
--                         IF RdPswdProtMode = '0' THEN
--                             IF PPB_bits(sect) = '1' THEN
--                                 PPBAR(7 downto 0) := "11111111";
--                             ELSE
--                                 PPBAR(7 downto 0) := "00000000";
--                             END IF;
--                         END IF;
--                         SOOut_zd <= PPBAR(7-read_cnt);
--                         read_cnt := read_cnt + 1;
--                         IF read_cnt = 8 THEN
--                             read_cnt := 0;
--                         END IF;
--                     END IF;
--                 END IF;

--             WHEN AUTOBOOT       =>
--                 IF start_autoboot = '1' THEN
--                     IF (oe) THEN
--                         any_read <= true;
--                         IF QUAD = '1' THEN
--                             IF ABSD > 0 THEN      --If ABSD > 0,
--                                 fast_rd <= false; --max SCK frequency is 104MHz
--                                 rd      <= false;
--                                 dual    <= true;
--                                 ddr     <= false;
--                             ELSE -- If ABSD = 0, max SCK frequency is 50 MHz
--                                 fast_rd <= false;
--                                 rd      <= true;
--                                 dual    <= false;
--                                 ddr     <= false;
--                             END IF;
--                             SecAddr := ReturnSectorID(read_addr);
--                             Sec_addr := read_addr - SecAddr*(SecSize+1);
--                             READ_DATA(SecAddr,Sec_addr,mem_data);
--                             data_out := to_slv((mem_data),8);
--                             HOLDNegOut_zd   <= data_out(7-4*read_cnt);
--                             WPNegOut_zd     <= data_out(6-4*read_cnt);
--                             SOOut_zd        <= data_out(5-4*read_cnt);
--                             SIOut_zd        <= data_out(4-4*read_cnt);
--                             read_cnt := read_cnt + 1;
--                             IF read_cnt = 2 THEN
--                                 read_cnt := 0;
--                                 read_addr := read_addr + 1;
--                             END IF;
--                         ELSE
--                             IF ABSD > 0 THEN      --If ABSD > 0,
--                                 fast_rd <= true; --max SCK frequency is 133MHz
--                                 rd      <= false;
--                                 dual    <= false;
--                                 ddr     <= false;
--                             ELSE -- If ABSD = 0, max SCK frequency is 50 MHz
--                                 fast_rd <= false;
--                                 rd      <= true;
--                                 dual    <= false;
--                                 ddr     <= false;
--                             END IF;
--                             SecAddr := ReturnSectorID(read_addr);
--                             Sec_addr := read_addr - SecAddr*(SecSize+1);
--                             READ_DATA(SecAddr,Sec_addr,mem_data);
--                             data_out := to_slv((mem_data),8);
--                             SOOut_zd <= data_out(7-read_cnt);
--                             read_cnt := read_cnt + 1;
--                             IF read_cnt = 8 THEN
--                                 read_cnt := 0;
--                                 read_addr := read_addr + 1;
--                             END IF;
--                        END IF;
--                     ELSIF oe_z THEN
--                        IF QUAD = '1' THEN
--                            IF ABSD > 0 THEN      --If ABSD > 0,
--                                fast_rd <= false; --max SCK frequency is 104MHz
--                                rd      <= false;
--                                dual    <= true;
--                                ddr     <= false;
--                            ELSE -- If ABSD = 0, max SCK frequency is 50 MHz
--                                fast_rd <= false;
--                                rd      <= true;
--                                dual    <= false;
--                                ddr     <= false;
--                            END IF;
--                        ELSE
--                            IF ABSD > 0 THEN      --If ABSD > 0,
--                                fast_rd <= true; --max SCK frequency is 133MHz
--                                rd      <= false;
--                                dual    <= false;
--                                ddr     <= false;
--                            ELSE -- If ABSD = 0, max SCK frequency is 50 MHz
--                                fast_rd <= false;
--                                rd      <= true;
--                                dual    <= false;
--                                ddr     <= false;
--                            END IF;
--                        END IF;
--                        HOLDNegOut_zd <= 'Z';
--                        WPNegOut_zd   <= 'Z';
--                        SOOut_zd      <= 'Z';
--                        SIOut_zd      <= 'Z';
--                     END IF;
--                 END IF;

--             WHEN WRITE_SR       =>
--                 fast_rd <= true;
--                 rd      <= false;
--                 dual    <= false;
--                 ddr     <= false;
--                 IF oe THEN
--                     any_read <= true;
--                     IF Instruct = RDSR THEN
--                         --Read Status Register 1
--                         SOOut_zd <= Status_reg1(7-read_cnt);
--                         read_cnt := read_cnt + 1;
--                         IF read_cnt = 8 THEN
--                             read_cnt := 0;
--                         END IF;
--                     ELSIF Instruct = RDSR2 THEN
--                         --Read Status Register 2
--                         SOOut_zd <= Status_reg2(7-read_cnt);
--                         read_cnt := read_cnt + 1;
--                         IF read_cnt = 8 THEN
--                             read_cnt := 0;
--                         END IF;
--                     ELSIF Instruct = RDCR THEN
--                         --Read Configuration Register 1
--                         SOOut_zd <= Config_reg1(7-read_cnt);
--                         read_cnt := read_cnt + 1;
--                         IF read_cnt = 8 THEN
--                             read_cnt := 0;
--                         END IF;
--                     END IF;
--                 ELSIF oe_z THEN
--                     HOLDNegOut_zd <= 'Z';
--                     WPNegOut_zd   <= 'Z';
--                     SOOut_zd      <= 'Z';
--                     SIOut_zd      <= 'Z';
--                 END IF;

--                 IF WDONE = '1' THEN
--                     WIP  := '0';
--                     WEL  := '0';
--                     SRWD := Status_reg1_in(7);--MSB first

-- --                    IF LOCK='0' THEN
--                         IF FREEZE='0' THEN
--                         --The Freeze Bit, when set to 1, locks the current
--                         --state of the BP2-0 bits in Status Register and
--                         --the TBPROT bit in the Config Register
--                         --As long as the FREEZE bit remains cleared to logic
--                         --'0', the other bits of the Configuration register
--                         --including FREEZE are writeable.
--                             BP2  := Status_reg1_in(4);
--                             BP1  := Status_reg1_in(3);
--                             BP0  := Status_reg1_in(2);

--                             BP_bits := BP2 & BP1 & BP0;

--                             IF TBPROT = '0' AND INITIAL_CONFIG = '0' THEN
--                                 TBPROT  := Config_reg1_in(5);
--                             END IF;

--                             change_BP <= '1', '0' AFTER 1 ns;

--                             LC1     := Config_reg1_in(7);
--                             LC0     := Config_reg1_in(6);
--                             QUAD    := Config_reg1_in(1);

--                             IF FREEZE = '0' THEN
--                                 FREEZE    := Config_reg1_in(0);
--                             END IF;

-- --                            IF WRLOCKENABLE AND LOCK = '0' THEN
-- --                                LOCK    := Config_reg1_in(4);
-- --                                WRLOCKENABLE <= false;
-- --                            END IF;

--                             IF BPNV = '0' THEN
--                                 BPNV    := Config_reg1_in(3);
--                             END IF;
--                         END IF;
-- --                    END IF;
--                 END IF;

--             WHEN PAGE_PG        =>
--                 fast_rd <= true;
--                 rd      <= false;
--                 dual    <= false;
--                 ddr     <= false;
--                 IF oe THEN
--                     any_read <= true;
--                     IF Instruct = RDSR THEN
--                         --Read Status Register 1
--                         SOOut_zd <= Status_reg1(7-read_cnt);
--                         read_cnt := read_cnt + 1;
--                         IF read_cnt = 8 THEN
--                             read_cnt := 0;
--                         END IF;
--                     ELSIF Instruct = RDSR2 THEN
--                         --Read Status Register 2
--                         SOOut_zd <= Status_reg2(7-read_cnt);
--                         read_cnt := read_cnt + 1;
--                         IF read_cnt = 8 THEN
--                             read_cnt := 0;
--                         END IF;
--                     ELSIF Instruct = RDCR THEN
--                         --Read Configuration Register 1
--                         SOOut_zd <= Config_reg1(7-read_cnt);
--                         read_cnt := read_cnt + 1;
--                         IF read_cnt = 8 THEN
--                             read_cnt := 0;
--                         END IF;
--                     END IF;
--                 ELSIF oe_z THEN
--                     HOLDNegOut_zd <= 'Z';
--                     WPNegOut_zd   <= 'Z';
--                     SOOut_zd      <= 'Z';
--                     SIOut_zd      <= 'Z';
--                 END IF;

--                 IF falling_edge(PDONE) THEN
-- --                    IF (Instruct = QPP OR Instruct = QPP4) THEN
-- --                        QPP_page(pgm_page) := '1';
-- --                    END IF;

--                     ADDRHILO_PG(AddrLo, AddrHi,Sec_addr,Page_addr,Addr);
--                     cnt := 0;

--                     FOR i IN 0 TO wr_cnt LOOP
--                         new_int := WData(i);
--                         SecAddr := ReturnSectorID(Addr);
--                         READ_DATA(SecAddr,
--                                 Sec_addr+i-cnt,
--                                 mem_data);
--                         old_int := mem_data;

--                         IF new_int > -1 THEN
--                             new_bit := to_slv(new_int,8);
--                             IF old_int > -1 THEN
--                                 old_bit := to_slv(old_int,8);
--                                 FOR j IN 0 TO 7 LOOP
--                                     IF old_bit(j) = '0' THEN
--                                         new_bit(j) := '0';
--                                     END IF;
--                                 END LOOP;
--                                 new_int := to_nat(new_bit);
--                             END IF;
--                             WData(i) := new_int;
--                         ELSE
--                             WData(i) := -1;
--                         END IF;

--                         write_mem(linked_list(SecAddr),
--                                 Sec_addr + i - cnt,
--                                 -1);
--                         IF (Sec_addr + i) = AddrHi THEN
--                             Sec_addr := AddrLo;
--                             cnt := i + 1;
--                         END IF;
--                     END LOOP;
--                     cnt :=0;
--                 END IF;

--                 IF PDONE = '1' THEN
--                     WIP := '0';
--                     WEL := '0';
--                     ADDRHILO_PG(AddrLo, AddrHi,Sec_addr,Page_addr,Addr);
--                     FOR i IN 0 TO wr_cnt LOOP
--                         SecAddr := ReturnSectorID(Addr);
--                         WRITE_DATA(SecAddr,
--                                    Sec_addr + i - cnt,
--                                    WData(i));
--                         IF (Sec_addr + i) = AddrHi THEN
--                             Sec_addr := AddrLo;
--                             cnt := i + 1;
--                         END IF;
--                     END LOOP;

--                 ELSIF Instruct = PGSP AND PRGSUSP_in = '0' THEN
--                     IF RES_TO_SUSP_MIN_TIME = '0' THEN
--                         PGSUSP <= '1', '0' AFTER 1 ns;
--                         PRGSUSP_in <= '1';
--                         IF LongTimming = TRUE THEN
--                             PRGSUSP_out <= '0', '1' AFTER (tdevice_PRGSUSP);
--                          ELSE
--                             PRGSUSP_out <= '0', '1' AFTER (tdevice_PRGSUSP/100);
--                          END IF;
--                         ASSERT RES_TO_SUSP_TYP_TIME = '0'
--                         REPORT "Typical periods are needed for " &
--                                "Program to progress to completion"
--                         SEVERITY warning;
--                     ELSE
--                         ASSERT FALSE
--                         REPORT "Minimum for tPRS is not satisfied! " &
--                                "PGSP command is ignored"
--                         SEVERITY warning;
--                     END IF;
--                 END IF;

--             WHEN PG_SUSP      =>
--                 fast_rd <= true;
--                 rd      <= false;
--                 dual    <= false;
--                 ddr     <= false;
--                 IF PRGSUSP_out = '1' AND PRGSUSP_in = '1' THEN
--                     PRGSUSP_in <= '0';
--                     --The RDY/BSY bit in the Status Register will indicate that
--                     --the device is ready for another operation.
--                     WIP := '0';
--                     --The Program Suspend (PS) bit in the Status Register will
--                     --be set to the logical “1” state to indicate that the
--                     --program operation has been suspended.
--                     PS := '1';
--                 END IF;

--                 IF oe THEN
--                     any_read <= true;
--                     IF Instruct = RDSR THEN
--                         --Read Status Register 1
--                         SOOut_zd <= Status_reg1(7-read_cnt);
--                         read_cnt := read_cnt + 1;
--                         IF read_cnt = 8 THEN
--                             read_cnt := 0;
--                         END IF;
--                     ELSIF Instruct = RDSR2 THEN
--                         --Read Status Register 2
--                         SOOut_zd <= Status_reg2(7-read_cnt);
--                         read_cnt := read_cnt + 1;
--                         IF read_cnt = 8 THEN
--                             read_cnt := 0;
--                         END IF;
--                     ELSIF Instruct = RDCR THEN
--                         --Read Configuration Register 1
--                         SOOut_zd <= Config_reg1(7-read_cnt);
--                         read_cnt := read_cnt + 1;
--                         IF read_cnt = 8 THEN
--                             read_cnt := 0;
--                         END IF;
--                     ELSIF Instruct = BRRD THEN
--                         --Read Bank Address Register
--                         fast_rd <= true;
--                         rd      <= false;
--                         dual    <= false;
--                         ddr     <= false;
--                         SOOut_zd <= Bank_Addr_reg(7-read_cnt);
--                         read_cnt := read_cnt + 1;
--                         IF read_cnt = 8 THEN
--                             read_cnt := 0;
--                         END IF;
--                     --Read Array Operations
--                     ELSIF Instruct = READ OR Instruct = RD4 OR
--                           Instruct = FSTRD OR Instruct = FSTRD4 OR
--                           Instruct = DDRFR OR Instruct = DDRFR4 THEN
--                         --Read Memory array
--                         IF Instruct = READ OR Instruct = RD4 THEN
--                             fast_rd <= false;
--                             rd      <= true;
--                             dual    <= false;
--                             ddr     <= false;
--                         ELSIF Instruct = DDRFR OR Instruct = DDRFR4 THEN
--                             fast_rd <= false;
--                             rd      <= false;
--                             dual    <= false;
--                             ddr     <= true;
--                         ELSE
--                             fast_rd <= true;
--                             rd      <= false;
--                             dual    <= false;
--                             ddr     <= false;
--                         END IF;
--                         IF bus_cycle_state = DUMMY_BYTES
--                         OR DummyBytes_act = '1' THEN
--                             DummyBytes_act := '0';
--                             IF (Instruct=DDRFR OR Instruct=DDRFR4) THEN
--                                 -- Data Learning Pattern (DLP) is
--                                 -- enabled, Optional DLP
--                                 data_out:= VDLR_reg;
--                                 SOOut_zd<=data_out(7-read_cnt);
--                                 read_cnt:=read_cnt + 1;
--                                 IF read_cnt = 8 THEN
--                                     read_cnt := 0;
--                                 END IF;
--                             END IF;
--                         ELSE
--                             IF pgm_page /= read_addr/PageSize THEN
--                                 SecAddr := ReturnSectorID(read_addr);
--                                 Sec_addr := read_addr - SecAddr*(SecSize+1);
--                                 READ_DATA(SecAddr,Sec_addr,mem_data);
--                                 data_out := to_slv((mem_data),8);
--                                 SOOut_zd <= data_out(7-read_cnt);
--                                 read_cnt := read_cnt + 1;
--                                 IF read_cnt = 8 THEN
--                                     read_cnt := 0;
--                                     IF read_addr = AddrRANGE THEN
--                                         read_addr := 0;
--                                     ELSE
--                                         read_addr := read_addr + 1;
--                                     END IF;
--                                 END IF;
--                             ELSE
--                                 SOOut_zd <= 'U';
--                                 read_cnt := read_cnt + 1;
--                                 IF read_cnt = 8 THEN
--                                     read_cnt := 0;
--                                     IF read_addr = AddrRANGE THEN
--                                         read_addr := 0;
--                                     ELSE
--                                         read_addr := read_addr + 1;
--                                     END IF;
--                                 END IF;
--                             END IF;
--                         END IF;
--                     ELSIF Instruct =DOR OR Instruct=DOR4 OR
--                           Instruct = DIOR OR Instruct = DIOR4 OR
--                           Instruct = DDRDIOR OR Instruct = DDRDIOR4 THEN
--                         --Read Memory array
--                         IF Instruct = DDRDIOR OR Instruct = DDRDIOR4 THEN
--                             fast_rd <= false;
--                             rd      <= false;
--                             dual    <= true;
--                             ddr     <= true;
--                         ELSE
--                             fast_rd <= false;
--                             rd      <= false;
--                             dual    <= true;
--                             ddr     <= false;
--                         END IF;
--                         IF bus_cycle_state = DUMMY_BYTES
--                         OR DummyBytes_act = '1' THEN
--                             DummyBytes_act := '0';
--                             data_out:=VDLR_reg;
--                             SOOut_zd<=data_out(7-read_cnt);
--                             SIOut_zd<=data_out(7-read_cnt);
--                             read_cnt := read_cnt + 1;
--                             IF read_cnt = 8 THEN
--                                 read_cnt := 0;
--                             END IF;
--                         ELSE
--                             IF  pgm_page /= read_addr/PageSize THEN
--                                 SecAddr := ReturnSectorID(read_addr);
--                                 Sec_addr := read_addr - SecAddr*(SecSize+1);
--                                 READ_DATA(SecAddr,Sec_addr,mem_data);
--                                 data_out := to_slv((mem_data),8);
--                                 SOOut_zd <= data_out(7-2*read_cnt);
--                                 SIOut_zd <= data_out(6-2*read_cnt);
--                                 read_cnt := read_cnt + 1;
--                                 IF read_cnt = 4 THEN
--                                     read_cnt := 0;
--                                     IF read_addr = AddrRANGE THEN
--                                         read_addr := 0;
--                                     ELSE
--                                         read_addr := read_addr + 1;
--                                     END IF;
--                                 END IF;
--                             ELSE
--                                 SOOut_zd <= 'U';
--                                 SIOut_zd <= 'U';
--                                 read_cnt := read_cnt + 1;
--                                 IF read_cnt = 4 THEN
--                                     read_cnt := 0;
--                                     IF read_addr = AddrRANGE THEN
--                                         read_addr := 0;
--                                     ELSE
--                                         read_addr := read_addr + 1;
--                                     END IF;
--                                 END IF;
--                             END IF;
--                         END IF;
--                     ELSIF (Instruct = QOR OR Instruct=QOR4 OR
--                            Instruct = QIOR OR Instruct= QIOR4 OR
--                            Instruct = DDRQIOR OR Instruct= DDRQIOR4)
--                            AND QUAD = '1' THEN
--                         --Read Memory array
--                         IF Instruct = DDRQIOR OR Instruct = DDRQIOR4 THEN
--                             fast_rd <= false;
--                             rd      <= false;
--                             dual    <= true;
--                             ddr     <= true;
--                         ELSE
--                             fast_rd <= false;
--                             rd      <= false;
--                             dual    <= true;
--                             ddr     <= false;
--                         END IF;
--                         IF bus_cycle_state = DUMMY_BYTES
--                         OR DummyBytes_act = '1' THEN
--                             DummyBytes_act := '0';
--                             data_out := VDLR_reg;
--                             HOLDNegOut_zd <= data_out(7-read_cnt);
--                             WPNegOut_zd   <= data_out(7-read_cnt);
--                             SOOut_zd      <= data_out(7-read_cnt);
--                             SIOut_zd      <= data_out(7-read_cnt);
--                             read_cnt := read_cnt + 1;
--                             IF read_cnt = 8 THEN
--                                 read_cnt := 0;
--                             END IF;
--                         ELSE
--                             IF  pgm_page /= read_addr/PageSize  THEN
--                                 SecAddr := ReturnSectorID(read_addr);
--                                 Sec_addr := read_addr - SecAddr*(SecSize+1);
--                                 READ_DATA(SecAddr,Sec_addr,mem_data);
--                                 data_out := to_slv((mem_data),8);
--                                 HOLDNegOut_zd   <= data_out(7-4*read_cnt);
--                                 WPNegOut_zd     <= data_out(6-4*read_cnt);
--                                 SOOut_zd        <= data_out(5-4*read_cnt);
--                                 SIOut_zd        <= data_out(4-4*read_cnt);
--                                 read_cnt := read_cnt + 1;
--                                 IF read_cnt = 2 THEN
--                                     read_cnt := 0;
--                                     IF read_addr = AddrRANGE THEN
--                                         read_addr := 0;
--                                     ELSE
--                                         read_addr := read_addr + 1;
--                                     END IF;
--                                 END IF;
--                             ELSE
--                                 HOLDNegOut_zd   <= 'U';
--                                 WPNegOut_zd     <= 'U';
--                                 SOOut_zd        <= 'U';
--                                 SIOut_zd        <= 'U';
--                                 read_cnt := read_cnt + 1;
--                                 IF read_cnt = 2 THEN
--                                     read_cnt := 0;
--                                     IF read_addr = AddrRANGE THEN
--                                         read_addr := 0;
--                                     ELSE
--                                         read_addr := read_addr + 1;
--                                     END IF;
--                                 END IF;
--                             END IF;
--                         END IF;
--                     END IF;
--                 ELSIF oe_z THEN
--                     IF Instruct = READ OR Instruct = RD4 THEN
--                         fast_rd <= false;
--                         rd      <= true;
--                         dual    <= false;
--                         ddr     <= false;
--                     ELSIF Instruct = DDRFR OR Instruct = DDRFR4 THEN
--                         fast_rd <= false;
--                         rd      <= false;
--                         dual    <= false;
--                         ddr     <= true;
--                     ELSIF Instruct = DDRDIOR OR Instruct = DDRDIOR4 THEN
--                         fast_rd <= false;
--                         rd      <= false;
--                         dual    <= true;
--                         ddr     <= true;
--                     ELSIF Instruct =DOR OR Instruct=DOR4 OR
--                           Instruct = DIOR OR Instruct = DIOR4 OR
--                           Instruct = QOR OR Instruct=QOR4 OR
--                           Instruct = QIOR OR Instruct= QIOR4 THEN
--                         fast_rd <= false;
--                         rd      <= false;
--                         dual    <= true;
--                         ddr     <= false;
--                     ELSIF Instruct = DDRQIOR OR Instruct = DDRQIOR4 THEN
--                         fast_rd <= false;
--                         rd      <= false;
--                         dual    <= true;
--                         ddr     <= true;
--                     ELSE
--                         fast_rd <= true;
--                         rd      <= false;
--                         dual    <= false;
--                         ddr     <= false;
--                     END IF;
--                     HOLDNegOut_zd <= 'Z';
--                     WPNegOut_zd   <= 'Z';
--                     SOOut_zd      <= 'Z';
--                     SIOut_zd      <= 'Z';
--                 ELSIF falling_edge(write) THEN
--                     IF Instruct = BRWR THEN
--                         Bank_Addr_reg(7) := Bank_Addr_reg_in(7);
--                         Bank_Addr_reg(1) := Bank_Addr_reg_in(1);
--                         Bank_Addr_reg(0) := Bank_Addr_reg_in(0);
--                     ELSIF Instruct = WRR AND BAR_ACC = '1' THEN
--                     -- Write to the lower address bits of the BAR
--                         IF (P_ERR = '0' AND E_ERR = '0')THEN
--                             Bank_Addr_reg(1) := Bank_Addr_reg_in(1);
--                             Bank_Addr_reg(0) := Bank_Addr_reg_in(0);
--                         END IF;
--                     ELSIF Instruct = PGRS THEN
--                         PGRES <= '1', '0' AFTER 5 ns;
--                         PS := '0';
--                         WIP := '1';
--                         RES_TO_SUSP_MIN_TIME <= '1', '0' AFTER 60 ns;
--                         RES_TO_SUSP_TYP_TIME <= '1', '0' AFTER 100 us;
--                     END IF;

--                     IF Instruct = BRAC AND P_ERR = '0' AND E_ERR = '0' AND
--                        RdPswdProtMode = '0' THEN
--                         BAR_ACC <= '1';
--                     ELSE
--                         BAR_ACC <= '0';
--                     END IF;
--                 END IF;

--             WHEN ERS_SUSP_PG_SUSP      =>
--                 fast_rd <= true;
--                 rd      <= false;
--                 dual    <= false;
--                 ddr     <= false;
--                 IF PRGSUSP_out = '1' AND PRGSUSP_in = '1' THEN
--                     PRGSUSP_in <= '0';
--                     --The RDY/BSY bit in the Status Register will indicate that
--                     --the device is ready for another operation.
--                     WIP := '0';
--                     --The Program Suspend (PS) bit in the Status Register will
--                     --be set to the logical “1” state to indicate that the
--                     --program operation has been suspended.
--                     PS := '1';
--                 END IF;

--                 IF oe THEN
--                     any_read <= true;
--                     IF Instruct = RDSR THEN
--                         --Read Status Register 1
--                         SOOut_zd <= Status_reg1(7-read_cnt);
--                         read_cnt := read_cnt + 1;
--                         IF read_cnt = 8 THEN
--                             read_cnt := 0;
--                         END IF;
--                     ELSIF Instruct = RDSR2 THEN
--                         --Read Status Register 2
--                         SOOut_zd <= Status_reg2(7-read_cnt);
--                         read_cnt := read_cnt + 1;
--                         IF read_cnt = 8 THEN
--                             read_cnt := 0;
--                         END IF;
--                     ELSIF Instruct = RDCR THEN
--                         --Read Configuration Register 1
--                         SOOut_zd <= Config_reg1(7-read_cnt);
--                         read_cnt := read_cnt + 1;
--                         IF read_cnt = 8 THEN
--                             read_cnt := 0;
--                         END IF;
--                     ELSIF Instruct = BRRD THEN
--                         --Read Bank Address Register
--                         fast_rd <= true;
--                         rd      <= false;
--                         dual    <= false;
--                         ddr     <= false;
--                         SOOut_zd <= Bank_Addr_reg(7-read_cnt);
--                         read_cnt := read_cnt + 1;
--                         IF read_cnt = 8 THEN
--                             read_cnt := 0;
--                         END IF;
--                     --Read Array Operations
--                     ELSIF Instruct = READ OR Instruct = RD4 OR
--                           Instruct = FSTRD OR Instruct = FSTRD4 OR
--                           Instruct = DDRFR OR Instruct = DDRFR4 THEN
--                         --Read Memory array
--                         IF Instruct = READ OR Instruct = RD4 THEN
--                             fast_rd <= false;
--                             rd      <= true;
--                             dual    <= false;
--                             ddr     <= false;
--                         ELSIF Instruct = DDRFR OR Instruct = DDRFR4 THEN
--                             fast_rd <= false;
--                             rd      <= false;
--                             dual    <= false;
--                             ddr     <= true;
--                         ELSE
--                             fast_rd <= true;
--                             rd      <= false;
--                             dual    <= false;
--                             ddr     <= false;
--                         END IF;
--                         IF bus_cycle_state = DUMMY_BYTES
--                         OR DummyBytes_act = '1' THEN
--                             DummyBytes_act := '0';
--                             IF (Instruct=DDRFR OR Instruct=DDRFR4) THEN
--                                 data_out:= VDLR_reg;
--                                 SOOut_zd<=data_out(7-read_cnt);
--                                 read_cnt:=read_cnt + 1;
--                                 IF read_cnt = 8 THEN
--                                     read_cnt := 0;
--                                 END IF;
--                             END IF;
--                         ELSE
--                             IF (SectorSuspend /= read_addr/(SecSize+1) AND
--                             pgm_page /= read_addr/PageSize) THEN
--                                 SecAddr := ReturnSectorID(read_addr);
--                                 Sec_addr := read_addr - SecAddr*(SecSize+1);
--                                 READ_DATA(SecAddr,Sec_addr,mem_data);
--                                 data_out := to_slv((mem_data),8);
--                                 SOOut_zd <= data_out(7-read_cnt);
--                                 read_cnt := read_cnt + 1;
--                                 IF read_cnt = 8 THEN
--                                     read_cnt := 0;
--                                     IF read_addr = AddrRANGE THEN
--                                         read_addr := 0;
--                                     ELSE
--                                         read_addr := read_addr + 1;
--                                     END IF;
--                                 END IF;
--                             ELSE
--                                 SOOut_zd <= 'U';
--                                 read_cnt := read_cnt + 1;
--                                 IF read_cnt = 8 THEN
--                                     read_cnt := 0;
--                                     IF read_addr = AddrRANGE THEN
--                                         read_addr := 0;
--                                     ELSE
--                                         read_addr := read_addr + 1;
--                                     END IF;
--                                 END IF;
--                             END IF;
--                         END IF;
--                     ELSIF Instruct =DOR OR Instruct=DOR4 OR
--                           Instruct = DIOR OR Instruct = DIOR4 OR
--                           Instruct = DDRDIOR OR Instruct = DDRDIOR4 THEN
--                         --Read Memory array

--                         IF Instruct = DDRDIOR OR Instruct = DDRDIOR4 THEN
--                             fast_rd <= false;
--                             rd      <= false;
--                             dual    <= true;
--                             ddr     <= true;
--                         ELSE
--                             fast_rd <= false;
--                             rd      <= false;
--                             dual    <= true;
--                             ddr     <= false;
--                         END IF;
--                         IF bus_cycle_state = DUMMY_BYTES
--                         OR DummyBytes_act = '1' THEN
--                             DummyBytes_act := '0';
--                             IF (Instruct=DDRDIOR OR Instruct=DDRDIOR4) THEN
--                                 data_out:=VDLR_reg;
--                                 SOOut_zd<=data_out(7-read_cnt);
--                                 SIOut_zd<=data_out(7-read_cnt);
--                                 read_cnt := read_cnt + 1;
--                                 IF read_cnt = 8 THEN
--                                     read_cnt := 0;
--                                 END IF;
--                             END IF;
--                         ELSE
--                             IF (SectorSuspend /= read_addr/(SecSize+1) AND
--                             pgm_page /= read_addr/PageSize) THEN
--                                 SecAddr := ReturnSectorID(read_addr);
--                                 Sec_addr := read_addr - SecAddr*(SecSize+1);
--                                 READ_DATA(SecAddr,Sec_addr,mem_data);
--                                 data_out := to_slv((mem_data),8);
--                                 SOOut_zd <= data_out(7-2*read_cnt);
--                                 SIOut_zd <= data_out(6-2*read_cnt);
--                                 read_cnt := read_cnt + 1;
--                                 IF read_cnt = 4 THEN
--                                     read_cnt := 0;
--                                     IF read_addr = AddrRANGE THEN
--                                         read_addr := 0;
--                                     ELSE
--                                         read_addr := read_addr + 1;
--                                     END IF;
--                                 END IF;
--                             ELSE
--                                 SOOut_zd <= 'U';
--                                 SIOut_zd <= 'U';
--                                 read_cnt := read_cnt + 1;
--                                 IF read_cnt = 4 THEN
--                                     read_cnt := 0;
--                                     IF read_addr = AddrRANGE THEN
--                                         read_addr := 0;
--                                     ELSE
--                                         read_addr := read_addr + 1;
--                                     END IF;
--                                 END IF;
--                             END IF;
--                         END IF;
--                     ELSIF (Instruct = QOR OR Instruct=QOR4 OR
--                            Instruct = QIOR OR Instruct= QIOR4 OR
--                            Instruct = DDRQIOR OR Instruct= DDRQIOR4)
--                            AND QUAD = '1' THEN

--                         --Read Memory array
--                         IF Instruct = DDRQIOR OR Instruct = DDRQIOR4 THEN
--                             fast_rd <= false;
--                             rd      <= false;
--                             dual    <= true;
--                             ddr     <= true;
--                         ELSE
--                             fast_rd <= false;
--                             rd      <= false;
--                             dual    <= true;
--                             ddr     <= false;
--                         END IF;
--                         IF bus_cycle_state = DUMMY_BYTES
--                         OR DummyBytes_act = '1' THEN
--                             DummyBytes_act := '0';
--                             data_out := VDLR_reg;
--                             HOLDNegOut_zd <= data_out(7-read_cnt);
--                             WPNegOut_zd   <= data_out(7-read_cnt);
--                             SOOut_zd      <= data_out(7-read_cnt);
--                             SIOut_zd      <= data_out(7-read_cnt);
--                             read_cnt := read_cnt + 1;
--                             IF read_cnt = 8 THEN
--                                 read_cnt := 0;
--                             END IF;
--                         ELSE
--                             IF (SectorSuspend /= read_addr/(SecSize+1) AND
--                             pgm_page /= read_addr/PageSize) THEN
--                                 SecAddr := ReturnSectorID(read_addr);
--                                 Sec_addr := read_addr - SecAddr*(SecSize+1);
--                                 READ_DATA(SecAddr,Sec_addr,mem_data);
--                                 data_out := to_slv((mem_data),8);
--                                 HOLDNegOut_zd   <= data_out(7-4*read_cnt);
--                                 WPNegOut_zd     <= data_out(6-4*read_cnt);
--                                 SOOut_zd        <= data_out(5-4*read_cnt);
--                                 SIOut_zd        <= data_out(4-4*read_cnt);
--                                 read_cnt := read_cnt + 1;
--                                 IF read_cnt = 2 THEN
--                                     read_cnt := 0;
--                                     IF read_addr = AddrRANGE THEN
--                                         read_addr := 0;
--                                     ELSE
--                                         read_addr := read_addr + 1;
--                                     END IF;
--                                 END IF;
--                             ELSE
--                                 HOLDNegOut_zd   <= 'U';
--                                 WPNegOut_zd     <= 'U';
--                                 SOOut_zd        <= 'U';
--                                 SIOut_zd        <= 'U';
--                                 read_cnt := read_cnt + 1;
--                                 IF read_cnt = 2 THEN
--                                     read_cnt := 0;
--                                     IF read_addr = AddrRANGE THEN
--                                         read_addr := 0;
--                                     ELSE
--                                         read_addr := read_addr + 1;
--                                     END IF;
--                                 END IF;
--                             END IF;
--                         END IF;
--                     END IF;
--                 ELSIF oe_z THEN
--                     IF Instruct = READ OR Instruct = RD4 THEN
--                         fast_rd <= false;
--                         rd      <= true;
--                         dual    <= false;
--                         ddr     <= false;
--                     ELSIF Instruct = DDRFR OR Instruct = DDRFR4 THEN
--                         fast_rd <= false;
--                         rd      <= false;
--                         dual    <= false;
--                         ddr     <= true;
--                     ELSIF Instruct = DDRDIOR OR Instruct = DDRDIOR4 THEN
--                         fast_rd <= false;
--                         rd      <= false;
--                         dual    <= true;
--                         ddr     <= true;
--                     ELSIF Instruct =DOR OR Instruct=DOR4 OR
--                           Instruct = DIOR OR Instruct = DIOR4 OR
--                           Instruct = QOR OR Instruct=QOR4 OR
--                           Instruct = QIOR OR Instruct= QIOR4 THEN
--                         fast_rd <= false;
--                         rd      <= false;
--                         dual    <= true;
--                         ddr     <= false;
--                     ELSIF Instruct = DDRQIOR OR Instruct = DDRQIOR4 THEN
--                         fast_rd <= false;
--                         rd      <= false;
--                         dual    <= true;
--                         ddr     <= true;
--                     ELSE
--                         fast_rd <= true;
--                         rd      <= false;
--                         dual    <= false;
--                         ddr     <= false;
--                     END IF;
--                     HOLDNegOut_zd <= 'Z';
--                     WPNegOut_zd   <= 'Z';
--                     SOOut_zd      <= 'Z';
--                     SIOut_zd      <= 'Z';
--                 ELSIF falling_edge(write) THEN
--                     IF Instruct = BRWR THEN
--                         Bank_Addr_reg(7) := Bank_Addr_reg_in(7);
--                         Bank_Addr_reg(1) := Bank_Addr_reg_in(1);
--                         Bank_Addr_reg(0) := Bank_Addr_reg_in(0);
--                     ELSIF Instruct = WRR AND BAR_ACC = '1' THEN
--                     -- Write to the lower address bits of the BAR
--                         IF (P_ERR = '0' AND E_ERR = '0')THEN
--                             Bank_Addr_reg(1) := Bank_Addr_reg_in(1);
--                             Bank_Addr_reg(0) := Bank_Addr_reg_in(0);
--                         END IF;
--                     ELSIF Instruct = PGRS THEN
--                         PGRES <= '1', '0' AFTER 5 ns;
--                         PS := '0';
--                         WIP := '1';
--                         RES_TO_SUSP_MIN_TIME <= '1', '0' AFTER 60 ns;
--                         RES_TO_SUSP_TYP_TIME <= '1', '0' AFTER 100 us;
--                     END IF;

--                     IF Instruct = BRAC AND P_ERR = '0' AND E_ERR = '0' AND
--                     RdPswdProtMode = '0' THEN
--                         BAR_ACC <= '1';
--                     ELSE
--                         BAR_ACC <= '0';
--                     END IF;
--                 END IF;

--             WHEN OTP_PG         =>
--                 fast_rd <= true;
--                 rd      <= false;
--                 dual    <= false;
--                 ddr     <= false;
--                 IF oe THEN
--                     any_read <= true;
--                     IF Instruct = RDSR THEN
--                         --Read Status Register 1
--                         SOOut_zd <= Status_reg1(7-read_cnt);
--                         read_cnt := read_cnt + 1;
--                         IF read_cnt = 8 THEN
--                             read_cnt := 0;
--                         END IF;
--                     ELSIF Instruct = RDSR2 THEN
--                         --Read Status Register 2
--                         SOOut_zd <= Status_reg2(7-read_cnt);
--                         read_cnt := read_cnt + 1;
--                         IF read_cnt = 8 THEN
--                             read_cnt := 0;
--                         END IF;
--                     ELSIF Instruct = RDCR THEN
--                         --Read Configuration Register 1
--                         SOOut_zd <= Config_reg1(7-read_cnt);
--                         read_cnt := read_cnt + 1;
--                         IF read_cnt = 8 THEN
--                             read_cnt := 0;
--                         END IF;
--                     END IF;
--                 ELSIF oe_z THEN
--                     HOLDNegOut_zd <= 'Z';
--                     WPNegOut_zd   <= 'Z';
--                     SOOut_zd      <= 'Z';
--                     SIOut_zd      <= 'Z';
--                 END IF;

--                 IF Address + wr_cnt <= OTPHiAddr THEN
--                     FOR i IN 0 TO wr_cnt LOOP
--                         new_int := WData(i);
--                         old_int := OTPMem(Addr + i);
--                         IF new_int > -1 THEN
--                             new_bit := to_slv(new_int,8);
--                             IF old_int > -1 THEN
--                                 old_bit := to_slv(old_int,8);
--                                 FOR j IN 0 TO 7 LOOP
--                                     IF old_bit(j) = '0' THEN
--                                         new_bit(j) := '0';
--                                     END IF;
--                                 END LOOP;
--                                 new_int := to_nat(new_bit);
--                             END IF;
--                             WData(i) := new_int;
--                         ELSE
--                             WData(i) := -1;
--                         END IF;

--                         OTPMem(Addr + i) :=  -1;
--                     END LOOP;
--                 ELSE
--                     ASSERT false
--                         REPORT "Programming will reach over address limit"&
--                         " of OTP array"
--                         SEVERITY warning;
--                 END IF;

--                 IF PDONE = '1' THEN
--                     WIP := '0';
--                     WEL := '0';
--                     FOR i IN 0 TO wr_cnt LOOP
--                         OTPMem(Addr_tmp + i) := WData(i);
--                     END LOOP;
--                     LOCK_BYTE1 := to_slv(OTPMem(16#10#),8);
--                     LOCK_BYTE2 := to_slv(OTPMem(16#11#),8);
--                     LOCK_BYTE3 := to_slv(OTPMem(16#12#),8);
--                     LOCK_BYTE4 := to_slv(OTPMem(16#13#),8);
--                 END IF;

--             WHEN SECTOR_ERS     =>
--                 fast_rd <= true;
--                 rd      <= false;
--                 dual    <= false;
--                 ddr     <= false;
--                 IF oe THEN
--                     any_read <= true;
--                     IF Instruct = RDSR THEN
--                         --Read Status Register 1
--                         SOOut_zd <= Status_reg1(7-read_cnt);
--                         read_cnt := read_cnt + 1;
--                         IF read_cnt = 8 THEN
--                             read_cnt := 0;
--                         END IF;
--                     ELSIF Instruct = RDSR2 THEN
--                         --Read Status Register 2
--                         SOOut_zd <= Status_reg2(7-read_cnt);
--                         read_cnt := read_cnt + 1;
--                         IF read_cnt = 8 THEN
--                             read_cnt := 0;
--                         END IF;
--                     ELSIF Instruct = RDCR THEN
--                         --Read Configuration Register 1
--                         SOOut_zd <= Config_reg1(7-read_cnt);
--                         read_cnt := read_cnt + 1;
--                         IF read_cnt = 8 THEN
--                             read_cnt := 0;
--                         END IF;
--                     END IF;
--                 ELSIF oe_z THEN
--                     HOLDNegOut_zd <= 'Z';
--                     WPNegOut_zd   <= 'Z';
--                     SOOut_zd      <= 'Z';
--                     SIOut_zd      <= 'Z';
--                 END IF;

--                 SecAddr := ReturnSectorID(Addr);
--                 Corrupt_Sec(SecAddr) := '1';
--                 erase_mem(
--                           0,
--                           SecSize,
--                           linked_list(SecAddr)
--                           );

--                 IF rising_edge(EDONE) THEN
--                     WIP := '0';
--                     WEL := '0';
--                     Corrupt_Sec(SecAddr) := '0';
--                     erase_mem(
--                               0,
--                               SecSize,
--                               linked_list(SecAddr)
--                               );

--                     ADDRHILO_SEC(AddrLo, AddrHi, Addr);
--                     FOR i IN AddrLo TO AddrHi LOOP
--                         pgm_page := i/PageSize;
-- --						QPP_page(pgm_page) := '0';
--                     END LOOP;

--                 ELSIF Instruct = ERSP AND ERSSUSP_in = '0' THEN
--                     ESUSP <= '1', '0' AFTER 10 ns;
--                     ERSSUSP_in <= '1';
-- --                     IF ERSSUSP_out = '0' THEN
--                         IF LongTimming = TRUE THEN
--                             ERSSUSP_out <= '0', '1' AFTER (tdevice_ERSSUSP);
--                          ELSE
--                             ERSSUSP_out <= '0', '1' AFTER (tdevice_ERSSUSP/100);
--                          END IF;
-- --                     END IF;
--                     ASSERT RES_TO_SUSP_TYP_TIME = '0'
--                     REPORT "Typical periods are needed for " &
--                            "Program to progress to completion"
--                     SEVERITY warning;
--                 END IF;

--             WHEN BULK_ERS       =>
--                 fast_rd <= true;
--                 rd      <= false;
--                 dual    <= false;
--                 ddr     <= false;
--                 IF oe THEN
--                     any_read <= true;
--                     IF Instruct = RDSR THEN
--                         --Read Status Register 1
--                         SOOut_zd <= Status_reg1(7-read_cnt);
--                         read_cnt := read_cnt + 1;
--                         IF read_cnt = 8 THEN
--                             read_cnt := 0;
--                         END IF;
--                     ELSIF Instruct = RDSR2 THEN
--                         --Read Status Register 2
--                         SOOut_zd <= Status_reg2(7-read_cnt);
--                         read_cnt := read_cnt + 1;
--                         IF read_cnt = 8 THEN
--                             read_cnt := 0;
--                         END IF;
--                     ELSIF Instruct = RDCR THEN
--                         --Read Configuration Register 1
--                         SOOut_zd <= Config_reg1(7-read_cnt);
--                         read_cnt := read_cnt + 1;
--                         IF read_cnt = 8 THEN
--                             read_cnt := 0;
--                         END IF;
--                     END IF;
--                 ELSIF oe_z THEN
--                     HOLDNegOut_zd <= 'Z';
--                     WPNegOut_zd   <= 'Z';
--                     SOOut_zd      <= 'Z';
--                     SIOut_zd      <= 'Z';
--                 END IF;

--                 IF falling_edge(EDONE) THEN
--                     FOR i IN 0 TO SecNum LOOP
--                         IF PPB_bits(i) = '1' AND DYB_bits(i) = '1' THEN
--                             Corrupt_Sec(i) := '1';
--                             erase_mem(
--                                     0,
--                                     SecSize,
--                                     linked_list(i)
--                                     );
--                         END IF;
--                     END LOOP;
--                 END IF;

--                 IF rising_edge(EDONE) THEN
--                     WIP := '0';
--                     WEL := '0';
--                     FOR i IN 0 TO SecNum LOOP
--                         IF PPB_bits(i) = '1' AND DYB_bits(i) = '1' THEN
--                             Corrupt_Sec(i) := '0';
--                             erase_mem(
--                                     0,
--                                     SecSize,
--                                     linked_list(i)
--                                     );
--                         END IF;
--                     END LOOP;

--                     FOR i IN 0 TO AddrRANGE LOOP
--                         sect := i/(SecSize+1);
--                         IF PPB_bits(sect) = '1' AND DYB_bits(sect) = '1' THEN
--                         	pgm_page := i/PageSize;
-- --							QPP_page(pgm_page) := '0';
--                         END IF;
--                     END LOOP;
--                 END IF;

--             WHEN ERS_SUSP       =>
--                 fast_rd <= true;
--                 rd      <= false;
--                 dual    <= false;
--                 ddr     <= false;
--                 IF ERSSUSP_out = '1' THEN
--                     ERSSUSP_in <= '0';
--                     --The Erase Suspend (ES) bit in the Status Register will
--                     --be set to the logical “1” state to indicate that the
--                     --erase operation has been suspended.
--                     ES := '1';
--                     --The WIP bit in the Status Register will indicate that
--                     --the device is ready for another operation.
--                     WIP := '0';
--                 END IF;

--                 IF oe THEN
--                     any_read <= true;
--                     IF Instruct = RDSR THEN
--                         --Read Status Register 1
--                         SOOut_zd <= Status_reg1(7-read_cnt);
--                         read_cnt := read_cnt + 1;
--                         IF read_cnt = 8 THEN
--                             read_cnt := 0;
--                         END IF;
--                     ELSIF Instruct = RDSR2 THEN
--                         --Read Status Register 2
--                         SOOut_zd <= Status_reg2(7-read_cnt);
--                         read_cnt := read_cnt + 1;
--                         IF read_cnt = 8 THEN
--                             read_cnt := 0;
--                         END IF;
--                     ELSIF Instruct = RDCR THEN
--                         --Read Configuration Register 1
--                         SOOut_zd <= Config_reg1(7-read_cnt);
--                         read_cnt := read_cnt + 1;
--                         IF read_cnt = 8 THEN
--                             read_cnt := 0;
--                         END IF;
--                     ELSIF Instruct = DYBRD THEN
--                         --Read DYB Access Register
--                         sect := Address/(SecSize+1);
--                         fast_rd <= true;
--                         rd      <= false;
--                         dual    <= false;
--                         ddr     <= false;
--                         IF DYB_bits(sect) = '1' THEN
--                             DYBAR(7 downto 0) := "11111111";
--                         ELSE
--                             DYBAR(7 downto 0) := "00000000";
--                         END IF;
--                         SOOut_zd <= DYBAR(7-read_cnt);
--                         read_cnt := read_cnt + 1;
--                         IF read_cnt = 8 THEN
--                             read_cnt := 0;
--                         END IF;
--                     ELSIF Instruct = BRRD THEN
--                         --Read Bank Address Register
--                         fast_rd <= true;
--                         rd      <= false;
--                         dual    <= false;
--                         ddr     <= false;
--                         SOOut_zd <= Bank_Addr_reg(7-read_cnt);
--                         read_cnt := read_cnt + 1;
--                         IF read_cnt = 8 THEN
--                             read_cnt := 0;
--                         END IF;
--                     ELSIF Instruct = PPBRD THEN
--                         --Read PPB Access Register
--                         sect := Address/(SecSize+1);
--                         fast_rd <= true;
--                         rd      <= false;
--                         dual    <= false;
--                         ddr     <= false;
--                         PPBAR(7 downto 0) := "UUUUUUUU";
--                         IF RdPswdProtMode = '0' THEN
--                             IF PPB_bits(sect) = '1' THEN
--                                 PPBAR(7 downto 0) := "11111111";
--                             ELSE
--                                 PPBAR(7 downto 0) := "00000000";
--                             END IF;
--                         END IF;
--                         SOOut_zd <= PPBAR(7-read_cnt);
--                         read_cnt := read_cnt + 1;
--                         IF read_cnt = 8 THEN
--                             read_cnt := 0;
--                         END IF;
--                     --Read Array Operations
--                     ELSIF Instruct = READ OR Instruct = RD4 OR
--                           Instruct = FSTRD OR Instruct = FSTRD4 OR
--                           Instruct = DDRFR OR Instruct = DDRFR4 THEN
--                         --Read Memory array
--                         IF Instruct = READ OR Instruct = RD4 THEN
--                             fast_rd <= false;
--                             rd      <= true;
--                             dual    <= false;
--                             ddr     <= false;
--                         ELSIF Instruct = DDRFR OR Instruct = DDRFR4 THEN
--                             fast_rd <= false;
--                             rd      <= false;
--                             dual    <= false;
--                             ddr     <= true;
--                         ELSE
--                             fast_rd <= true;
--                             rd      <= false;
--                             dual    <= false;
--                             ddr     <= false;
--                         END IF;
--                         IF bus_cycle_state = DUMMY_BYTES
--                         OR DummyBytes_act = '1' THEN
--                             DummyBytes_act := '0';
--                             IF (Instruct=DDRFR OR Instruct=DDRFR4) THEN
--                                 data_out:= VDLR_reg;
--                                 SOOut_zd<=data_out(7-read_cnt);
--                                 read_cnt:=read_cnt + 1;
--                                 IF read_cnt = 8 THEN
--                                     read_cnt := 0;
--                                 END IF;
--                             END IF;
--                         ELSE
--                             IF SectorSuspend /= read_addr/(SecSize+1) THEN
--                                 SecAddr := ReturnSectorID(read_addr);
--                                 Sec_addr := read_addr - SecAddr*(SecSize+1);
--                                 READ_DATA(SecAddr,Sec_addr,mem_data);
--                                 data_out := to_slv((mem_data),8);
--                                 SOOut_zd <= data_out(7-read_cnt);
--                                 read_cnt := read_cnt + 1;
--                                 IF read_cnt = 8 THEN
--                                     read_cnt := 0;
--                                     IF read_addr = AddrRANGE THEN
--                                         read_addr := 0;
--                                     ELSE
--                                         read_addr := read_addr + 1;
--                                     END IF;
--                                 END IF;
--                             ELSE
--                                 SOOut_zd <= 'U';
--                                 read_cnt := read_cnt + 1;
--                                 IF read_cnt = 8 THEN
--                                     read_cnt := 0;
--                                     IF read_addr = AddrRANGE THEN
--                                         read_addr := 0;
--                                     ELSE
--                                         read_addr := read_addr + 1;
--                                     END IF;
--                                 END IF;
--                             END IF;
--                         END IF;
--                     ELSIF Instruct =DOR OR Instruct=DOR4 OR
--                           Instruct = DIOR OR Instruct = DIOR4 OR
--                           Instruct = DDRDIOR OR Instruct = DDRDIOR4 THEN
--                         --Read Memory array
--                         IF Instruct = DDRDIOR OR Instruct = DDRDIOR4 THEN
--                             fast_rd <= false;
--                             rd      <= false;
--                             dual    <= true;
--                             ddr     <= true;
--                         ELSE
--                             fast_rd <= false;
--                             rd      <= false;
--                             dual    <= true;
--                             ddr     <= false;
--                         END IF;
--                         IF bus_cycle_state = DUMMY_BYTES
--                         OR DummyBytes_act = '1' THEN
--                             DummyBytes_act := '0';
--                             data_out:=VDLR_reg;
--                             SOOut_zd<=data_out(7-read_cnt);
--                             SIOut_zd<=data_out(7-read_cnt);
--                             read_cnt := read_cnt + 1;
--                             IF read_cnt = 8 THEN
--                                 read_cnt := 0;
--                             END IF;
--                         ELSE
--                             IF SectorSuspend /= read_addr/(SecSize+1) THEN
--                                 SecAddr := ReturnSectorID(read_addr);
--                                 Sec_addr := read_addr - SecAddr*(SecSize+1);
--                                 READ_DATA(SecAddr,Sec_addr,mem_data);
--                                 data_out := to_slv((mem_data),8);
--                                 SOOut_zd <= data_out(7-2*read_cnt);
--                                 SIOut_zd <= data_out(6-2*read_cnt);
--                                 read_cnt := read_cnt + 1;
--                                 IF read_cnt = 4 THEN
--                                     read_cnt := 0;
--                                     IF read_addr = AddrRANGE THEN
--                                         read_addr := 0;
--                                     ELSE
--                                         read_addr := read_addr + 1;
--                                     END IF;
--                                 END IF;
--                             ELSE
--                                 SOOut_zd <= 'U';
--                                 SIOut_zd <= 'U';
--                                 read_cnt := read_cnt + 1;
--                                 IF read_cnt = 4 THEN
--                                     read_cnt := 0;
--                                     IF read_addr = AddrRANGE THEN
--                                         read_addr := 0;
--                                     ELSE
--                                         read_addr := read_addr + 1;
--                                     END IF;
--                                 END IF;
--                             END IF;
--                         END IF;
--                     ELSIF (Instruct = QOR OR Instruct=QOR4 OR
--                            Instruct = QIOR OR Instruct= QIOR4 OR
--                            Instruct = DDRQIOR OR Instruct= DDRQIOR4)
--                            AND QUAD = '1' THEN
--                         --Read Memory array
--                         IF Instruct = DDRQIOR OR Instruct = DDRQIOR4 THEN
--                             fast_rd <= false;
--                             rd      <= false;
--                             dual    <= true;
--                             ddr     <= true;
--                         ELSE
--                             fast_rd <= false;
--                             rd      <= false;
--                             dual    <= true;
--                             ddr     <= false;
--                         END IF;
--                         IF bus_cycle_state = DUMMY_BYTES
--                         OR DummyBytes_act = '1' THEN
--                             DummyBytes_act := '0';
--                             data_out := VDLR_reg;
--                             HOLDNegOut_zd <= data_out(7-read_cnt);
--                             WPNegOut_zd   <= data_out(7-read_cnt);
--                             SOOut_zd      <= data_out(7-read_cnt);
--                             SIOut_zd      <= data_out(7-read_cnt);
--                             read_cnt := read_cnt + 1;
--                             IF read_cnt = 8 THEN
--                                 read_cnt := 0;
--                             END IF;
--                         ELSE
--                             IF SectorSuspend /= read_addr/(SecSize+1) THEN
--                                 SecAddr := ReturnSectorID(read_addr);
--                                 Sec_addr := read_addr - SecAddr*(SecSize+1);
--                                 READ_DATA(SecAddr,Sec_addr,mem_data);
--                                 data_out := to_slv((mem_data),8);
--                                 HOLDNegOut_zd   <= data_out(7-4*read_cnt);
--                                 WPNegOut_zd     <= data_out(6-4*read_cnt);
--                                 SOOut_zd        <= data_out(5-4*read_cnt);
--                                 SIOut_zd        <= data_out(4-4*read_cnt);
--                                 read_cnt := read_cnt + 1;
--                                 IF read_cnt = 2 THEN
--                                     read_cnt := 0;
--                                     IF read_addr = AddrRANGE THEN
--                                         read_addr := 0;
--                                     ELSE
--                                         read_addr := read_addr + 1;
--                                     END IF;
--                                 END IF;
--                             ELSE
--                                 HOLDNegOut_zd   <= 'U';
--                                 WPNegOut_zd     <= 'U';
--                                 SOOut_zd        <= 'U';
--                                 SIOut_zd        <= 'U';
--                                 read_cnt := read_cnt + 1;
--                                 IF read_cnt = 2 THEN
--                                     read_cnt := 0;
--                                     IF read_addr = AddrRANGE THEN
--                                         read_addr := 0;
--                                     ELSE
--                                         read_addr := read_addr + 1;
--                                     END IF;
--                                 END IF;
--                             END IF;
--                         END IF;
--                     END IF;
--                 ELSIF oe_z THEN
--                     IF Instruct = READ OR Instruct = RD4 THEN
--                         fast_rd <= false;
--                         rd      <= true;
--                         dual    <= false;
--                         ddr     <= false;
--                     ELSIF Instruct = DDRFR OR Instruct = DDRFR4 THEN
--                         fast_rd <= false;
--                         rd      <= false;
--                         dual    <= false;
--                         ddr     <= true;
--                     ELSIF Instruct = DDRDIOR OR Instruct = DDRDIOR4 THEN
--                         fast_rd <= false;
--                         rd      <= false;
--                         dual    <= true;
--                         ddr     <= true;
--                     ELSIF Instruct =DOR OR Instruct=DOR4 OR
--                           Instruct = DIOR OR Instruct = DIOR4 OR
--                           Instruct = QOR OR Instruct=QOR4 OR
--                           Instruct = QIOR OR Instruct= QIOR4 THEN
--                         fast_rd <= false;
--                         rd      <= false;
--                         dual    <= true;
--                         ddr     <= false;
--                     ELSIF Instruct = DDRQIOR OR Instruct = DDRQIOR4 THEN
--                         fast_rd <= false;
--                         rd      <= false;
--                         dual    <= true;
--                         ddr     <= true;
--                     ELSE
--                         fast_rd <= true;
--                         rd      <= false;
--                         dual    <= false;
--                         ddr     <= false;
--                     END IF;
--                     HOLDNegOut_zd <= 'Z';
--                     WPNegOut_zd   <= 'Z';
--                     SOOut_zd      <= 'Z';
--                     SIOut_zd      <= 'Z';
--                 END IF;

--                 IF falling_edge(write) THEN
--                     IF (Instruct = PP OR Instruct = PP4) AND WEL = '1' THEN
--                         sect := Address/(SecSize+1);
--                         pgm_page := Address/PageSize;
--                         IF SectorSuspend /= sect THEN
--                             IF (Sec_Prot(sect) = '0' AND
--                                PPB_bits(sect)='1' AND DYB_bits(sect)='1') THEN
--                                 PSTART <= '1', '0' AFTER 5 ns;
--                                 PGSUSP  <= '0';
--                                 PGRES   <= '0';
--                                 WIP := '1' ;
--                                 SA <= sect;
--                                 Addr := Address;
--                                 Addr_tmp := Address;
--                                 wr_cnt := Byte_number;
--                                 FOR I IN wr_cnt DOWNTO 0 LOOP
--                                     IF Viol /= '0' THEN
--                                         WData(i) := -1;
--                                     ELSE
--                                         WData(i) := WByte(i);
--                                     END IF;
--                                 END LOOP;
--                             ELSE
--                                 WEL   := '0';
--                                 P_ERR := '1';
--                             END IF;
--                         ELSE
--                             WEL := '0';
--                             P_ERR := '1';
--                         END IF;
--                     ELSIF (Instruct = QPP OR Instruct = QPP4) AND WEL = '1' THEN
--                         sect := Address/(SecSize+1);
--                         pgm_page := Address/PageSize;
--                         IF SectorSuspend /= sect THEN
--                             IF (Sec_Prot(sect)='0' AND
--                                 PPB_bits(sect)='1' AND DYB_bits(sect)='1') THEN
--                                 PSTART <= '1', '0' AFTER 5 ns;
--                                 PGSUSP  <= '0';
--                                 PGRES   <= '0';
--                                 WIP := '1' ;
--                                 SA <= sect;
--                                 Addr := Address;
--                                 Addr_tmp := Address;
--                                 wr_cnt := Byte_number;
--                                 FOR I IN wr_cnt DOWNTO 0 LOOP
--                                     IF Viol /= '0' THEN
--                                         WData(i) := -1;
--                                     ELSE
--                                         WData(i) := WByte(i);
--                                     END IF;
--                                 END LOOP;
--                             ELSE
--                                 WEL   := '0';
--                                 P_ERR := '1';
--                             END IF;
--                         ELSE
--                             WEL := '0';
--                             P_ERR := '1';
--                         END IF;
--                     ELSIF Instruct = WREN THEN
--                         WEL := '1';
--                     ELSIF Instruct = CLSR THEN
--                     --The Clear Status Register Command resets bit SR1[5]
--                     --(Erase Fail Flag) and bit SR1[6] (Program Fail Flag)
--                        E_ERR := '0';
--                        P_ERR := '0';
--                     ELSIF Instruct = BRWR THEN
--                         Bank_Addr_reg(7) := Bank_Addr_reg_in(7);
--                         Bank_Addr_reg(1) := Bank_Addr_reg_in(1);
--                         Bank_Addr_reg(0) := Bank_Addr_reg_in(0);
--                     ELSIF Instruct = WRR AND BAR_ACC = '1' THEN
--                     -- Write to the lower address bits of the BAR
--                         IF (P_ERR = '0' AND E_ERR = '0')THEN
--                             Bank_Addr_reg(1) := Bank_Addr_reg_in(1);
--                             Bank_Addr_reg(0) := Bank_Addr_reg_in(0);
--                         END IF;
--                     ELSIF Instruct = DYBWR  AND WEL = '1' THEN
--                         sect := Address/(SecSize+1);
--                         pgm_page := Address/PageSize;
--                         PSTART <= '1', '0' AFTER 5 ns;
--                         WIP := '1';
--                     ELSIF Instruct = ERRS THEN
--                         ERES <= '1', '0' AFTER 5 ns;
--                         ES := '0';
--                         WIP := '1';
--                         Addr := SectorSuspend*(SecSize+1);
--                         ADDRHILO_SEC(AddrLo, AddrHi, Addr);
--                         RES_TO_SUSP_TYP_TIME <= '1', '0' AFTER 100 us;
--                     END IF;

--                     IF Instruct = BRAC AND P_ERR = '0' AND E_ERR = '0' AND
--                        RdPswdProtMode = '0' THEN
--                         BAR_ACC <= '1';
--                     ELSE
--                         BAR_ACC <= '0';
--                     END IF;
--                 END IF;

--             WHEN ERS_SUSP_PG    =>
--                 fast_rd <= true;
--                 rd      <= false;
--                 dual    <= false;
--                 ddr     <= false;
--                 IF oe THEN
--                     any_read <= true;
--                     IF Instruct = RDSR THEN
--                         --Read Status Register 1
--                         SOOut_zd <= Status_reg1(7-read_cnt);
--                         read_cnt := read_cnt + 1;
--                         IF read_cnt = 8 THEN
--                             read_cnt := 0;
--                         END IF;
--                     ELSIF Instruct = RDSR2 THEN
--                         --Read Status Register 2
--                         SOOut_zd <= Status_reg2(7-read_cnt);
--                         read_cnt := read_cnt + 1;
--                         IF read_cnt = 8 THEN
--                             read_cnt := 0;
--                         END IF;
--                     ELSIF Instruct = RDCR THEN
--                         --Read Configuration Register 1
--                         SOOut_zd <= Config_reg1(7-read_cnt);
--                         read_cnt := read_cnt + 1;
--                         IF read_cnt = 8 THEN
--                             read_cnt := 0;
--                         END IF;
--                     END IF;
--                 ELSIF oe_z THEN
--                     HOLDNegOut_zd <= 'Z';
--                     WPNegOut_zd   <= 'Z';
--                     SOOut_zd      <= 'Z';
--                     SIOut_zd      <= 'Z';
--                 END IF;

-- --                IF (Instruct = QPP OR Instruct = QPP4) THEN
-- --                    QPP_page(pgm_page) := '1';
-- --                END IF;

--                 ADDRHILO_PG(AddrLo, AddrHi,Sec_addr,Page_addr,Addr);
--                 cnt := 0;

--                 FOR i IN 0 TO wr_cnt LOOP
--                     new_int := WData(i);
--                     SecAddr := ReturnSectorID(Addr + i - cnt);
--                     Sec_addr := Addr + i - cnt - SecAddr*(SecSize+1);
--                     READ_DATA(SecAddr,Sec_addr,mem_data);
--                     old_int := mem_data;
--                     IF new_int > -1 THEN
--                         new_bit := to_slv(new_int,8);
--                         IF old_int > -1 THEN
--                             old_bit := to_slv(old_int,8);
--                             FOR j IN 0 TO 7 LOOP
--                                 IF old_bit(j) = '0' THEN
--                                     new_bit(j) := '0';
--                                 END IF;
--                             END LOOP;
--                             new_int := to_nat(new_bit);
--                         END IF;
--                         WData(i) := new_int;
--                     ELSE
--                         WData(i) := -1;
--                     END IF;

--                     IF (Addr + i) = AddrHi THEN
--                         Addr := AddrLo;
--                         cnt := i + 1;
--                     END IF;
--                 END LOOP;
--                 cnt :=0;

--                 IF PDONE = '1' THEN
--                     ADDRHILO_PG(AddrLo, AddrHi,Sec_addr,Page_addr,Addr);
--                     SecAddr := ReturnSectorID(Addr);
--                     WIP := '0';
--                     WEL := '0';
--                     FOR i IN 0 TO wr_cnt LOOP
--                         WRITE_DATA(SecAddr,
--                                    Sec_addr + i - cnt,
--                                    WData(i));
--                         IF (Sec_addr + i) = AddrHi THEN
--                             Sec_addr := AddrLo;
--                             cnt := i + 1;
--                         END IF;
--                     END LOOP;
--                 ELSIF Instruct = PGSP AND PRGSUSP_in = '0' THEN
--                     IF RES_TO_SUSP_MIN_TIME = '0' THEN
--                         PGSUSP <= '1', '0' AFTER 1 ns;
--                         PRGSUSP_in <= '1';
--                         IF LongTimming = TRUE THEN
--                             PRGSUSP_out <= '0', '1' AFTER (tdevice_PRGSUSP);
--                          ELSE
--                             PRGSUSP_out <= '0', '1' AFTER (tdevice_PRGSUSP/100);
--                          END IF;
--                         ASSERT RES_TO_SUSP_TYP_TIME = '0'
--                         REPORT "Typical periods are needed for " &
--                                "Program to progress to completion"
--                         SEVERITY warning;
--                     ELSE
--                         ASSERT FALSE
--                         REPORT "Minimum for tPRS is not satisfied! " &
--                                "PGSP command is ignored"
--                         SEVERITY warning;
--                     END IF;
--                 END IF;

--             WHEN PASS_PG        =>
--                 fast_rd <= true;
--                 rd      <= false;
--                 dual    <= false;
--                 ddr     <= false;
--                 IF oe THEN
--                     any_read <= true;
--                     IF Instruct = RDSR THEN
--                         --Read Status Register 1
--                         SOOut_zd <= Status_reg1(7-read_cnt);
--                         read_cnt := read_cnt + 1;
--                         IF read_cnt = 8 THEN
--                             read_cnt := 0;
--                         END IF;
--                     ELSIF Instruct = RDSR2 THEN
--                         --Read Status Register 2
--                         SOOut_zd <= Status_reg2(7-read_cnt);
--                         read_cnt := read_cnt + 1;
--                         IF read_cnt = 8 THEN
--                             read_cnt := 0;
--                         END IF;
--                     ELSIF Instruct = RDCR THEN
--                         --Read Configuration Register 1
--                         SOOut_zd <= Config_reg1(7-read_cnt);
--                         read_cnt := read_cnt + 1;
--                         IF read_cnt = 8 THEN
--                             read_cnt := 0;
--                         END IF;
--                     END IF;
--                 ELSIF oe_z THEN
--                     HOLDNegOut_zd <= 'Z';
--                     WPNegOut_zd   <= 'Z';
--                     SOOut_zd      <= 'Z';
--                     SIOut_zd      <= 'Z';
--                 END IF;

--                 new_pass := Password_reg_in;
--                 old_pass := Password_reg;
--                 FOR j IN 0 TO 63 LOOP
--                     IF old_pass(j) = '0' THEN
--                         new_pass(j) := '0';
--                     END IF;
--                 END LOOP;

--                 IF PDONE = '1' THEN
--                     Password_reg := new_pass;
--                     WIP  := '0';
--                     WEL  := '0';
--                 END IF;

--             WHEN PASS_UNLOCK    =>
--                 WIP := '1';
--                 fast_rd <= true;
--                 rd      <= false;
--                 dual    <= false;
--                 ddr     <= false;
--                 IF oe THEN
--                     any_read <= true;
--                     IF Instruct = RDSR THEN
--                         --Read Status Register 1
--                         SOOut_zd <= Status_reg1(7-read_cnt);
--                         read_cnt := read_cnt + 1;
--                         IF read_cnt = 8 THEN
--                             read_cnt := 0;
--                         END IF;
--                     ELSIF Instruct = RDSR2 THEN
--                         --Read Status Register 2
--                         SOOut_zd <= Status_reg2(7-read_cnt);
--                         read_cnt := read_cnt + 1;
--                         IF read_cnt = 8 THEN
--                             read_cnt := 0;
--                         END IF;
--                     ELSIF Instruct = RDCR THEN
--                         --Read Configuration Register 1
--                         SOOut_zd <= Config_reg1(7-read_cnt);
--                         read_cnt := read_cnt + 1;
--                         IF read_cnt = 8 THEN
--                             read_cnt := 0;
--                         END IF;
--                     END IF;
--                 ELSIF oe_z THEN
--                     HOLDNegOut_zd <= 'Z';
--                     WPNegOut_zd   <= 'Z';
--                     SOOut_zd      <= 'Z';
--                     SIOut_zd      <= 'Z';
--                 END IF;

--                 IF PASS_TEMP = Password_reg THEN
--                     PASS_UNLOCKED <= TRUE;
--                 ELSE
--                     PASS_UNLOCKED <= FALSE;
--                 END IF;

--                 IF PASSULCK_out = '1' THEN
--                     IF PASS_UNLOCKED AND PWDMLB = '0' THEN
--                         PPB_LOCK := '1';
--                         PPB_LOCK_temp <= '1';
--                         WIP   := '0';
--                     ELSE
--                         P_ERR := '1';
--                         REPORT "Incorrect Password!"
--                         SEVERITY warning;
--                         PASSACC_in <= '1';
--                         IF LongTimming = TRUE THEN
--                              PASSACC_out <= '0', '1' AFTER (tdevice_PASSACC);
--                           ELSE
--                              PASSACC_out <= '0', '1' AFTER (tdevice_PASSACC/10);
--                         END IF;
--                     END IF;
--                     WEL   := '0';
--                     PASSULCK_in <= '0';
--                 END IF;

--             WHEN PPB_PG         =>
--                 fast_rd <= true;
--                 rd      <= false;
--                 dual    <= false;
--                 ddr     <= false;
--                 IF oe THEN
--                     any_read <= true;
--                     IF Instruct = RDSR THEN
--                         --Read Status Register 1
--                         SOOut_zd <= Status_reg1(7-read_cnt);
--                         read_cnt := read_cnt + 1;
--                         IF read_cnt = 8 THEN
--                             read_cnt := 0;
--                         END IF;
--                     ELSIF Instruct = RDSR2 THEN
--                         --Read Status Register 2
--                         SOOut_zd <= Status_reg2(7-read_cnt);
--                         read_cnt := read_cnt + 1;
--                         IF read_cnt = 8 THEN
--                             read_cnt := 0;
--                         END IF;
--                     ELSIF Instruct = RDCR THEN
--                         --Read Configuration Register 1
--                         SOOut_zd <= Config_reg1(7-read_cnt);
--                         read_cnt := read_cnt + 1;
--                         IF read_cnt = 8 THEN
--                             read_cnt := 0;
--                         END IF;
--                     END IF;
--                 ELSIF oe_z THEN
--                     HOLDNegOut_zd <= 'Z';
--                     WPNegOut_zd   <= 'Z';
--                     SOOut_zd      <= 'Z';
--                     SIOut_zd      <= 'Z';
--                 END IF;

--                 IF PDONE = '1' THEN
--                     IF PPB_LOCK /= '0' THEN
--                         PPB_bits(sect):= '0';
--                         WIP   := '0';
--                         WEL   := '0';
--                     ELSE
--                         P_ERR := '0';
--                         WIP   := '0';
--                         WEL   := '0';
--                     END IF;
--                 END IF;

--             WHEN PPB_ERS        =>
--                 fast_rd <= true;
--                 rd      <= false;
--                 dual    <= false;
--                 ddr     <= false;
--                 IF oe THEN
--                     any_read <= true;
--                     IF Instruct = RDSR THEN
--                         --Read Status Register 1
--                         SOOut_zd <= Status_reg1(7-read_cnt);
--                         read_cnt := read_cnt + 1;
--                         IF read_cnt = 8 THEN
--                             read_cnt := 0;
--                         END IF;
--                     ELSIF Instruct = RDSR2 THEN
--                         --Read Status Register 2
--                         SOOut_zd <= Status_reg2(7-read_cnt);
--                         read_cnt := read_cnt + 1;
--                         IF read_cnt = 8 THEN
--                             read_cnt := 0;
--                         END IF;
--                     ELSIF Instruct = RDCR THEN
--                         --Read Configuration Register 1
--                         SOOut_zd <= Config_reg1(7-read_cnt);
--                         read_cnt := read_cnt + 1;
--                         IF read_cnt = 8 THEN
--                             read_cnt := 0;
--                         END IF;
--                     END IF;
--                 ELSIF oe_z THEN
--                     HOLDNegOut_zd <= 'Z';
--                     WPNegOut_zd   <= 'Z';
--                     SOOut_zd      <= 'Z';
--                     SIOut_zd      <= 'Z';
--                 END IF;

--                 IF PPBERASE_out = '1' THEN
--                     IF PPB_LOCK /= '0' AND PPBOTP = '1' THEN
--                         PPB_bits:= (OTHERS => '1');
--                     ELSE
--                         E_ERR := '1';
--                     END IF;
--                     WIP   := '0';
--                     WEL   := '0';
--                     PPBERASE_in <= '0';
--                 END IF;

--             WHEN AUTOBOOT_PG    =>
--                 fast_rd <= true;
--                 rd      <= false;
--                 dual    <= false;
--                 ddr     <= false;
--                 IF oe THEN
--                     any_read <= true;
--                     IF Instruct = RDSR THEN
--                         --Read Status Register 1
--                         SOOut_zd <= Status_reg1(7-read_cnt);
--                         read_cnt := read_cnt + 1;
--                         IF read_cnt = 8 THEN
--                             read_cnt := 0;
--                         END IF;
--                     ELSIF Instruct = RDSR2 THEN
--                         --Read Status Register 2
--                         SOOut_zd <= Status_reg2(7-read_cnt);
--                         read_cnt := read_cnt + 1;
--                         IF read_cnt = 8 THEN
--                             read_cnt := 0;
--                         END IF;
--                     ELSIF Instruct = RDCR THEN
--                         --Read Configuration Register 1
--                         SOOut_zd <= Config_reg1(7-read_cnt);
--                         read_cnt := read_cnt + 1;
--                         IF read_cnt = 8 THEN
--                             read_cnt := 0;
--                         END IF;
--                     END IF;
--                 ELSIF oe_z THEN
--                     HOLDNegOut_zd <= 'Z';
--                     WPNegOut_zd   <= 'Z';
--                     SOOut_zd      <= 'Z';
--                     SIOut_zd      <= 'Z';
--                 END IF;

--                 IF PDONE = '1' THEN
--                     FOR I IN 0 TO 3 LOOP
--                         FOR J IN 0 TO 7 LOOP
--                             AutoBoot_reg(I*8+J) :=
--                             AutoBoot_reg_in((3-I)*8+J);
--                         END LOOP;
--                     END LOOP;
--                     WIP  := '0';
--                     WEL  := '0';
--                 END IF;

--             WHEN PLB_PG         =>
--                 fast_rd <= true;
--                 rd      <= false;
--                 dual    <= false;
--                 ddr     <= false;
--                 IF oe THEN
--                     any_read <= true;
--                     IF Instruct = RDSR THEN
--                         --Read Status Register 1
--                         SOOut_zd <= Status_reg1(7-read_cnt);
--                         read_cnt := read_cnt + 1;
--                         IF read_cnt = 8 THEN
--                             read_cnt := 0;
--                         END IF;
--                     ELSIF Instruct = RDSR2 THEN
--                         --Read Status Register 2
--                         SOOut_zd <= Status_reg2(7-read_cnt);
--                         read_cnt := read_cnt + 1;
--                         IF read_cnt = 8 THEN
--                             read_cnt := 0;
--                         END IF;
--                     ELSIF Instruct = RDCR THEN
--                         --Read Configuration Register 1
--                         SOOut_zd <= Config_reg1(7-read_cnt);
--                         read_cnt := read_cnt + 1;
--                         IF read_cnt = 8 THEN
--                             read_cnt := 0;
--                         END IF;
--                     END IF;
--                 ELSIF oe_z THEN
--                     HOLDNegOut_zd <= 'Z';
--                     WPNegOut_zd   <= 'Z';
--                     SOOut_zd      <= 'Z';
--                     SIOut_zd      <= 'Z';
--                 END IF;

--                 IF PDONE = '1' THEN
--                     PPB_LOCK := '0';
--                     PPB_LOCK_temp <= '0';
--                     WIP  := '0';
--                     WEL  := '0';
--                 END IF;

--             WHEN DYB_PG         =>
--                 fast_rd <= true;
--                 rd      <= false;
--                 dual    <= false;
--                 ddr     <= false;
--                 IF oe THEN
--                     any_read <= true;
--                     IF Instruct = RDSR THEN
--                         --Read Status Register 1
--                         SOOut_zd <= Status_reg1(7-read_cnt);
--                         read_cnt := read_cnt + 1;
--                         IF read_cnt = 8 THEN
--                             read_cnt := 0;
--                         END IF;
--                     ELSIF Instruct = RDSR2 THEN
--                         --Read Status Register 2
--                         SOOut_zd <= Status_reg2(7-read_cnt);
--                         read_cnt := read_cnt + 1;
--                         IF read_cnt = 8 THEN
--                             read_cnt := 0;
--                         END IF;
--                     ELSIF Instruct = RDCR THEN
--                         --Read Configuration Register 1
--                         SOOut_zd <= Config_reg1(7-read_cnt);
--                         read_cnt := read_cnt + 1;
--                         IF read_cnt = 8 THEN
--                             read_cnt := 0;
--                         END IF;
--                     END IF;
--                 ELSIF oe_z THEN
--                     HOLDNegOut_zd <= 'Z';
--                     WPNegOut_zd   <= 'Z';
--                     SOOut_zd      <= 'Z';
--                     SIOut_zd      <= 'Z';
--                 END IF;

--                 IF PDONE = '1' THEN
--                     DYBAR := DYBAR_in;
--                     IF DYBAR = "11111111" THEN
--                         DYB_bits(sect):= '1';
--                     ELSIF DYBAR = "00000000" THEN
--                         DYB_bits(sect):= '0';
--                     ELSE
--                         P_ERR := '1';
--                     END IF;
--                     WIP  := '0';
--                     WEL  := '0';
--                 END IF;

--             WHEN ASP_PG         =>
--                 fast_rd <= true;
--                 rd      <= false;
--                 dual    <= false;
--                 ddr     <= false;
--                 IF oe THEN
--                     any_read <= true;
--                     IF Instruct = RDSR THEN
--                         --Read Status Register 1
--                         SOOut_zd <= Status_reg1(7-read_cnt);
--                         read_cnt := read_cnt + 1;
--                         IF read_cnt = 8 THEN
--                             read_cnt := 0;
--                         END IF;
--                     ELSIF Instruct = RDSR2 THEN
--                         --Read Status Register 2
--                         SOOut_zd <= Status_reg2(7-read_cnt);
--                         read_cnt := read_cnt + 1;
--                         IF read_cnt = 8 THEN
--                             read_cnt := 0;
--                         END IF;
--                     ELSIF Instruct = RDCR THEN
--                         --Read Configuration Register 1
--                         SOOut_zd <= Config_reg1(7-read_cnt);
--                         read_cnt := read_cnt + 1;
--                         IF read_cnt = 8 THEN
--                             read_cnt := 0;
--                         END IF;
--                     END IF;
--                 ELSIF oe_z THEN
--                     HOLDNegOut_zd <= 'Z';
--                     WPNegOut_zd   <= 'Z';
--                     SOOut_zd      <= 'Z';
--                     SIOut_zd      <= 'Z';
--                 END IF;

--                 IF PDONE = '1' THEN

--                     IF (RPME = '0' AND ASP_reg_in(5) = '1') THEN
--                        P_ERR := '1';
--                        REPORT "RPME bit is allready programmed"
--                        SEVERITY warning;
--                     ELSE
--                         RPME := ASP_reg_in(5);
--                     END IF;

--                     IF (PPBOTP = '0' AND ASP_reg_in(3) ='1') THEN
--                        P_ERR := '1';
--                        REPORT "PPBOTP bit is allready programmed"
--                        SEVERITY warning;
--                     ELSE
--                         PPBOTP := ASP_reg_in(3);
--                     END IF;

--                     IF (PWDMLB = '1' AND PSTMLB = '1') THEN
--                         IF (ASP_reg_in(2) = '0' AND ASP_reg_in(1) = '0') THEN
--                             REPORT "ASPR[2:1] = 00  Illegal condition"
--                             SEVERITY warning;
--                             P_ERR := '1';
--                         ELSE
--                             IF (ASP_reg_in(2) /= '1' OR
--                                 ASP_reg_in(1) /= '1') THEN
--                                 ASPOTPFLAG <= TRUE;
--                             END IF;
--                             PWDMLB := ASP_reg_in(2);
--                             PSTMLB := ASP_reg_in(1);
--                         END IF;
--                     END IF;
--                     WIP  := '0';
--                     WEL  := '0';
--                 END IF;

--             WHEN NVDLR_PG    =>
--                 fast_rd <= true;
--                 rd      <= false;
--                 dual    <= false;
--                 ddr     <= false;
--                 IF oe THEN
--                     any_read <= true;
--                     IF Instruct = RDSR THEN
--                         --Read Status Register 1
--                         SOOut_zd <= Status_reg1(7-read_cnt);
--                         read_cnt := read_cnt + 1;
--                         IF read_cnt = 8 THEN
--                             read_cnt := 0;
--                         END IF;
--                     ELSIF Instruct = RDSR2 THEN
--                         --Read Status Register 2
--                         SOOut_zd <= Status_reg2(7-read_cnt);
--                         read_cnt := read_cnt + 1;
--                         IF read_cnt = 8 THEN
--                             read_cnt := 0;
--                         END IF;
--                     ELSIF Instruct = RDCR THEN
--                         --Read Configuration Register 1
--                         SOOut_zd <= Config_reg1(7-read_cnt);
--                         read_cnt := read_cnt + 1;
--                         IF read_cnt = 8 THEN
--                             read_cnt := 0;
--                         END IF;
--                     END IF;
--                 ELSIF oe_z THEN
--                     HOLDNegOut_zd <= 'Z';
--                     WPNegOut_zd   <= 'Z';
--                     SOOut_zd      <= 'Z';
--                     SIOut_zd      <= 'Z';
--                 END IF;

--                 IF PDONE = '1' THEN
--                     IF NVDLR_reg = "00000000" THEN
--                         NVDLR_reg :=NVDLR_reg_in;
--                         VDLR_reg := NVDLR_reg_in;
--                         WIP := '0';
--                         WEL := '0';
--                     ELSE
--                         WIP   := '0';
--                         WEL   := '0';
--                         P_ERR := '1';
--                         REPORT "NVDLR is allready programmed"
--                         SEVERITY warning;
--                     END IF;
--                 END IF;

--             WHEN RESET_STATE    =>
--                 --the default condition hardware reset
--                 --The Bank Address Register is loaded to all zeroes
--                 Bank_Addr_reg := (others => '0');
--                 IF BPNV = '1' AND FREEZE = '0' THEN
--                     BP0 := '1';
--                     BP1 := '1';
--                     BP2 := '1';
--                     BP_bits := BP2 & BP1 & BP0;
--                     change_BP <= '1', '0' AFTER 1 ns;
--                 END IF;
--                 --Resets the Status register 1
--                 P_ERR     := '0';
--                 E_ERR     := '0';
--                 WEL       := '0';
--                 WIP       := '0';
--                 --Resets the Status register 2
--                 ES        := '0';
--                 PS        := '0';
--                 --Resets the Configuration register 1
--                 FREEZE    := '0';
--                 --On reset cycles the data pattern reverts back
--                 --to what is in the NVDLR
--                 VDLR_reg := NVDLR_reg;
--                 dlp_act := FALSE;
--                 --Loads the Program Buffer with all ones
--                 WData := (OTHERS => MaxData);

--                 IF PWDMLB = '0' THEN
--                     PPB_LOCK := '0';
--                     PPB_LOCK_temp <= '0';
--                 ELSE
--                     PPB_LOCK := '1';
--                     PPB_LOCK_temp <= '1';
--                 END IF;

--         END CASE;

--         --Output Disable Control
--         IF (CSNeg_ipd = '1') THEN
--             SIOut_zd        <= 'Z';
--             HOLDNegOut_zd   <= 'Z';
--             WPNegOut_zd     <= 'Z';
--             SOOut_zd        <= 'Z';
--         END IF;

--     END PROCESS Functional;

--     SFDPPreload:    PROCESS
--     BEGIN
--         SFDP_array(16#0000#) := 16#53#;
--         SFDP_array(16#0001#) := 16#46#;
--         SFDP_array(16#0002#) := 16#44#;
--         SFDP_array(16#0003#) := 16#50#;
--         SFDP_array(16#0004#) := 16#06#;
--         SFDP_array(16#0005#) := 16#01#;
--         SFDP_array(16#0006#) := 16#05#;
--         SFDP_array(16#0007#) := 16#FF#;
--         SFDP_array(16#0008#) := 16#00#;
--         SFDP_array(16#0009#) := 16#00#;
--         SFDP_array(16#000A#) := 16#01#;
--         SFDP_array(16#000B#) := 16#09#;
--         SFDP_array(16#000C#) := 16#20#;
--         SFDP_array(16#000D#) := 16#11#;
--         SFDP_array(16#000E#) := 16#00#;
--         SFDP_array(16#000F#) := 16#FF#;
--         SFDP_array(16#0010#) := 16#00#;
--         SFDP_array(16#0011#) := 16#05#;
--         SFDP_array(16#0012#) := 16#01#;
--         SFDP_array(16#0013#) := 16#10#;
--         SFDP_array(16#0014#) := 16#20#;
--         SFDP_array(16#0015#) := 16#11#;
--         SFDP_array(16#0016#) := 16#00#;
--         SFDP_array(16#0017#) := 16#FF#;
--         SFDP_array(16#0018#) := 16#00#;
--         SFDP_array(16#0019#) := 16#06#;
--         SFDP_array(16#001A#) := 16#01#;
--         SFDP_array(16#001B#) := 16#10#;
--         SFDP_array(16#001C#) := 16#20#;
--         SFDP_array(16#001D#) := 16#11#;
--         SFDP_array(16#001E#) := 16#00#;
--         SFDP_array(16#001F#) := 16#FF#;
--         SFDP_array(16#0020#) := 16#81#;
--         SFDP_array(16#0021#) := 16#00#;
--         SFDP_array(16#0022#) := 16#01#;
--         SFDP_array(16#0023#) := 16#02#;
--         SFDP_array(16#0024#) := 16#60#;
--         SFDP_array(16#0025#) := 16#11#;
--         SFDP_array(16#0026#) := 16#00#;
--         SFDP_array(16#0027#) := 16#FF#;
--         SFDP_array(16#0028#) := 16#84#;
--         SFDP_array(16#0029#) := 16#00#;
--         SFDP_array(16#002A#) := 16#01#;
--         SFDP_array(16#002B#) := 16#02#;
--         SFDP_array(16#002C#) := 16#68#;
--         SFDP_array(16#002D#) := 16#11#;
--         SFDP_array(16#002E#) := 16#00#;
--         SFDP_array(16#002F#) := 16#FF#;
--         SFDP_array(16#0030#) := 16#01#;
--         SFDP_array(16#0031#) := 16#01#;
--         SFDP_array(16#0032#) := 16#01#;
--         SFDP_array(16#0033#) := 16#5C#;
--         SFDP_array(16#0034#) := 16#00#;
--         SFDP_array(16#0035#) := 16#10#;
--         SFDP_array(16#0036#) := 16#00#;
--         SFDP_array(16#0037#) := 16#01#;
--         -- Undefined space
--         FOR I IN  16#0038# TO 16#1000# LOOP
--             SFDP_array(i) := 16#FF#;
--         END LOOP;
--         -- ID-CFI array data
--         -- Manufacturer and Device ID
--         SFDP_array(16#1000#) := 16#01#;
--         SFDP_array(16#1001#) := 16#02#;
--         SFDP_array(16#1002#) := 16#20#;
--         SFDP_array(16#1003#) := 16#00#;
--         -- Uniform 256kB sectors
--         SFDP_array(16#1004#) := 16#00#;
--         SFDP_array(16#1005#) := 16#80#;
--         SFDP_array(16#1006#) := 16#FF#;
--         SFDP_array(16#1007#) := 16#FF#;
--         SFDP_array(16#1008#) := 16#FF#;
--         SFDP_array(16#1009#) := 16#FF#;
--         SFDP_array(16#100A#) := 16#FF#;
--         SFDP_array(16#100B#) := 16#FF#;
--         SFDP_array(16#100C#) := 16#FF#;
--         SFDP_array(16#100D#) := 16#FF#;
--         SFDP_array(16#100E#) := 16#FF#;
--         SFDP_array(16#100F#) := 16#FF#;
--         -- CFI Query Identification String
--         SFDP_array(16#1010#) := 16#51#;
--         SFDP_array(16#1011#) := 16#52#;
--         SFDP_array(16#1012#) := 16#59#;
--         SFDP_array(16#1013#) := 16#02#;
--         SFDP_array(16#1014#) := 16#00#;
--         SFDP_array(16#1015#) := 16#40#;
--         SFDP_array(16#1016#) := 16#00#;
--         SFDP_array(16#1017#) := 16#53#;
--         SFDP_array(16#1018#) := 16#46#;
--         SFDP_array(16#1019#) := 16#51#;
--         SFDP_array(16#101A#) := 16#00#;
--         -- CFI system interface string
--         SFDP_array(16#101B#) := 16#27#;
--         SFDP_array(16#101C#) := 16#36#;
--         SFDP_array(16#101D#) := 16#00#;
--         SFDP_array(16#101E#) := 16#00#;
--         SFDP_array(16#101F#) := 16#06#;
--         SFDP_array(16#1020#) := 16#09#;
--         -- 256kB sector
--         SFDP_array(16#1021#) := 16#09#;
--         SFDP_array(16#1022#) := 16#11#;
--         SFDP_array(16#1023#) := 16#02#;
--         SFDP_array(16#1024#) := 16#02#;
--         SFDP_array(16#1025#) := 16#03#;
--         SFDP_array(16#1026#) := 16#03#;
--         -- Device Geometry Definition
--         SFDP_array(16#1027#) := 16#1A#;
--         SFDP_array(16#1028#) := 16#02#;
--         SFDP_array(16#1029#) := 16#01#;
--         SFDP_array(16#102A#) := 16#09#;
--         SFDP_array(16#102B#) := 16#00#;
--         SFDP_array(16#102C#) := 16#01#;
--         SFDP_array(16#102D#) := 16#FF#;
--         SFDP_array(16#102E#) := 16#00#;
--         SFDP_array(16#102F#) := 16#00#;
--         SFDP_array(16#1030#) := 16#04#;
--         SFDP_array(16#1031#) := 16#FF#;
--         SFDP_array(16#1032#) := 16#FF#;
--         SFDP_array(16#1033#) := 16#FF#;
--         SFDP_array(16#1034#) := 16#FF#;
--         SFDP_array(16#1035#) := 16#FF#;
--         SFDP_array(16#1036#) := 16#FF#;
--         SFDP_array(16#1037#) := 16#FF#;
--         SFDP_array(16#1038#) := 16#FF#;
--         SFDP_array(16#1039#) := 16#FF#;
--         SFDP_array(16#103A#) := 16#FF#;
--         SFDP_array(16#103B#) := 16#FF#;
--         SFDP_array(16#103C#) := 16#FF#;
--         SFDP_array(16#103D#) := 16#FF#;
--         SFDP_array(16#103E#) := 16#FF#;
--         SFDP_array(16#103F#) := 16#FF#;
--         -- CFI Primary Vendor-Specific Extended Query
--         SFDP_array(16#1040#) := 16#50#;
--         SFDP_array(16#1041#) := 16#52#;
--         SFDP_array(16#1042#) := 16#49#;
--         SFDP_array(16#1043#) := 16#31#;
--         SFDP_array(16#1044#) := 16#33#;
--         SFDP_array(16#1045#) := 16#21#;
--         SFDP_array(16#1046#) := 16#02#;
--         SFDP_array(16#1047#) := 16#01#;
--         SFDP_array(16#1048#) := 16#00#;
--         SFDP_array(16#1049#) := 16#08#;
--         SFDP_array(16#104A#) := 16#00#;
--         SFDP_array(16#104B#) := 16#01#;
--         SFDP_array(16#104C#) := 16#04#;
--         SFDP_array(16#104D#) := 16#00#;
--         SFDP_array(16#104E#) := 16#00#;
--         SFDP_array(16#104F#) := 16#07#;
--         SFDP_array(16#1050#) := 16#01#;
--         -- CFI Alternate Vendor Specific Extended Query Header
--         SFDP_array(16#1051#) := 16#41#;
--         SFDP_array(16#1052#) := 16#4C#;
--         SFDP_array(16#1053#) := 16#54#;
--         SFDP_array(16#1054#) := 16#32#;
--         SFDP_array(16#1055#) := 16#30#;
--         -- CFI Alternate Vendor Specific Extended Query Parameter 0
--         SFDP_array(16#1056#) := 16#00#;
--         SFDP_array(16#1057#) := 16#10#;
--         SFDP_array(16#1058#) := 16#53#;
--         SFDP_array(16#1059#) := 16#32#;
--         SFDP_array(16#105A#) := 16#35#;
--         SFDP_array(16#105B#) := 16#46#;
--         SFDP_array(16#105C#) := 16#4C#;
--         SFDP_array(16#105D#) := 16#35#;
--         SFDP_array(16#105E#) := 16#31#;
--         SFDP_array(16#105F#) := 16#32#;
--         SFDP_array(16#1060#) := 16#53#;
--         SFDP_array(16#1061#) := 16#FF#;
--         SFDP_array(16#1062#) := 16#FF#;
--         SFDP_array(16#1063#) := 16#FF#;
--         SFDP_array(16#1064#) := 16#FF#;
--         SFDP_array(16#1065#) := 16#FF#;
--         SFDP_array(16#1066#) := 16#FF#;
--         SFDP_array(16#1067#) := 16#FF#;
--         -- CFI Alternate Vendor-Specific Extended Query Parameter 80h
--         SFDP_array(16#1068#) := 16#80#;
--         SFDP_array(16#1069#) := 16#01#;
--         SFDP_array(16#106A#) := 16#F0#;
--         -- CFI Alternate Vendor-Specific Extended Query Parameter 84h
--         SFDP_array(16#106B#) := 16#84#;
--         SFDP_array(16#106C#) := 16#08#;
--         SFDP_array(16#106D#) := 16#85#;
--         SFDP_array(16#106E#) := 16#28#;
--         SFDP_array(16#106F#) := 16#8A#;
--         SFDP_array(16#1070#) := 16#64#;
--         SFDP_array(16#1071#) := 16#75#;
--         SFDP_array(16#1072#) := 16#28#;
--         SFDP_array(16#1073#) := 16#7A#;
--         SFDP_array(16#1074#) := 16#64#;
--         -- CFI Alternate Vendor-Specific Extended Query Parameter 88h
--         SFDP_array(16#1075#) := 16#88#;
--         SFDP_array(16#1076#) := 16#04#;
--         SFDP_array(16#1077#) := 16#0A#;
--         SFDP_array(16#1078#) := 16#01#;
--         SFDP_array(16#1079#) := 16#FF#;
--         SFDP_array(16#107A#) := 16#01#;
--         -- CFI Alternate Vendor-Specific Extended Query Parameter 8Ch
--         SFDP_array(16#107B#) := 16#8C#;
--         SFDP_array(16#107C#) := 16#06#;
--         SFDP_array(16#107D#) := 16#96#;
--         SFDP_array(16#107E#) := 16#01#;
--         SFDP_array(16#107F#) := 16#23#;
--         SFDP_array(16#1080#) := 16#00#;
--         SFDP_array(16#1081#) := 16#23#;
--         SFDP_array(16#1082#) := 16#00#;
--         -- CFI Alternate Vendor-Specific Extended Query Parameter 90h
--         SFDP_array(16#1083#) := 16#90#;
--         SFDP_array(16#1084#) := 16#56#;
--         SFDP_array(16#1085#) := 16#06#;
--         SFDP_array(16#1086#) := 16#0E#;
--         SFDP_array(16#1087#) := 16#46#;
--         SFDP_array(16#1088#) := 16#43#;
--         SFDP_array(16#1089#) := 16#03#;
--         SFDP_array(16#108A#) := 16#13#;
--         SFDP_array(16#108B#) := 16#0B#;
--         SFDP_array(16#108C#) := 16#0C#;
--         SFDP_array(16#108D#) := 16#3B#;
--         SFDP_array(16#108E#) := 16#3C#;
--         SFDP_array(16#108F#) := 16#6B#;
--         SFDP_array(16#1090#) := 16#6C#;
--         SFDP_array(16#1091#) := 16#BB#;
--         SFDP_array(16#1092#) := 16#BC#;
--         SFDP_array(16#1093#) := 16#EB#;
--         SFDP_array(16#1094#) := 16#EC#;
--         SFDP_array(16#1095#) := 16#32#;
--         SFDP_array(16#1096#) := 16#03#;
--         SFDP_array(16#1097#) := 16#00#;
--         SFDP_array(16#1098#) := 16#00#;
--         SFDP_array(16#1099#) := 16#00#;
--         SFDP_array(16#109A#) := 16#00#;
--         SFDP_array(16#109B#) := 16#00#;
--         SFDP_array(16#109C#) := 16#00#;
--         SFDP_array(16#109D#) := 16#00#;
--         SFDP_array(16#109E#) := 16#00#;
--         SFDP_array(16#109F#) := 16#00#;
--         SFDP_array(16#10A0#) := 16#04#;
--         SFDP_array(16#10A1#) := 16#02#;
--         SFDP_array(16#10A2#) := 16#01#;
--         SFDP_array(16#10A3#) := 16#50#;
--         SFDP_array(16#10A4#) := 16#00#;
--         SFDP_array(16#10A5#) := 16#FF#;
--         SFDP_array(16#10A6#) := 16#FF#;
--         SFDP_array(16#10A7#) := 16#00#;
--         SFDP_array(16#10A8#) := 16#08#;
--         SFDP_array(16#10A9#) := 16#00#;
--         SFDP_array(16#10AA#) := 16#08#;
--         SFDP_array(16#10AB#) := 16#00#;
--         SFDP_array(16#10AC#) := 16#08#;
--         SFDP_array(16#10AD#) := 16#00#;
--         SFDP_array(16#10AE#) := 16#04#;
--         SFDP_array(16#10AF#) := 16#02#;
--         SFDP_array(16#10B0#) := 16#04#;
--         SFDP_array(16#10B1#) := 16#5A#;
--         SFDP_array(16#10B2#) := 16#01#;
--         SFDP_array(16#10B3#) := 16#FF#;
--         SFDP_array(16#10B4#) := 16#FF#;
--         SFDP_array(16#10B5#) := 16#00#;
--         SFDP_array(16#10B6#) := 16#08#;
--         SFDP_array(16#10B7#) := 16#00#;
--         SFDP_array(16#10B8#) := 16#08#;
--         SFDP_array(16#10B9#) := 16#00#;
--         SFDP_array(16#10BA#) := 16#08#;
--         SFDP_array(16#10BB#) := 16#00#;
--         SFDP_array(16#10BC#) := 16#05#;
--         SFDP_array(16#10BD#) := 16#02#;
--         SFDP_array(16#10BE#) := 16#04#;
--         SFDP_array(16#10BF#) := 16#68#;
--         SFDP_array(16#10C0#) := 16#02#;
--         SFDP_array(16#10C1#) := 16#FF#;
--         SFDP_array(16#10C2#) := 16#FF#;
--         SFDP_array(16#10C3#) := 16#00#;
--         SFDP_array(16#10C4#) := 16#08#;
--         SFDP_array(16#10C5#) := 16#00#;
--         SFDP_array(16#10C6#) := 16#08#;
--         SFDP_array(16#10C7#) := 16#00#;
--         SFDP_array(16#10C8#) := 16#08#;
--         SFDP_array(16#10C9#) := 16#00#;
--         SFDP_array(16#10CA#) := 16#06#;
--         SFDP_array(16#10CB#) := 16#02#;
--         SFDP_array(16#10CC#) := 16#05#;
--         SFDP_array(16#10CD#) := 16#85#;
--         SFDP_array(16#10CE#) := 16#02#;
--         SFDP_array(16#10CF#) := 16#FF#;
--         SFDP_array(16#10D0#) := 16#FF#;
--         SFDP_array(16#10D1#) := 16#00#;
--         SFDP_array(16#10D2#) := 16#08#;
--         SFDP_array(16#10D3#) := 16#FF#;
--         SFDP_array(16#10D4#) := 16#FF#;
--         -- CFI Alternate Vendor-Specific Extended Query Parameter 9Ah-HPLC
--         SFDP_array(16#10D5#) := 16#9A#;
--         SFDP_array(16#10D6#) := 16#2A#;
--         SFDP_array(16#10D7#) := 16#05#;
--         SFDP_array(16#10D8#) := 16#08#;
--         SFDP_array(16#10D9#) := 16#46#;
--         SFDP_array(16#10DA#) := 16#43#;
--         SFDP_array(16#10DB#) := 16#0D#;
--         SFDP_array(16#10DC#) := 16#0E#;
--         SFDP_array(16#10DD#) := 16#BD#;
--         SFDP_array(16#10DE#) := 16#BE#;
--         SFDP_array(16#10DF#) := 16#ED#;
--         SFDP_array(16#10E0#) := 16#EE#;
--         SFDP_array(16#10E1#) := 16#32#;
--         SFDP_array(16#10E2#) := 16#03#;
--         SFDP_array(16#10E3#) := 16#00#;
--         SFDP_array(16#10E4#) := 16#04#;
--         SFDP_array(16#10E5#) := 16#00#;
--         SFDP_array(16#10E6#) := 16#04#;
--         SFDP_array(16#10E7#) := 16#01#;
--         SFDP_array(16#10E8#) := 16#03#;
--         SFDP_array(16#10E9#) := 16#42#;
--         SFDP_array(16#10EA#) := 16#00#;
--         SFDP_array(16#10EB#) := 16#00#;
--         SFDP_array(16#10EC#) := 16#05#;
--         SFDP_array(16#10ED#) := 16#00#;
--         SFDP_array(16#10EE#) := 16#06#;
--         SFDP_array(16#10EF#) := 16#01#;
--         SFDP_array(16#10F0#) := 16#06#;
--         SFDP_array(16#10F1#) := 16#42#;
--         SFDP_array(16#10F2#) := 16#01#;
--         SFDP_array(16#10F3#) := 16#00#;
--         SFDP_array(16#10F4#) := 16#06#;
--         SFDP_array(16#10F5#) := 16#00#;
--         SFDP_array(16#10F6#) := 16#07#;
--         SFDP_array(16#10F7#) := 16#01#;
--         SFDP_array(16#10F8#) := 16#07#;
--         SFDP_array(16#10F9#) := 16#42#;
--         SFDP_array(16#10FA#) := 16#02#;
--         SFDP_array(16#10FB#) := 16#00#;
--         SFDP_array(16#10FC#) := 16#07#;
--         SFDP_array(16#10FD#) := 16#00#;
--         SFDP_array(16#10FE#) := 16#08#;
--         SFDP_array(16#10FF#) := 16#01#;
--         SFDP_array(16#1100#) := 16#08#;
--         -- CFI Alternate Vendor-Specific Extended Query Parameter 90h-EHPLC
--         SFDP_array(16#1101#) := 16#90#;
--         SFDP_array(16#1102#) := 16#56#;
--         SFDP_array(16#1103#) := 16#06#;
--         SFDP_array(16#1104#) := 16#0E#;
--         SFDP_array(16#1105#) := 16#46#;
--         SFDP_array(16#1106#) := 16#43#;
--         SFDP_array(16#1107#) := 16#03#;
--         SFDP_array(16#1108#) := 16#13#;
--         SFDP_array(16#1109#) := 16#0B#;
--         SFDP_array(16#110A#) := 16#0C#;
--         SFDP_array(16#110B#) := 16#3B#;
--         SFDP_array(16#110C#) := 16#3C#;
--         SFDP_array(16#110D#) := 16#6B#;
--         SFDP_array(16#110E#) := 16#6C#;
--         SFDP_array(16#110F#) := 16#BB#;
--         SFDP_array(16#1110#) := 16#BC#;
--         SFDP_array(16#1111#) := 16#EB#;
--         SFDP_array(16#1112#) := 16#EC#;
--         SFDP_array(16#1113#) := 16#32#;
--         SFDP_array(16#1114#) := 16#03#;
--         SFDP_array(16#1115#) := 16#00#;
--         SFDP_array(16#1116#) := 16#00#;
--         SFDP_array(16#1117#) := 16#00#;
--         SFDP_array(16#1118#) := 16#00#;
--         SFDP_array(16#1119#) := 16#00#;
--         SFDP_array(16#111A#) := 16#00#;
--         SFDP_array(16#111B#) := 16#00#;
--         SFDP_array(16#111C#) := 16#00#;
--         SFDP_array(16#111D#) := 16#04#;
--         SFDP_array(16#111E#) := 16#00#;
--         SFDP_array(16#111F#) := 16#02#;
--         SFDP_array(16#1120#) := 16#01#;
--         SFDP_array(16#1121#) := 16#50#;
--         SFDP_array(16#1122#) := 16#00#;
--         SFDP_array(16#1123#) := 16#FF#;
--         SFDP_array(16#1124#) := 16#FF#;
--         SFDP_array(16#1125#) := 16#00#;
--         SFDP_array(16#1126#) := 16#08#;
--         SFDP_array(16#1127#) := 16#00#;
--         SFDP_array(16#1128#) := 16#08#;
--         SFDP_array(16#1129#) := 16#00#;
--         SFDP_array(16#112A#) := 16#08#;
--         SFDP_array(16#112B#) := 16#04#;
--         SFDP_array(16#112C#) := 16#00#;
--         SFDP_array(16#112D#) := 16#02#;
--         SFDP_array(16#112E#) := 16#04#;
--         SFDP_array(16#112F#) := 16#5A#;
--         SFDP_array(16#1130#) := 16#01#;
--         SFDP_array(16#1131#) := 16#FF#;
--         SFDP_array(16#1132#) := 16#FF#;
--         SFDP_array(16#1133#) := 16#00#;
--         SFDP_array(16#1134#) := 16#08#;
--         SFDP_array(16#1135#) := 16#00#;
--         SFDP_array(16#1136#) := 16#08#;
--         SFDP_array(16#1137#) := 16#00#;
--         SFDP_array(16#1138#) := 16#08#;
--         SFDP_array(16#1139#) := 16#04#;
--         SFDP_array(16#113A#) := 16#01#;
--         SFDP_array(16#113B#) := 16#02#;
--         SFDP_array(16#113C#) := 16#04#;
--         SFDP_array(16#113D#) := 16#68#;
--         SFDP_array(16#113E#) := 16#02#;
--         SFDP_array(16#113F#) := 16#FF#;
--         SFDP_array(16#1140#) := 16#FF#;
--         SFDP_array(16#1141#) := 16#00#;
--         SFDP_array(16#1142#) := 16#08#;
--         SFDP_array(16#1143#) := 16#00#;
--         SFDP_array(16#1144#) := 16#08#;
--         SFDP_array(16#1145#) := 16#00#;
--         SFDP_array(16#1146#) := 16#08#;
--         SFDP_array(16#1147#) := 16#04#;
--         SFDP_array(16#1148#) := 16#02#;
--         SFDP_array(16#1149#) := 16#02#;
--         SFDP_array(16#114A#) := 16#05#;
--         SFDP_array(16#114B#) := 16#85#;
--         SFDP_array(16#114C#) := 16#02#;
--         SFDP_array(16#114D#) := 16#FF#;
--         SFDP_array(16#114E#) := 16#FF#;
--         SFDP_array(16#114F#) := 16#00#;
--         SFDP_array(16#1150#) := 16#08#;
--         SFDP_array(16#1151#) := 16#FF#;
--         SFDP_array(16#1152#) := 16#FF#;
--         SFDP_array(16#1153#) := 16#FF#;
--         SFDP_array(16#1154#) := 16#FF#;
--         SFDP_array(16#1155#) := 16#FF#;
--         SFDP_array(16#1156#) := 16#FF#;
--         SFDP_array(16#1157#) := 16#FF#;
--         SFDP_array(16#1158#) := 16#FF#;
--         -- CFI Alternate Vendor-Specific Extended Query Parameter 9Ah-EHPLC
--         SFDP_array(16#1159#) := 16#9A#;
--         SFDP_array(16#115A#) := 16#2A#;
--         SFDP_array(16#115B#) := 16#05#;
--         SFDP_array(16#115C#) := 16#08#;
--         SFDP_array(16#115D#) := 16#46#;
--         SFDP_array(16#115E#) := 16#43#;
--         SFDP_array(16#115F#) := 16#0D#;
--         SFDP_array(16#1160#) := 16#0E#;
--         SFDP_array(16#1161#) := 16#BD#;
--         SFDP_array(16#1162#) := 16#ED#;
--         SFDP_array(16#1163#) := 16#EE#;
--         SFDP_array(16#1164#) := 16#32#;
--         SFDP_array(16#1165#) := 16#03#;
--         SFDP_array(16#1166#) := 16#04#;
--         SFDP_array(16#1167#) := 16#01#;
--         SFDP_array(16#1168#) := 16#02#;
--         SFDP_array(16#1169#) := 16#02#;
--         SFDP_array(16#116A#) := 16#01#;
--         SFDP_array(16#116B#) := 16#03#;
--         SFDP_array(16#116C#) := 16#42#;
--         SFDP_array(16#116D#) := 16#00#;
--         SFDP_array(16#116E#) := 16#04#;
--         SFDP_array(16#116F#) := 16#02#;
--         SFDP_array(16#1170#) := 16#02#;
--         SFDP_array(16#1171#) := 16#04#;
--         SFDP_array(16#1172#) := 16#01#;
--         SFDP_array(16#1173#) := 16#06#;
--         SFDP_array(16#1174#) := 16#42#;
--         SFDP_array(16#1175#) := 16#01#;
--         SFDP_array(16#1176#) := 16#04#;
--         SFDP_array(16#1177#) := 16#04#;
--         SFDP_array(16#1178#) := 16#02#;
--         SFDP_array(16#1179#) := 16#05#;
--         SFDP_array(16#117A#) := 16#01#;
--         SFDP_array(16#117B#) := 16#07#;
--         SFDP_array(16#117C#) := 16#42#;
--         SFDP_array(16#117D#) := 16#02#;
--         SFDP_array(16#117E#) := 16#04#;
--         SFDP_array(16#117F#) := 16#05#;
--         SFDP_array(16#1180#) := 16#02#;
--         SFDP_array(16#1181#) := 16#06#;
--         SFDP_array(16#1182#) := 16#01#;
--         SFDP_array(16#1183#) := 16#08#;
--         -- CFI Alternate Vendor-Specific Extended Query Parameter F0h RFU
--         SFDP_array(16#1184#) := 16#F0#;
--         SFDP_array(16#1185#) := 16#0F#;
--         SFDP_array(16#1186#) := 16#FF#;
--         SFDP_array(16#1187#) := 16#FF#;
--         SFDP_array(16#1188#) := 16#FF#;
--         SFDP_array(16#1189#) := 16#FF#;
--         SFDP_array(16#118A#) := 16#FF#;
--         SFDP_array(16#118B#) := 16#FF#;
--         SFDP_array(16#118C#) := 16#FF#;
--         SFDP_array(16#118D#) := 16#FF#;
--         SFDP_array(16#118E#) := 16#FF#;
--         SFDP_array(16#118F#) := 16#FF#;
--         SFDP_array(16#1190#) := 16#FF#;
--         SFDP_array(16#1191#) := 16#FF#;
--         SFDP_array(16#1192#) := 16#FF#;
--         SFDP_array(16#1193#) := 16#FF#;
--         SFDP_array(16#1194#) := 16#FF#;
--         -- CFI Alternate Vendor-Specific Extended Query Parameter A5h
--         -- Jedec SFDP Rev B
--         SFDP_array(16#1195#) := 16#A5#;
--         SFDP_array(16#1196#) := 16#50#;
--         SFDP_array(16#1197#) := 16#E7#;
--         SFDP_array(16#1198#) := 16#FF#;
--         SFDP_array(16#1199#) := 16#F3#;
--         SFDP_array(16#119A#) := 16#FF#;
--         SFDP_array(16#119B#) := 16#FF#;
--         SFDP_array(16#119C#) := 16#FF#;
--         SFDP_array(16#119D#) := 16#FF#;
--         SFDP_array(16#119E#) := 16#1F#;
--         SFDP_array(16#119F#) := 16#44#;
--         SFDP_array(16#11A0#) := 16#EB#;
--         SFDP_array(16#11A1#) := 16#08#;
--         SFDP_array(16#11A2#) := 16#6B#;
--         SFDP_array(16#11A3#) := 16#08#;
--         SFDP_array(16#11A4#) := 16#3B#;
--         IF EHP THEN
--             SFDP_array(16#11A5#) := 16#80#;
--         ELSE
--             SFDP_array(16#11A5#) := 16#04#;
--         END IF;
--         SFDP_array(16#11A6#) := 16#BB#;
--         SFDP_array(16#11A7#) := 16#EE#;
--         SFDP_array(16#11A8#) := 16#FF#;
--         SFDP_array(16#11A9#) := 16#FF#;
--         SFDP_array(16#11AA#) := 16#FF#;
--         SFDP_array(16#11AB#) := 16#FF#;
--         SFDP_array(16#11AC#) := 16#FF#;
--         SFDP_array(16#11AD#) := 16#FF#;
--         SFDP_array(16#11AE#) := 16#FF#;
--         SFDP_array(16#11AF#) := 16#FF#;
--         SFDP_array(16#11B0#) := 16#FF#;
--         SFDP_array(16#11B1#) := 16#FF#;
--         SFDP_array(16#11B2#) := 16#EB#;
--         SFDP_array(16#11B3#) := 16#00#;
--         SFDP_array(16#11B4#) := 16#FF#;
--         SFDP_array(16#11B5#) := 16#00#;
--         SFDP_array(16#11B6#) := 16#FF#;
--         SFDP_array(16#11B7#) := 16#12#;
--         SFDP_array(16#11B8#) := 16#D8#;
--         SFDP_array(16#11B9#) := 16#00#;
--         SFDP_array(16#11BA#) := 16#FF#;
--         SFDP_array(16#11BB#) := 16#F2#;
--         SFDP_array(16#11BC#) := 16#FF#;
--         SFDP_array(16#11BD#) := 16#0F#;
--         SFDP_array(16#11BE#) := 16#FF#;
--         SFDP_array(16#11BF#) := 16#91#;
--         SFDP_array(16#11C0#) := 16#25#;
--         SFDP_array(16#11C1#) := 16#07#;
--         SFDP_array(16#11C2#) := 16#D9#;
--         SFDP_array(16#11C3#) := 16#EC#;
--         SFDP_array(16#11C4#) := 16#83#;
--         SFDP_array(16#11C5#) := 16#18#;
--         SFDP_array(16#11C6#) := 16#45#;
--         SFDP_array(16#11C7#) := 16#8A#;
--         SFDP_array(16#11C8#) := 16#85#;
--         SFDP_array(16#11C9#) := 16#7A#;
--         SFDP_array(16#11CA#) := 16#75#;
--         SFDP_array(16#11CB#) := 16#F7#;
--         SFDP_array(16#11CC#) := 16#FF#;
--         SFDP_array(16#11CD#) := 16#FF#;
--         SFDP_array(16#11CE#) := 16#FF#;
--         SFDP_array(16#11CF#) := 16#00#;
--         SFDP_array(16#11D0#) := 16#F6#;
--         SFDP_array(16#11D1#) := 16#5D#;
--         SFDP_array(16#11D2#) := 16#FF#;
--         SFDP_array(16#11D3#) := 16#F0#;
--         SFDP_array(16#11D4#) := 16#28#;
--         SFDP_array(16#11D5#) := 16#FA#;
--         SFDP_array(16#11D6#) := 16#A8#;
--         SFDP_array(16#11D7#) := 16#FF#;
--         SFDP_array(16#11D8#) := 16#00#;
--         SFDP_array(16#11D9#) := 16#00#;
--         SFDP_array(16#11DA#) := 16#FF#;
--         SFDP_array(16#11DB#) := 16#F4#;
--         SFDP_array(16#11DC#) := 16#FF#;
--         SFDP_array(16#11DD#) := 16#FF#;
--         SFDP_array(16#11DE#) := 16#3F#;
--         SFDP_array(16#11DF#) := 16#FF#;
--         SFDP_array(16#11E0#) := 16#E8#;
--         SFDP_array(16#11E1#) := 16#FF#;
--         SFDP_array(16#11E2#) := 16#FF#;
--         SFDP_array(16#11E3#) := 16#FF#;
--         SFDP_array(16#11E4#) := 16#FF#;
--         SFDP_array(16#11E5#) := 16#DC#;
--         SFDP_array(16#11E6#) := 16#FF#;
--         WAIT;
--     END PROCESS SFDPPreload;

--     AspRegInit: PROCESS(ASP_INIT)
--     BEGIN
--         IF ASP_INIT = 0 THEN
--             ASP_reg := to_slv(16#FE4F#,16);
--         ELSE
--             ASP_reg := to_slv(16#FE7F#,16);
--         END IF;
--     END PROCESS AspRegInit;

--     Protect : PROCESS(change_BP)
--     BEGIN
--         IF rising_edge(change_BP) THEN

--             CASE BP_bits IS
--                 WHEN "000" =>
--                     Sec_Prot := (OTHERS => '0');
--                 WHEN "001" =>
--                     IF TBPROT = '0' THEN
--                         Sec_Prot(SecNum downto (SecNum+1)*63/64)
--                                                     := (OTHERS => '1');
--                         Sec_Prot((SecNum+1)*63/64 - 1 downto 0)
--                                                     := (OTHERS => '0');
--                     ELSE
--                         Sec_Prot((SecNum+1)/64 - 1 downto 0)
--                                                     := (OTHERS => '1');
--                         Sec_Prot(SecNum downto (SecNum+1)/64)
--                                                     := (OTHERS => '0');
--                     END IF;
--                 WHEN "010" =>
--                     IF TBPROT =  '0' THEN
--                         Sec_Prot(SecNum downto (SecNum+1)*31/32)
--                                                     := (OTHERS => '1');
--                         Sec_Prot((SecNum+1)*31/32 - 1 downto 0)
--                                                     := (OTHERS => '0');
--                     ELSE
--                         Sec_Prot((SecNum+1)/32 - 1 downto 0)
--                                                     := (others => '1');
--                         Sec_Prot(SecNum downto (SecNum+1)/32)
--                                                     := (OTHERS => '0');
--                     END IF;
--                 WHEN "011" =>
--                     IF TBPROT =  '0' THEN
--                         Sec_Prot(SecNum downto (SecNum+1)*15/16)
--                                                     := (OTHERS => '1');
--                         Sec_Prot((SecNum+1)*15/16 - 1 downto 0)
--                                                     := (OTHERS => '0');
--                     ELSE
--                         Sec_Prot((SecNum+1)/16 - 1 downto 0)
--                                                     := (OTHERS => '1');
--                         Sec_Prot(SecNum downto (SecNum+1)/16)
--                                                     := (OTHERS => '0');
--                     END IF;
--                 WHEN "100" =>
--                     IF TBPROT =  '0' THEN
--                         Sec_Prot(SecNum downto (SecNum+1)*7/8)
--                                                     := (OTHERS => '1');
--                         Sec_Prot((SecNum+1)*7/8 - 1 downto 0)
--                                                     := (OTHERS => '0');
--                     ELSE
--                         Sec_Prot((SecNum+1)/8 - 1 downto 0) := (OTHERS => '1');
--                         Sec_Prot(SecNum downto (SecNum+1)/8):= (OTHERS => '0');
--                     END IF;
--                 WHEN "101" =>
--                     IF TBPROT =  '0' THEN
--                         Sec_Prot(SecNum downto (SecNum+1)*3/4)
--                                                     := (OTHERS => '1');
--                         Sec_Prot((SecNum+1)*3/4 - 1 downto 0)
--                                                     := (OTHERS => '0');
--                     ELSE
--                         Sec_Prot((SecNum+1)/4 - 1 downto 0) := (OTHERS => '1');
--                         Sec_Prot(SecNum downto (SecNum+1)/4):= (OTHERS => '0');
--                     END IF;

--                 WHEN "110" =>
--                     IF TBPROT =  '0' THEN
--                         Sec_Prot(SecNum downto (SecNum+1)/2):= (OTHERS => '1');
--                         Sec_Prot((SecNum+1)/2 - 1 downto 0) := (OTHERS => '0');
--                     ELSE
--                         Sec_Prot((SecNum+1)/2 - 1 downto 0) := (OTHERS => '1');
--                         Sec_Prot(SecNum downto (SecNum+1)/2):= (OTHERS => '0');
--                     END IF;

--                 WHEN OTHERS =>
--                     Sec_Prot := (OTHERS => '1');
--             END CASE;
--         END IF;
--     END PROCESS Protect;

--     HOLD_FRAME_ON_PO_ZD : PROCESS(SOOut_zd, SIOut_zd, HOLDNeg_pullup)
--     BEGIN
--         IF (HOLDNeg_pullup = '0' AND QUAD /= '1') THEN
--             hold_mode := TRUE;
--             SIOut_z <= 'Z';
--             SOOut_z <= 'Z';
--         ELSE
--             IF hold_mode THEN
--                 SIOut_z <= SIOut_zd AFTER tpd_HOLDNeg_oSO(trz0);
--                 SOOut_z <= SOOut_zd AFTER tpd_HOLDNeg_oSO(trz0);
--                 hold_mode := FALSE;
--             ELSE
--                 SIOut_z <= SIOut_zd;
--                 SOOut_z <= SOOut_zd;
--                 hold_mode := FALSE;
--             END IF;
--         END IF;
--     END PROCESS HOLD_FRAME_ON_PO_ZD;

--     HOLD_PULL_UP : PROCESS(HOLDNegIn)
--     BEGIN
--         IF (QUAD = '0') THEN
--             IF (HOLDNegIn = 'Z') THEN
--                 HOLDNeg_pullup <= '1';
--             ELSE
--                 HOLDNeg_pullup <= HOLDNegIn;
--             END IF;
--         END IF;
--     END PROCESS HOLD_PULL_UP;

--     WP_PULL_UP : PROCESS(WPNegIn)
--     BEGIN
--         IF (QUAD = '0') THEN
--             IF (WPNegIn = 'Z') THEN
--                 WPNeg_pullup <= '1';
--             ELSE
--                 WPNeg_pullup <= WPNegIn;
--             END IF;
--         END IF;
--     END PROCESS WP_PULL_UP;

--     RST_PULL_UP : PROCESS(RSTNeg)
--     BEGIN
--         IF (RSTNeg = 'Z') THEN
--             RSTNeg_pullup <= '1';
--         ELSE
--             RSTNeg_pullup <= RSTNeg;
--         END IF;
--     END PROCESS RST_PULL_UP;

--     ---------------------------------------------------------------------------
--     ---- File Read Section - Preload Control
--     ---------------------------------------------------------------------------
--     MemPreload : PROCESS

--         -- text file input variables
--         FILE mem_file         : text  is  mem_file_name;
--         FILE otp_file         : text  is  otp_file_name;
--         VARIABLE ind          : NATURAL RANGE 0 TO AddrRANGE := 0;
--         VARIABLE S_ind        : NATURAL RANGE 0 TO SecNum:= 0;
--         VARIABLE index        : NATURAL RANGE 0 TO SecSize:=0;
--         VARIABLE otp_ind      : NATURAL RANGE 16#000# TO 16#3FF# := 16#000#;
--         VARIABLE buf          : line;
--         VARIABLE reported     : NATURAL;
--         VARIABLE mem_data     : INTEGER;

--     BEGIN
--     ---------------------------------------------------------------------------
--     --s25fl512s memory preload file format
-- -----------------------------------
--     ---------------------------------------------------------------------------
--     --   /       - comment
--     --   @aaaaaa - <aaaaaa> stands for address
--     --   dd      - <dd> is byte to be written at Mem(aaaaaa++)
--     --             (aaaaaa is incremented at every load)
--     --   only first 1-7 columns are loaded. NO empty lines !!!!!!!!!!!!!!!!
--     ---------------------------------------------------------------------------
--          -- memory preload
--         configure_memory(16#FF#);
--         initialize;
-- --         LongT;

--         IF (mem_file_name /= "none" AND UserPreload ) THEN
--             ind := 0;
--             reported := 0;
--             WHILE (not ENDFILE (mem_file)) LOOP
--                 READLINE (mem_file, buf);
--                 IF buf(1) = '/' THEN --comment
--                     NEXT;
--                 ELSIF buf(1) = '@' THEN --address
--                     ind := h(buf(2 to 8));
--                 ELSE
--                     IF ind <= AddrRANGE THEN
--                         mem_data := h(buf(1 to 2));
--                         IF ind=0 THEN
--                             S_ind := 0;
--                             index := 0;
--                         ELSIF ind < SecSize+1  THEN
--                             S_ind := 0;
--                             index := ind;
--                         ELSE
--                             S_ind := NATURAL(ind / (SecSize +1));
--                             index := ind - S_ind*(SecSize+1);
--                         END IF;
--                         WRITE_DATA(S_ind,index,mem_data);
--                         IF ind < AddrRANGE THEN
--                             ind := ind + 1;
--                         END IF;
--                     ELSIF reported = 0 THEN
--                         REPORT " Memory address out of range"
--                         SEVERITY warning;
--                         reported := 1;
--                     END IF;
--                 END IF;
--             END LOOP;
--         END IF;

--     ---------------------------------------------------------------------------
--     --s25fl512s_otp memory preload file format
--     ---------------------------------------------------------------------------
--     --   /       - comment
--     --   @aaa - <aaa> stands for address
--     --   dd      - <dd> is byte to be written at OTPMem(aaa++)
--     --             (aaa is incremented at every load)
--     --   only first 1-4 columns are loaded. NO empty lines !!!!!!!!!!!!!!!!
--     ---------------------------------------------------------------------------

--          -- memory preload
--         IF (otp_file_name /= "none" AND UserPreload) THEN
--             otp_ind := 16#000#;
--             OTPMem := (OTHERS => MaxData);
--             WHILE (not ENDFILE (otp_file)) LOOP
--                 READLINE (otp_file, buf);
--                 IF buf(1) = '/' THEN
--                     NEXT;
--                 ELSIF buf(1) = '@' THEN
--                     IF otp_ind > 16#3FF# OR otp_ind < 16#000# THEN
--                         ASSERT false
--                             REPORT "Given preload address is out of" &
--                                    "OTP address range"
--                             SEVERITY warning;
--                     ELSE
--                         otp_ind := h(buf(2 to 4)); --address
--                     END IF;
--                 ELSE
--                     OTPMem(otp_ind) := h(buf(1 to 2));
--                     otp_ind := otp_ind + 1;
--                 END IF;
--             END LOOP;
--         END IF;

--         LOCK_BYTE1 := to_slv(OTPMem(16#10#),8);
--         LOCK_BYTE2 := to_slv(OTPMem(16#11#),8);
--         LOCK_BYTE3 := to_slv(OTPMem(16#12#),8);
--         LOCK_BYTE4 := to_slv(OTPMem(16#13#),8);

--         WAIT;
--     END PROCESS MemPreload;

--     ----------------------------------------------------------------------------
--     -- Path Delay Section
--     ----------------------------------------------------------------------------

--     SO_Out_PathDelay_Gen : PROCESS(SOOut_z)

--             VARIABLE SO_GlitchData : VitalGlitchDataType;
--         BEGIN
--             VitalPathDelay01Z (
--                 OutSignal       => SOOut,
--                 OutSignalName   => "oSO",
--                 OutTemp         => SOOut_z,
--                 Mode            => VitalTransport,
--                 GlitchData      => SO_GlitchData,
--                 Paths           => (
--                     0 => (InputChangeTime => SCK_ipd'LAST_EVENT,
--                         PathDelay => VitalExtendtofillDelay(tpd_SCK_oSO_normal),
--                         PathCondition   => NOT(ddr)),
--                     1 => (InputChangeTime => SCK_ipd'LAST_EVENT,
--                         PathDelay   => VitalExtendtofillDelay(tpd_SCK_oSO_DDR),
--                         PathCondition   => (ddr OR fast_rd)),
--                     2 => (InputChangeTime => CSNeg_ipd'LAST_EVENT,
--                         PathDelay       => tpd_CSNeg_oSO,
--                         PathCondition   => CSNeg_ipd = '1'),
--                     3 => (InputChangeTime => HOLDNegIn'LAST_EVENT,
--                         PathDelay       => tpd_HOLDNeg_oSO,
--                         PathCondition   => QUAD = '0')
--                 )
--             );
--         END PROCESS;

--     SI_Out_PathDelay : PROCESS(SIOut_z)

--             VARIABLE SI_GlitchData : VitalGlitchDataType;
--         BEGIN
--             VitalPathDelay01Z (
--                 OutSignal       => SIOut,
--                 OutSignalName   => "oSI",
--                 OutTemp         => SIOut_z,
--                 Mode            => VitalTransport,
--                 GlitchData      => SI_GlitchData,
--                 Paths           => (
--                     0 => (InputChangeTime => SCK_ipd'LAST_EVENT,
--                         PathDelay => VitalExtendtofillDelay(tpd_SCK_oSO_normal),
--                         PathCondition => dual AND NOT(ddr)),
--                     1 => (InputChangeTime => SCK_ipd'LAST_EVENT,
--                         PathDelay   => VitalExtendtofillDelay(tpd_SCK_oSO_DDR),
--                         PathCondition   => dual AND ddr),
--                     2 => (InputChangeTime => CSNeg_ipd'LAST_EVENT,
--                         PathDelay       => tpd_CSNeg_oSO,
--                         PathCondition   => CSNeg_ipd = '1'),
--                     3 => (InputChangeTime => HOLDNegIn'LAST_EVENT,
--                         PathDelay       => tpd_HOLDNeg_oSO,
--                         PathCondition   => dual AND QUAD = '0')
--                 )
--             );
--         END PROCESS;

--     HOLD_Out_PathDelay : PROCESS(HOLDNegOut_zd)

--             VARIABLE HOLD_GlitchData : VitalGlitchDataType;
--         BEGIN
--             VitalPathDelay01Z (
--                 OutSignal       => HOLDNegOut,
--                 OutSignalName   => "oHOLDNeg",
--                 OutTemp         => HOLDNegOut_zd,
--                 Mode            => VitalTransport,
--                 GlitchData      => HOLD_GlitchData,
--                 Paths           => (
--                     0 => (InputChangeTime => SCK_ipd'LAST_EVENT,
--                         PathDelay => VitalExtendtofillDelay(tpd_SCK_oSO_normal),
--                         PathCondition   => dual AND not(ddr) AND QUAD = '1'),
--                     1 => (InputChangeTime => SCK_ipd'LAST_EVENT,
--                         PathDelay => VitalExtendtofillDelay(tpd_SCK_oSO_DDR),
--                         PathCondition   => ddr AND QUAD = '1'),
--                     2 => (InputChangeTime => CSNeg_ipd'LAST_EVENT,
--                         PathDelay       => tpd_CSNeg_oSO,
--                         PathCondition   => CSNeg_ipd = '1' AND
--                                            HOLDNegOut_zd = 'Z' AND QUAD = '1')
--                 )
--             );
--         END PROCESS;

--     WP_Out_PathDelay : PROCESS(WPNegOut_zd)

--             VARIABLE WP_GlitchData : VitalGlitchDataType;
--         BEGIN
--             VitalPathDelay01Z (
--                 OutSignal       => WPNegOut,
--                 OutSignalName   => "oWPNeg",
--                 OutTemp         => WPNegOut_zd,
--                 Mode            => VitalTransport,
--                 GlitchData      => WP_GlitchData,
--                 Paths           => (
--                     0 => (InputChangeTime => SCK_ipd'LAST_EVENT,
--                         PathDelay => VitalExtendtofillDelay(tpd_SCK_oSO_normal),
--                         PathCondition   => dual AND not(ddr) AND QUAD = '1'),
--                     1 => (InputChangeTime => SCK_ipd'LAST_EVENT,
--                         PathDelay => VitalExtendtofillDelay(tpd_SCK_oSO_DDR),
--                         PathCondition   => ddr AND QUAD = '1'),
--                     2 => (InputChangeTime => CSNeg_ipd'LAST_EVENT,
--                         PathDelay       => tpd_CSNeg_oSO,
--                         PathCondition   => CSNeg_ipd = '1' AND
--                                            WPNegOut_zd = 'Z' AND QUAD = '1')
--                 )
--             );
--         END PROCESS;

--     END BLOCK behavior;
-- END vhdl_behavioral_dynamic_memory_allocation;
ARCHITECTURE vhdl_behavioral_static_memory_allocation of s25fl512s IS
    ATTRIBUTE VITAL_LEVEL0 OF
    vhdl_behavioral_static_memory_allocation : ARCHITECTURE IS TRUE;

    ---------------------------------------------------------------------------
    -- CONSTANT AND SIGNAL DECLARATION
    ---------------------------------------------------------------------------
    --Declaration of constants - memory characteristics
        -- The constant declared here are used to enable the creation of models
        -- of memories within a family with a minimum amount of editing

    CONSTANT PartID        : STRING  := "s25fl512s";
    CONSTANT MaxData       : NATURAL := 16#FF#;        --255;
    CONSTANT MemSize       : NATURAL := 16#3FFFFFF#;
    CONSTANT SecNum        : NATURAL := 255;
    CONSTANT SecSize       : NATURAL := 16#3FFFF#;     --256KB
    CONSTANT PageNum       : NATURAL := 16#1FFFF#;
    CONSTANT PageSize      : NATURAL := 512;
    CONSTANT AddrRANGE     : NATURAL := 16#3FFFFFF#;
    CONSTANT HiAddrBit     : NATURAL := 31;
    CONSTANT OTPSize       : NATURAL := 1023;
    CONSTANT OTPLoAddr     : NATURAL := 16#000#;
    CONSTANT OTPHiAddr     : NATURAL := 16#3FF#;
    CONSTANT BYTE          : NATURAL := 8;
    CONSTANT SFDPHiAddr    : NATURAL := 16#11E6#;
    CONSTANT IDCFILength   : NATURAL := 16#1E6#;

    --Manufacturer Identification
    CONSTANT Manuf_ID      : NATURAL := 16#01#;
    CONSTANT DeviceID      : NATURAL := 16#19#;
    --Electronic Signature
    CONSTANT ESignature    : NATURAL := 16#19#;
    --Device ID
    --Manufacturer Identification && Memory Type && Memory Capacity
    CONSTANT Jedec_ID      : NATURAL := 16#01#; -- first byte of Device ID
    CONSTANT DeviceID1     : NATURAL := 16#02#;
    CONSTANT DeviceID2     : NATURAL := 16#20#;
    CONSTANT ExtendedBytes : NATURAL := 16#4D#;
    CONSTANT ExtendedID    : NATURAL := 16#00#;
    CONSTANT DieRev        : NATURAL := 16#00#;
    CONSTANT MaskRev       : NATURAL := 16#00#;
    CONSTANT HOLD_CSNeg_RSTNeg : TIME := 34800 ns;

    -- Declaration of signals that will hold the delayed values of ports
    SIGNAL SI_ipd          : std_ulogic := 'U';
    SIGNAL SO_ipd          : std_ulogic := 'U';
    SIGNAL SCK_ipd         : std_ulogic := 'U';
    SIGNAL CSNeg_ipd       : std_ulogic := 'U';
    SIGNAL RSTNeg_ipd      : std_ulogic := 'U';
    SIGNAL WPNeg_ipd       : std_ulogic := 'U';
    SIGNAL HOLDNeg_ipd     : std_ulogic := 'U';
    SIGNAL HOLDNeg_pullup  : std_ulogic := 'U';
    SIGNAL WPNeg_pullup    : std_ulogic := 'U';
    SIGNAL RSTNeg_pullup   : std_ulogic := 'U';

    -- internal delays
    SIGNAL PP_in           : std_ulogic := '0';
    SIGNAL PP_out          : std_ulogic := '0';
    SIGNAL BP_in           : std_ulogic := '0';
    SIGNAL BP_out          : std_ulogic := '0';
    SIGNAL SE_in           : std_ulogic := '0';
    SIGNAL SE_out          : std_ulogic := '0';
    SIGNAL BE_in           : std_ulogic := '0';
    SIGNAL BE_out          : std_ulogic := '0';
    SIGNAL WRR_in          : std_ulogic := '0';
    SIGNAL WRR_out         : std_ulogic := '0';
    SIGNAL ERSSUSP_in      : std_ulogic := '0';
    SIGNAL ERSSUSP_out     : std_ulogic := '0';
    SIGNAL ERSSUSP_out_sdf : std_ulogic := '0';
    SIGNAL PRGSUSP_in      : std_ulogic := '0';
    SIGNAL PRGSUSP_out     : std_ulogic := '0';
    SIGNAL PRGSUSP_out_sdf : std_ulogic := '0';
    SIGNAL PU_in           : std_ulogic := '0';
    SIGNAL PU_out          : std_ulogic := '0';
    SIGNAL RST_in          : std_ulogic := '0';-- Hardware Reset Timeout
    SIGNAL RST_out         : std_ulogic := '1';--
    SIGNAL PPBERASE_in     : std_ulogic := '0';
    SIGNAL PPBERASE_out    : std_ulogic := '0';
    SIGNAL PPBERASE_out_sdf    : std_ulogic := '0';
    SIGNAL PASSULCK_in     : std_ulogic := '0';
    SIGNAL PASSULCK_out    : std_ulogic := '0';
    SIGNAL PASSULCK_out_sdf    : std_ulogic := '0';
    SIGNAL PASSACC_in      : std_ulogic := '0';
    SIGNAL PASSACC_out     : std_ulogic := '0';
    SIGNAL PASSACC_out_sdf : std_ulogic := '0';


    SHARED VARIABLE LongCheck        : NATURAL;


    FUNCTION ReturnSectorID(ADDR : NATURAL) RETURN NATURAL IS
            VARIABLE result : NATURAL;
        BEGIN
            IF ADDR <= AddrRange THEN
                result := ADDR / (SecSize+1);
            ELSE
                result := SecNum+1;
            END IF;
            RETURN result;
        END ReturnSectorID;

    FUNCTION ReturnSectorIDRdPswdMd(TBPROT : std_logic) RETURN NATURAL IS
            VARIABLE result : NATURAL;
        BEGIN
            result := SecNum;
            IF TBPROT = '0' THEN
                result := 0;
            END IF;
            RETURN result;
        END ReturnSectorIDRdPswdMd;

BEGIN

    ---------------------------------------------------------------------------
    -- Internal Delays

    ---------------------------------------------------------------------------
    -- Artificial VITAL primitives to incorporate internal delays
    -- Because a tdevice generics is used, there must be a VITAL_primitives
    -- assotiated with them
    PP        :VitalBuf(PP_out,      PP_in,      (tdevice_PP      ,UnitDelay));
    BP        :VitalBuf(BP_out,      BP_in,      (tdevice_BP      ,UnitDelay));
    SE        :VitalBuf(SE_out,      SE_in,      (tdevice_SE      ,UnitDelay));
    BE        :VitalBuf(BE_out,      BE_in,      (tdevice_BE      ,UnitDelay));
    WRR       :VitalBuf(WRR_out,     WRR_in,     (tdevice_WRR     ,UnitDelay));
    ERSSUSP   :VitalBuf(ERSSUSP_out_sdf, ERSSUSP_in, (tdevice_ERSSUSP ,UnitDelay));
    PRGSUSP   :VitalBuf(PRGSUSP_out_sdf, PRGSUSP_in, (tdevice_PRGSUSP ,UnitDelay));
    PU        :VitalBuf(PU_out,      PU_in,      (tdevice_PU      ,UnitDelay));
    PPBERASE  :VitalBuf(PPBERASE_out_sdf,PPBERASE_in,(tdevice_PPBERASE,UnitDelay));
    PASSULCK  :VitalBuf(PASSULCK_out_sdf,PASSULCK_in,(tdevice_PASSULCK,UnitDelay));
    PASSACC   :VitalBuf(PASSACC_out_sdf, PASSACC_in, (tdevice_PASSACC ,UnitDelay));

    ---------------------------------------------------------------------------
    -- Wire Delays
    ---------------------------------------------------------------------------
    WireDelay : BLOCK
    BEGIN

        w_1 : VitalWireDelay (SI_ipd,      SI,      tipd_SI);
        w_2 : VitalWireDelay (SO_ipd,      SO,      tipd_SO);
        w_3 : VitalWireDelay (SCK_ipd,     SCK,     tipd_SCK);
        w_4 : VitalWireDelay (CSNeg_ipd,   CSNeg,   tipd_CSNeg);
        w_5 : VitalWireDelay (RSTNeg_ipd,  RSTNeg,  tipd_RSTNeg);
        w_6 : VitalWireDelay (WPNeg_ipd,   WPNeg,   tipd_WPNeg);
        w_7 : VitalWireDelay (HOLDNeg_ipd, HOLDNeg, tipd_HOLDNeg);

    END BLOCK;

    ---------------------------------------------------------------------------
    -- Main Behavior Block
    ---------------------------------------------------------------------------
    Behavior: BLOCK

        PORT (
            SIIn           : IN    std_ulogic := 'U';
            SIOut          : OUT   std_ulogic := 'U';
            SOIn           : IN    std_logic  := 'U';
            SOOut          : OUT   std_logic  := 'U';
            SCK            : IN    std_ulogic := 'U';
            CSNeg          : IN    std_ulogic := 'U';
            RSTNeg         : IN    std_ulogic := 'U';
            HOLDNegIn      : IN    std_ulogic := 'U';
            HOLDNegOut     : OUT   std_ulogic := 'U';
            WPNegIn        : IN    std_ulogic := 'U';
            WPNegOut       : OUT   std_ulogic := 'U'
        );

        PORT MAP (
             SIIn       => SI_ipd,
             SIOut      => oSI,
             SOIn       => SO_ipd,
             SOOut      => oSO,
             SCK        => SCK_ipd,
             CSNeg      => CSNeg_ipd,
             RSTNeg     => RSTNeg_ipd,
             HOLDNegIn  => HOLDNeg_ipd,
             HOLDNegOut => oHOLDNeg,
             WPNegIn    => WPNeg_ipd,
             WPNegOut   => oWPNeg
        );

        -- State Machine : State_Type
        TYPE state_type IS (IDLE,
                            RESET_STATE,
                            AUTOBOOT,
                            WRITE_SR,
                            PAGE_PG,
                            OTP_PG,
                            PG_SUSP,
                            SECTOR_ERS,
                            BULK_ERS,
                            ERS_SUSP,
                            ERS_SUSP_PG,
                            ERS_SUSP_PG_SUSP,
                            PASS_PG,
                            PASS_UNLOCK,
                            PPB_PG,
                            PPB_ERS,
                            AUTOBOOT_PG,
                            ASP_PG,
                            PLB_PG,
                            DYB_PG,
                            NVDLR_PG
                            );

        -- Instruction Type
        TYPE instruction_type IS ( NONE,
                                   WREN,       -- Write Enable
                                   WRDI,       -- Write Disable
                                   WRR,        -- Write Register
                                   READ,       -- Read Normal (3Byte Address)
                                   RD4,        -- Read Normal (4Byte +)
                                   OTPR,       -- OTP Read
                                   RDSR,       -- Read Status Register 1
                                   RDSR2,      -- Read Status Register 2
                                   RDCR,       -- Read Configuration Register 1
                                   REMS,       -- Read ID (SST)
                                   RDID,       -- Read ID JEDEC
                                   RES,        -- Read ID
                                   FSTRD,      -- Fast Read (3Byte Address)
                                   FSTRD4,     -- Fast Read (4Byte +)
                                   DDRFR,      -- Fast Read DDR (3Byte Address)
                                   DDRFR4,     -- Fast Read DDR (4Byte +)
                                   DOR,        -- Read Dual Out (3Byte Address)
                                   DOR4,       -- Read Dual Out (4Byte +)
                                   DIOR,       -- Read Dual I/O (3Byte Address)
                                   DIOR4,      -- Read Dual I/O (4Byte +)
                                   DDRDIOR,    -- Read DDR Dual I/O (3Byte)
                                   DDRDIOR4,   -- Read DDR Dual I/O (4Byte +)
                                   QOR,        -- Read Quad Out (3Byte Address)
                                   QOR4,       -- Read Quad Out (4Byte +)
                                   QIOR,       -- Read Quad I/O (3Byte Address)
                                   QIOR4,      -- Read Quad I/O (4Byte +)
                                   DDRQIOR,    -- Read DDR Quad I/O (3Byte)
                                   DDRQIOR4,   -- Read DDR Quad I/O (4Byte +)
                                   PP,         -- Program Page (3Byte Address)
                                   PP4,        -- Program Page (4Byte +)
                                   QPP,        -- Quad Page Program (3Byte)
                                   QPP4,       -- Quad Page Program (4Byte +)
                                   OTPP,       -- OTP Program
                                   PGSP,       -- Program Suspend
                                   PGRS,       -- Program Resume
                                   BE,         -- Bulk Erase
                                   SE,         -- Erase 128/256KB (3Byte)
                                   SE4,        -- Erase 128/256KB (4Byte +)
                                   P4E,        -- 4KB-sector Erase (3Byte Addr)
                                   P4E4,       -- 4KB-sector Erase (4Byte Addr)
                                   ERSP,       -- Erase Suspend
                                   ERRS,       -- Erase Resume
                                   ABRD,       -- AutoBoot Register Read
                                   ABWR,       -- AutoBoot Register Write
                                   BRRD,       -- Bank Register Read
                                   BRWR,       -- Bank Register Write
                                   BRAC,       -- Bank Register Access
                                   ECCRD,      -- ECC Register Read
                                   DLPRD,      -- Read Data Learning Pattern
                                   PNVDLR,     -- Program NVDLP Reg
                                   WVDLR,      -- Write Volatile DLP Reg
                                   ASPRD,      -- ASP Read
                                   ASPP,       -- ASP Program
                                   DYBRD,      -- DYB Read
                                   DYBWR,      -- DYB Write
                                   PPBRD,      -- PPB Read
                                   PPBP,       -- PPB Program
                                   PPBERS,     -- PPB Erase
                                   PLBWR,      -- PPB Lock Bit Write
                                   PLBRD,      -- PPB Lock Bit Read
                                   PASSRD,     -- Password Read
                                   PASSP,      -- Password Program
                                   PASSU,      -- Password Unlock
                                   RESET,      -- Reset
                                   MBR,        -- Mode Bit Reset
                                   MPM,        -- Multi-I/O-High Perf Mode
                                   CLSR,       -- Clear Status Register
                                   RSFDP
                                );

        TYPE WByteType IS ARRAY (0 TO 511) OF INTEGER RANGE -1 TO MaxData;
        -- Flash Memory Array
        TYPE MemArray IS ARRAY (0 TO AddrRANGE) OF INTEGER RANGE -1 TO MaxData;
        -- OTP Memory Array
        TYPE OTPArray IS ARRAY (OTPLoAddr TO OTPHiAddr) OF INTEGER
                                                    RANGE -1 TO MaxData;
        --SFDP Array
        TYPE SFDPtype  IS ARRAY (16#00# TO SFDPHiAddr) OF
                                              INTEGER RANGE -1 TO 16#FF#;
        -----------------------------------------------------------------------
        --  memory declaration
        -----------------------------------------------------------------------
        -- Memory
        SHARED VARIABLE Mem          : MemArray  := (OTHERS => MaxData);
        -- OTP Sector
        SHARED VARIABLE OTPMem       : OTPArray  := (OTHERS => MaxData);
        --SFDP Array
        SHARED VARIABLE SFDP_array    : SFDPtype   := (OTHERS => 0);
        -- Programming Buffer
        SIGNAL WByte                 : WByteType := (OTHERS => MaxData);

        -- states
        SIGNAL current_state         : state_type := RESET_STATE;
        SIGNAL next_state            : state_type := RESET_STATE;

        SIGNAL Instruct              : instruction_type;
        --zero delay signal
        SIGNAL SOOut_zd              : std_logic := 'Z';
        SIGNAL SIOut_zd              : std_logic := 'Z';
        SIGNAL HOLDNegOut_zd         : std_logic := 'Z';
        SIGNAL WPNegOut_zd           : std_logic := 'Z';
        --HOLD delay on output data
        SIGNAL SOOut_z               : std_logic := 'Z';
        SIGNAL SIOut_z               : std_logic := 'Z';
        -- powerup
        SIGNAL PoweredUp             : std_logic := '0';

        -----------------------------------------------------------------------
        -- Registers
        -----------------------------------------------------------------------
        --     ***  Status Register 1  ***
        SHARED VARIABLE Status_reg1   : std_logic_vector(7 downto 0)
                                                := (others => '0');
        SIGNAL Status_reg1_in         : std_logic_vector(7 downto 0)
                                                := (others => '0');

        -- Status Register Write Disable Bit
        ALIAS SRWD      :std_logic IS Status_reg1(7);
        -- Status Register Programming Error Bit
        ALIAS P_ERR     :std_logic IS Status_reg1(6);
        -- Status Register Erase Error Bit
        ALIAS E_ERR     :std_logic IS Status_reg1(5);
        -- Status Register Block Protection Bits
        ALIAS BP2       :std_logic IS Status_reg1(4);
        ALIAS BP1       :std_logic IS Status_reg1(3);
        ALIAS BP0       :std_logic IS Status_reg1(2);
        -- Status Register Write Enable Latch Bit
        ALIAS WEL       :std_logic IS Status_reg1(1);
        -- Status Register Write In Progress Bit
        ALIAS WIP       :std_logic IS Status_reg1(0);

        --     ***  Status Register 2  ***
        SHARED VARIABLE Status_reg2   : std_logic_vector(7 downto 0)
                                                := (others => '0');
        SIGNAL Status_reg2_in         : std_logic_vector(7 downto 0)
                                                := (others => '0');

        -- Status Register Write Enable Latch Bit
        ALIAS ES        :std_logic IS Status_reg2(1);
        -- Status Register Write In Progress Bit
        ALIAS PS        :std_logic IS Status_reg2(0);

        --      ***  Configuration Register 1  ***
        SHARED VARIABLE Config_reg1   : std_logic_vector(7 downto 0)
                                                := (others => '0');
        SIGNAL Config_reg1_in         : std_logic_vector(7 downto 0)
                                                := (others => '0');

        -- Latency code
        ALIAS LC1       :std_logic IS Config_reg1(7);
        ALIAS LC0       :std_logic IS Config_reg1(6);
        -- Configuration Register TBPROT bit
        ALIAS TBPROT    :std_logic IS Config_reg1(5);
--        -- Configuration Register LOCK bit
--        ALIAS LOCK      :std_logic IS Config_reg1(4);
        -- Configuration Register BPNV bit
        ALIAS BPNV      :std_logic IS Config_reg1(3);
        -- Configuration Register QUAD bit
        ALIAS QUAD      :std_logic IS Config_reg1(1);
        -- Configuration Register FREEZE bit
        ALIAS FREEZE    :std_logic IS Config_reg1(0);

        --      ***  VDLR Register  ***
        SHARED VARIABLE VDLR_reg      : std_logic_vector(7 downto 0)
                                                := (others => '0');
        SIGNAL VDLR_reg_in            : std_logic_vector(7 downto 0)
                                                := (others => '0');

        --      ***  NVDLR Register  ***
        SHARED VARIABLE NVDLR_reg     : std_logic_vector(7 downto 0)
                                                := (others => '0');
        SIGNAL NVDLR_reg_in           : std_logic_vector(7 downto 0)
                                                := (others => '0');

        --      ***  AutoBoot Register  ***
        SHARED VARIABLE AutoBoot_reg   : std_logic_vector(31 downto 0)
                                                := (others => '0');
        SIGNAL AutoBoot_reg_in         : std_logic_vector(31 downto 0)
                                                := (others => '0');
        --AutoBoot Enable Bit
        ALIAS ABE       :std_logic IS AutoBoot_reg(0);

        --      ***  Bank Address Register  ***
        SHARED VARIABLE Bank_Addr_reg  : std_logic_vector(7 downto 0)
                                                := (others => '0');
        SIGNAL Bank_Addr_reg_in        : std_logic_vector(7 downto 0)
                                                := (others => '0');
        --Bank Address Register EXTADD bit
        ALIAS EXTADD    :std_logic IS Bank_Addr_reg(7);
        --      ***  ECC Status Register  ***
        SHARED VARIABLE ECCSR     : std_logic_vector(7 downto 0)
                                                := (others => '0');
        ALIAS EECC        :std_logic IS ECCSR(2);
        ALIAS EECCD       :std_logic IS ECCSR(1);
        ALIAS ECCDI       :std_logic IS ECCSR(0);

        --      ***  ASP Register  ***
        SHARED VARIABLE ASP_reg        : std_logic_vector(15 downto 0);
        SIGNAL ASP_reg_in              : std_logic_vector(15 downto 0)
                                                     := (others => '1');
        --Read Password Mode Enable Bit
        ALIAS RPME      :std_logic IS ASP_reg(5);
        --PPB OTP Bit
        ALIAS PPBOTP    :std_logic IS ASP_reg(3);
        -- Password Protection Mode Lock Bit
        ALIAS PWDMLB    :std_logic IS ASP_reg(2);
        --Persistent Protection Mode Lock Bit
        ALIAS PSTMLB    :std_logic IS ASP_reg(1);

        --      ***  Password Register  ***
        SHARED VARIABLE Password_reg   : std_logic_vector(63 downto 0)
                                                := (others => '1');
        SIGNAL Password_reg_in         : std_logic_vector(63 downto 0)
                                                := (others => '1');
        --      ***  PPB Lock Register  ***
        SHARED VARIABLE PPBL           : std_logic_vector(7 downto 0)
                                                := (others => '0');
        SIGNAL PPBL_in                 : std_logic_vector(7 downto 0)
                                                := (others => '0');
        --Persistent Protection Mode Lock Bit
        ALIAS PPB_LOCK                  : std_logic IS PPBL(0);
        SIGNAL PPB_LOCK_temp            : std_ulogic := '0';

        --      ***  PPB Access Register  ***
        SHARED VARIABLE PPBAR          : std_logic_vector(7 downto 0)
                                                := (others => '1');
        SIGNAL PPBAR_in                : std_logic_vector(7 downto 0)
                                                := (others => '1');
        -- PPB_bits(Sec)
        SHARED VARIABLE PPB_bits       : std_logic_vector(SecNum downto 0)
                                                := (OTHERS => '1');
        --      ***  DYB Access Register  ***
        SHARED VARIABLE DYBAR          : std_logic_vector(7 downto 0)
                                                := (others => '1');
        SIGNAL DYBAR_in                : std_logic_vector(7 downto 0)
                                                := (others => '1');
        -- DYB(Sec)
        SHARED VARIABLE DYB_bits       : std_logic_vector(SecNum downto 0);

        -- The Lock Protection Registers for OTP Memory space
        SHARED VARIABLE LOCK_BYTE1 :std_logic_vector(7 downto 0);
        SHARED VARIABLE LOCK_BYTE2 :std_logic_vector(7 downto 0);
        SHARED VARIABLE LOCK_BYTE3 :std_logic_vector(7 downto 0);
        SHARED VARIABLE LOCK_BYTE4 :std_logic_vector(7 downto 0);

        --Command Register
        SIGNAL write              : std_logic := '0';
        SIGNAL cfg_write          : std_logic := '0';
        SIGNAL read_out           : std_logic := '0';

        SIGNAL rd                 : boolean   := false;
        SIGNAL dual               : boolean   := false;
        SIGNAL fast_rd            : boolean   := true;
        SIGNAL ddr                : boolean   := false;
        SIGNAL ddr80              : boolean   := false;
        SIGNAL any_read           : boolean   := false;

        SIGNAL quadpg             : boolean   := false;

        SIGNAL oe                 : boolean   := false;
        SIGNAL oe_z               : boolean   := false;

        SHARED VARIABLE hold_mode : boolean := false;

        --FSM control signals
        SIGNAL PDONE              : std_logic := '1'; --Page Prog. Done
        SIGNAL PSTART             : std_logic := '0'; --Start Page Programming
        SIGNAL PGSUSP             : std_logic := '0'; --Suspend Program
        SIGNAL PGRES              : std_logic := '0'; --Resume Program

        SIGNAL TSU                : std_logic := '0'; --Resume Program

        SIGNAL RES_TO_SUSP_MIN_TIME : std_logic := '0';--Resume to Suspend Flag
        SIGNAL RES_TO_SUSP_TYP_TIME : std_logic := '0';--Resume to Suspend Flag

        SIGNAL WDONE              : std_logic := '1'; --Write operation Done
        SIGNAL WSTART             : std_logic := '0'; --Start Write operation

        SIGNAL ESTART             : std_logic := '0'; --Start Erase operation
        SIGNAL EDONE              : std_logic := '1'; --Erase operation Done
        SIGNAL ESUSP              : std_logic := '0'; --Suspend Erase
        SIGNAL ERES               : std_logic := '0'; --Resume Erase

        --reset timing
        SIGNAL RST                 : std_logic := '0';
        SIGNAL Reseted             : std_logic := '0'; --Reset Timing Control
        --Lock Bit is enabled for customer programming
--        SIGNAL WRLOCKENABLE       : BOOLEAN   := TRUE;
        --Flag that mark if ASP Register is allready programmed
        SIGNAL ASPOTPFLAG         : BOOLEAN   := FALSE;
        SIGNAL ASP_INIT           : NATURAL RANGE 0 TO 1;
        SIGNAL INITIAL_CONFIG     : std_logic := '0';

        SHARED VARIABLE SecAddr   : NATURAL RANGE 0 TO SecNum:= 0;

        SHARED VARIABLE Sec_addr  : NATURAL   := 0;

        SHARED VARIABLE Page_addr : NATURAL;
        SHARED VARIABLE pgm_page  : NATURAL;

--        SHARED VARIABLE QPP_page       : std_logic_vector(PageNum downto 0)
--                                                       := (OTHERS => '0');

        --Flag for Password unlock command
        SIGNAL PASS_UNLOCKED      : boolean   := FALSE;
        SIGNAL PASS_TEMP          : std_logic_vector(63 downto 0)
                                                := (others => '1');

        SHARED VARIABLE DOUBLE    : BOOLEAN := FALSE;
        SHARED VARIABLE EHP       : BOOLEAN := FALSE;

        SHARED VARIABLE read_cnt  : NATURAL := 0;
        SHARED VARIABLE byte_cnt  : NATURAL := 1;
        SHARED VARIABLE read_addr : NATURAL RANGE 0 TO AddrRANGE ;
        SHARED VARIABLE read_addr_tmp : NATURAL RANGE 0 TO AddrRANGE ;

        SHARED VARIABLE start_delay : NATURAL RANGE 0 TO 7;
        SHARED VARIABLE ABSD        : NATURAL RANGE 0 TO 7;
        SIGNAL start_autoboot       : std_logic := '0';

        SIGNAL change_addr        : std_logic := '0';
        SIGNAL Address            : NATURAL RANGE 0 TO AddrRANGE := 0;
        SIGNAL SectorSuspend      : NATURAL RANGE 0 TO SecNum := 0;

        -- Sector address
        SIGNAL SA                 : NATURAL RANGE 0 TO SecNum := 0;

        -- Sector is protect if Sec_Prot(SecNum) = '1'
        SHARED VARIABLE Sec_Prot  : std_logic_vector(SecNum downto 0) :=
                                                   (OTHERS => '0');

        SIGNAL change_BP          : std_logic := '0';
        SHARED VARIABLE BP_bits   : std_logic_vector(2 downto 0) := "000";

        SHARED VARIABLE CFI_array_tmp    : std_logic_vector(647 downto 0);

        SIGNAL Byte_number        : NATURAL RANGE 0 TO 511    := 0;

        TYPE bus_cycle_type IS (STAND_BY,
                                OPCODE_BYTE,
                                ADDRESS_BYTES,
                                DUMMY_BYTES,
                                MODE_BYTE,
                                DATA_BYTES
                                );
        SHARED VARIABLE bus_cycle_state    : bus_cycle_type;
        -- switch between Data bytes and Dummy bytes
        SHARED VARIABLE DummyBytes_act     : X01 := '0';
        SIGNAL dummy_cnt_act_temp          : NATURAL := 0;
        SIGNAL dummy_cnt_act               : NATURAL := 0;
        --Read Password Protection Mode Active flag
        SIGNAL RdPswdProtMode              : std_ulogic := '0';
        --Read Password Protection Mode Support flag
        SIGNAL RdPswdProtEnable            : std_ulogic := '0';
        SIGNAL BAR_ACC                     : std_ulogic := '0';

        SHARED VARIABLE Latency_code       : NATURAL RANGE 0 TO 7;
        SHARED VARIABLE opcode_cnt         : NATURAL := 0;
        SHARED VARIABLE addr_cnt           : NATURAL := 0;
        SHARED VARIABLE mode_cnt           : NATURAL := 0;
        SHARED VARIABLE dummy_cnt          : NATURAL := 0;
        SHARED VARIABLE data_cnt           : NATURAL := 0;

        -- timing check violation
        SIGNAL Viol               : X01 := '0';

        PROCEDURE ADDRHILO_SEC(
            VARIABLE   AddrLOW  : INOUT NATURAL RANGE 0 to ADDRRange;
            VARIABLE   AddrHIGH : INOUT NATURAL RANGE 0 to ADDRRange;
            VARIABLE   Addr     : NATURAL) IS
            VARIABLE   sector   : NATURAL RANGE 0 TO SecNum;
        BEGIN
            sector   := Addr/16#40000#;
            AddrLOW  := sector*16#40000#;
            AddrHIGH := sector*16#40000# + 16#3FFFF#;
        END ADDRHILO_SEC;

        PROCEDURE ADDRHILO_PG(
            VARIABLE   AddrLOW  : INOUT NATURAL RANGE 0 to ADDRRange;
            VARIABLE   AddrHIGH : INOUT NATURAL RANGE 0 to ADDRRange;
            VARIABLE   Addr     : NATURAL) IS
            VARIABLE   page     : NATURAL RANGE 0 TO PageNum;
        BEGIN
            page     := Addr/PageSize;
            AddrLOW  := Page*PageSize;
            AddrHIGH := Page*PageSize + (PageSize-1);
        END AddrHILO_PG;

    BEGIN
    ---------------------------------------------------------------------------
    --Power Up time
    ---------------------------------------------------------------------------

    PoweredUp <= '1' AFTER tdevice_PU;

    LongT: PROCESS(PoweredUp)
    BEGIN
        IF LongTimming = TRUE THEN
            LongCheck := 1;
         ELSE
            LongCheck := 100;
         END IF;
    END PROCESS;

    TimingModelSel: PROCESS
    BEGIN
        --Enhanced High Performance Flag
        IF (TimingModel(15) = '0' OR TimingModel(15) = '2' OR
            TimingModel(15) = '3' OR TimingModel(15) = 'R' OR
            TimingModel(15) = 'A' OR TimingModel(15) = 'B' OR
            TimingModel(15) = 'C' OR TimingModel(15) = 'D' OR
            TimingModel(15) = 'Y' OR TimingModel(15) = 'Z' OR
            TimingModel(15) = 'S' OR TimingModel(15) = 'T' OR
            TimingModel(15) = 'K' OR TimingModel(15) = 'L') THEN
            EHP := TRUE;
            IF (TimingModel(15) = 'Y' OR TimingModel(15) = 'Z' OR
                TimingModel(15) = 'S' OR TimingModel(15) = 'T' OR
                TimingModel(15) = 'K' OR TimingModel(15) = 'L')   THEN
                RdPswdProtEnable <= '1';
            END IF;
        ELSIF (TimingModel(15) = '4' OR TimingModel(15) = '6' OR
               TimingModel(15) = '7' OR TimingModel(15) = '8' OR
               TimingModel(15) = '9' OR TimingModel(15) = 'Q') THEN
            EHP := FALSE;
        END IF;

        IF (TimingModel(15) ='0' OR TimingModel(15) ='2' OR
            TimingModel(15) ='3' OR TimingModel(15) ='R' OR
            TimingModel(15) ='A' OR TimingModel(15) ='B' OR
            TimingModel(15) ='C' OR TimingModel(15) ='D' OR
            TimingModel(15) ='4' OR TimingModel(15) ='6' OR
            TimingModel(15) ='7' OR TimingModel(15) ='8' OR
            TimingModel(15) ='9' OR TimingModel(15) ='Q') THEN
            ASP_INIT   <= 1;
        ELSIF (TimingModel(15) ='Y' OR TimingModel(15) ='Z' OR
               TimingModel(15) ='S' OR TimingModel(15) ='T' OR
               TimingModel(15) ='K' OR TimingModel(15) ='L') THEN
            ASP_INIT   <= 0;
        END IF;
        WAIT;
    END PROCESS;

    RSTtiming: PROCESS(RSTNeg_pullup,Instruct)
    BEGIN
        IF falling_edge(RSTNeg_pullup) THEN
            RST <= '1', '0' AFTER 200 ns;
        ELSIF Instruct = RESET THEN
            Reseted <= '0', '1' AFTER 10 ns;
        END IF;
    END PROCESS;

    DUMMYcnt: PROCESS(dummy_cnt_act_temp)
    BEGIN
        dummy_cnt_act <= dummy_cnt_act_temp;
    END PROCESS;

    ReadPasswordProtectionMode: PROCESS(PPB_LOCK_temp,
    ASP_reg_in(2), ASP_reg_in(5))
    BEGIN
        IF (PPB_LOCK = '0' AND PWDMLB = '0' AND RPME = '0' AND
            RdPswdProtEnable = '1') THEN
            RdPswdProtMode <= '1';
            ABE := '0';
        ELSE
            RdPswdProtMode <= '0';
        END IF;
    END PROCESS;
    ---------------------------------------------------------------------------
    -- autoboot control logic
    ---------------------------------------------------------------------------
    AutoBootControl: PROCESS(SCK_ipd, current_state)
    BEGIN
        IF (current_state = AUTOBOOT) THEN
            IF rising_edge(SCK_ipd) THEN
                IF (start_delay > 0) THEN
                    start_delay := start_delay - 1;
                END IF;
            END IF;

            IF (start_delay = 0) THEN
                start_autoboot <= '1';
            ELSE
                start_autoboot <= '0';
            END IF;
        END IF;
    END PROCESS;

    ---------------------------------------------------------------------------
    -- VITAL Timing Checks Procedures
    ---------------------------------------------------------------------------
    VITALTimingCheck: PROCESS(SIIn, SOIn, SCK_ipd, CSNeg_ipd, RSTNeg_ipd,
                              HOLDNegIn, WPNegIn)

        -- Timing Check Variables
        -- Setup/Hold Checks variables
        VARIABLE Tviol_CSNeg_SCK_normal  : X01 := '0';
        VARIABLE TD_CSNeg_SCK_normal     : VitalTimingDataType;

        VARIABLE Tviol_CSNeg_SCK_DDR     : X01 := '0';
        VARIABLE TD_CSNeg_SCK_DDR        : VitalTimingDataType;

        VARIABLE Tviol_CSNeg_RSTNeg      : X01 := '0';
        VARIABLE TD_CSNeg_RSTNeg         : VitalTimingDataType;

        VARIABLE Tviol_SI_SCK            : X01 := '0';
        VARIABLE TD_SI_SCK               : VitalTimingDataType;

        VARIABLE Tviol_WPNeg_CSNeg_setup : X01 := '0';
        VARIABLE TD_WPNeg_CSNeg_setup    : VitalTimingDataType;

        VARIABLE Tviol_WPNeg_CSNeg_hold  : X01 := '0';
        VARIABLE TD_WPNeg_CSNeg_hold     : VitalTimingDataType;

        VARIABLE Tviol_HOLDNeg_SCK       : X01 := '0';
        VARIABLE TD_HOLDNeg_SCK          : VitalTimingDataType;

        VARIABLE Tviol_SI_SCK_DDR_R      : X01 := '0';
        VARIABLE TD_SI_SCK_DDR_R         : VitalTimingDataType;

        VARIABLE Tviol_SI_SCK_DDR_F      : X01 := '0';
        VARIABLE TD_SI_SCK_DDR_F         : VitalTimingDataType;

        VARIABLE Tviol_RSTNeg_CSNeg      : X01 := '0';
        VARIABLE TD_RSTNeg_CSNeg         : VitalTimingDataType;

        --Pulse Width and Period Check Variables
        VARIABLE Pviol_SCK_serial  : X01 := '0';
        VARIABLE PD_SCK_serial     : VitalPeriodDataType:= VitalPeriodDataInit;

        VARIABLE Pviol_SCK_dual    : X01 := '0';
        VARIABLE PD_SCK_dual       : VitalPeriodDataType:= VitalPeriodDataInit;

        VARIABLE Pviol_SCK_DDR80   : X01 := '0';
        VARIABLE PD_SCK_DDR80      : VitalPeriodDataType:= VitalPeriodDataInit;

        VARIABLE Pviol_SCK_fast    : X01 := '0';
        VARIABLE PD_SCK_fast       : VitalPeriodDataType:= VitalPeriodDataInit;

        VARIABLE Pviol_SCK_quadpg  : X01 := '0';
        VARIABLE PD_SCK_quadpg     : VitalPeriodDataType:= VitalPeriodDataInit;

        VARIABLE Pviol_SCK_DDR     : X01 := '0';
        VARIABLE PD_SCK_DDR        : VitalPeriodDataType:= VitalPeriodDataInit;

        VARIABLE Pviol_CSNeg_read  : X01 := '0';
        VARIABLE PD_CSNeg_read     : VitalPeriodDataType:= VitalPeriodDataInit;

        VARIABLE Pviol_CSNeg_pgers : X01 := '0';
        VARIABLE PD_CSNeg_pgers    : VitalPeriodDataType:= VitalPeriodDataInit;

        VARIABLE Pviol_RSTNeg      : X01 := '0';
        VARIABLE PD_RSTNeg         : VitalPeriodDataType:= VitalPeriodDataInit;

        VARIABLE Pviol_SCK_serial_rd : X01 := '0';
        VARIABLE PD_SCK_serial_rd  : VitalPeriodDataType:= VitalPeriodDataInit;

        VARIABLE Pviol_SCK_fast_rd : X01 := '0';
        VARIABLE PD_SCK_fast_rd    : VitalPeriodDataType:=VitalPeriodDataInit;

        VARIABLE Pviol_SCK_dual_rd : X01 := '0';
        VARIABLE PD_SCK_dual_rd    : VitalPeriodDataType:=VitalPeriodDataInit;

        VARIABLE Pviol_SCK_DDR_rd  : X01 := '0';
        VARIABLE PD_SCK_DDR_rd     : VitalPeriodDataType:= VitalPeriodDataInit;

        VARIABLE Pviol_SCK_DDR80_rd  : X01 := '0';
        VARIABLE PD_SCK_DDR80_rd     : VitalPeriodDataType:= VitalPeriodDataInit;

        VARIABLE Pviol_SCK_quad_pg : X01 := '0';
        VARIABLE PD_SCK_quad_pg    : VitalPeriodDataType:= VitalPeriodDataInit;

        VARIABLE Violation         : X01 := '0';

    BEGIN
    ---------------------------------------------------------------------------
    -- Timing Check Section
    ---------------------------------------------------------------------------
        IF (TimingChecksOn) THEN

        -- Setup/Hold Check between CS# and SCK
        VitalSetupHoldCheck (
            TestSignal      => CSNeg_ipd,
            TestSignalName  => "CS#",
            RefSignal       => SCK_ipd,
            RefSignalName   => "SCK",
            SetupHigh       => tsetup_CSNeg_SCK_normal_noedge_posedge,
            SetupLow        => tsetup_CSNeg_SCK_normal_noedge_posedge,
            HoldHigh        => thold_CSNeg_SCK_normal_noedge_posedge,
            HoldLow         => thold_CSNeg_SCK_normal_noedge_posedge,
            CheckEnabled    => ddr = false,
            RefTransition   => '/',
            HeaderMsg       => InstancePath & PartID,
            TimingData      => TD_CSNeg_SCK_normal,
            Violation       => Tviol_CSNeg_SCK_normal
        );

        -- Setup/Hold Check between CS# and SCK
        VitalSetupHoldCheck (
            TestSignal      => CSNeg_ipd,
            TestSignalName  => "CS#",
            RefSignal       => SCK_ipd,
            RefSignalName   => "SCK",
            SetupHigh       => tsetup_CSNeg_SCK_DDR_noedge_posedge,
            SetupLow        => tsetup_CSNeg_SCK_DDR_noedge_posedge,
            HoldHigh        => thold_CSNeg_SCK_DDR_noedge_posedge,
            HoldLow         => thold_CSNeg_SCK_DDR_noedge_posedge,
            CheckEnabled    => ddr,
            RefTransition   => '/',
            HeaderMsg       => InstancePath & PartID,
            TimingData      => TD_CSNeg_SCK_DDR,
            Violation       => Tviol_CSNeg_SCK_DDR
        );

        -- Hold Check between CSNeg and RSTNeg
        VitalSetupHoldCheck (
            TestSignal      => CSNeg,
            TestSignalName  => "CSNeg",
            RefSignal       => RSTNeg,
            RefSignalName   => "RSTNeg",
            HoldHigh        => thold_CSNeg_RSTNeg,
            CheckEnabled    => TRUE,
            RefTransition   => '\',
            HeaderMsg       => InstancePath & PartID,
            TimingData      => TD_CSNeg_RSTNeg,
            Violation       => Tviol_CSNeg_RSTNeg
        );

        -- Setup/Hold Check between SI and SCK, serial mode
        VitalSetupHoldCheck (
            TestSignal      => SIIn,
            TestSignalName  => "SI",
            RefSignal       => SCK_ipd,
            RefSignalName   => "SCK",
            SetupHigh       => tsetup_SI_SCK_normal_noedge_posedge,
            SetupLow        => tsetup_SI_SCK_normal_noedge_posedge,
            HoldHigh        => thold_SI_SCK_normal_noedge_posedge,
            HoldLow         => thold_SI_SCK_normal_noedge_posedge,
            CheckEnabled    => NOT(DOUBLE) AND SIOut_z /= SIIn ,
            RefTransition   => '/',
            HeaderMsg       => InstancePath & PartID,
            TimingData      => TD_SI_SCK,
            Violation       => Tviol_SI_SCK
        );

        -- Setup Check between WP# and CS# \
        VitalSetupHoldCheck (
            TestSignal      => WPNegIn,
            TestSignalName  => "WP#",
            RefSignal       => CSNeg_ipd,
            RefSignalName   => "CS#",
            SetupHigh       => tsetup_WPNeg_CSNeg,
            CheckEnabled    => true,
            RefTransition   => '\',
            HeaderMsg       => InstancePath & PartID,
            TimingData      => TD_WPNeg_CSNeg_setup,
            Violation       => Tviol_WPNeg_CSNeg_setup
        );

        -- Hold Check between WP# and CS# /
        VitalSetupHoldCheck (
            TestSignal      => WPNegIn,
            TestSignalName  => "WP#",
            RefSignal       => CSNeg_ipd,
            RefSignalName   => "CS#",
            HoldHigh        => thold_WPNeg_CSNeg,
            CheckEnabled    => SRWD = '1' AND WEL = '1',
            RefTransition   => '/',
            HeaderMsg       => InstancePath & PartID,
            TimingData      => TD_WPNeg_CSNeg_hold,
            Violation       => Tviol_WPNeg_CSNeg_hold
        );

        -- Setup/Hold Check between HOLD# and SCK /
        VitalSetupHoldCheck (
            TestSignal      => HOLDNegIn,
            TestSignalName  => "HOLD#",
            RefSignal       => SCK_ipd,
            RefSignalName   => "SCK",
            SetupLow        => tsetup_HOLDNeg_SCK,
            SetupHigh       => tsetup_HOLDNeg_SCK,
            HoldLow         => thold_HOLDNeg_SCK,
            HoldHigh        => thold_HOLDNeg_SCK,
            CheckEnabled    => QUAD = '0'
                               AND HOLDNegOut_zd /= HOLDNegIn,
            RefTransition   => '/',
            HeaderMsg       => InstancePath & PartID,
            TimingData      => TD_HOLDNeg_SCK,
            Violation       => Tviol_HOLDNeg_SCK
        );

        -- Setup/Hold Check between SI and SCK, DDR mode
        VitalSetupHoldCheck (
            TestSignal      => SIIn,
            TestSignalName  => "SI",
            RefSignal       => SCK_ipd,
            RefSignalName   => "SCK",
            SetupHigh       => tsetup_SI_SCK_DDR_noedge_posedge,
            SetupLow        => tsetup_SI_SCK_DDR_noedge_posedge,
            HoldHigh        => thold_SI_SCK_DDR_noedge_posedge,
            HoldLow         => thold_SI_SCK_DDR_noedge_posedge,
            CheckEnabled    => DOUBLE AND dual = false AND
                               ddr80 = false AND SIOut_z /= SIIn,
            RefTransition   => '/',
            HeaderMsg       => InstancePath & PartID,
            TimingData      => TD_SI_SCK_DDR_R,
            Violation       => Tviol_SI_SCK_DDR_R
        );

        -- Setup/Hold Check between SI and SCK, DDR mode
        VitalSetupHoldCheck (
            TestSignal      => SIIn,
            TestSignalName  => "SI",
            RefSignal       => SCK_ipd,
            RefSignalName   => "SCK",
            SetupHigh       => tsetup_SI_SCK_DDR80_noedge_posedge,
            SetupLow        => tsetup_SI_SCK_DDR80_noedge_posedge,
            HoldHigh        => thold_SI_SCK_DDR80_noedge_posedge,
            HoldLow         => thold_SI_SCK_DDR80_noedge_posedge,
            CheckEnabled    => DOUBLE AND dual = false AND
                               ddr80 = true AND SIOut_z /= SIIn,
            RefTransition   => '/',
            HeaderMsg       => InstancePath & PartID,
            TimingData      => TD_SI_SCK_DDR_R,
            Violation       => Tviol_SI_SCK_DDR_R
        );

        -- Setup/Hold Check between SI and SCK, DDR mode
        VitalSetupHoldCheck (
            TestSignal      => SIIn,
            TestSignalName  => "SI",
            RefSignal       => SCK_ipd,
            RefSignalName   => "SCK",
            SetupHigh       => tsetup_SI_SCK_DDR_noedge_negedge,
            SetupLow        => tsetup_SI_SCK_DDR_noedge_negedge,
            HoldHigh        => thold_SI_SCK_DDR_noedge_negedge,
            HoldLow         => thold_SI_SCK_DDR_noedge_negedge,
            CheckEnabled    => ddr AND dual = false
                               AND SIOut_z /= SIIn ,
            RefTransition   => '\',
            HeaderMsg       => InstancePath & PartID,
            TimingData      => TD_SI_SCK_DDR_F,
            Violation       => Tviol_SI_SCK_DDR_F
        );


        -- Setup/Hold Check between SI and SCK, DDR mode
        VitalSetupHoldCheck (
            TestSignal      => SIIn,
            TestSignalName  => "SI",
            RefSignal       => SCK_ipd,
            RefSignalName   => "SCK",
            SetupHigh       => tsetup_SI_SCK_DDR80_noedge_negedge,
            SetupLow        => tsetup_SI_SCK_DDR80_noedge_negedge,
            HoldHigh        => thold_SI_SCK_DDR80_noedge_negedge,
            HoldLow         => thold_SI_SCK_DDR80_noedge_negedge,
            CheckEnabled    => ddr AND dual = false AND
                               ddr80 = false AND SIOut_z /= SIIn ,
            RefTransition   => '\',
            HeaderMsg       => InstancePath & PartID,
            TimingData      => TD_SI_SCK_DDR_F,
            Violation       => Tviol_SI_SCK_DDR_F
        );

        -- Setup Check between RSTNeg and SCK, DDR fast mode
        VitalSetupHoldCheck (
            TestSignal      => RSTNeg,
            TestSignalName  => "RSTNeg",
            RefSignal       => CSNeg,
            RefSignalName   => "CSNeg",
            SetupHigh       => tsetup_RSTNeg_CSNeg,
            CheckEnabled    => TRUE,
            RefTransition   => '\',
            HeaderMsg       => InstancePath & PartID,
            TimingData      => TD_RSTNeg_CSNeg,
            Violation       => Tviol_RSTNeg_CSNeg
        );

        --Pulse Width and Period Check Variables
        -- Pulse Width Check SCK for READ, serial mode
        VitalPeriodPulseCheck (
            TestSignal      =>  SCK_ipd,
            TestSignalName  =>  "SCK",
            PulseWidthLow   =>  tpw_SCK_serial_negedge,
            PulseWidthHigh  =>  tpw_SCK_serial_posedge,
            PeriodData      =>  PD_SCK_serial,
            XOn             =>  XOn,
            MsgOn           =>  MsgOn,
            Violation       =>  Pviol_SCK_serial,
            HeaderMsg       =>  InstancePath & PartID,
            CheckEnabled    =>  rd);

        -- Pulse Width Check SCK for DUAL_READ, serial mode
        VitalPeriodPulseCheck (
            TestSignal      =>  SCK_ipd,
            TestSignalName  =>  "SCK",
            PulseWidthLow   =>  tpw_SCK_dual_negedge,
            PulseWidthHigh  =>  tpw_SCK_dual_posedge,
            PeriodData      =>  PD_SCK_dual,
            XOn             =>  XOn,
            MsgOn           =>  MsgOn,
            Violation       =>  Pviol_SCK_dual,
            HeaderMsg       =>  InstancePath & PartID,
            CheckEnabled    =>  dual);

        -- Pulse Width Check SCK for DUAL_READ, serial mode
        VitalPeriodPulseCheck (
            TestSignal      =>  SCK_ipd,
            TestSignalName  =>  "SCK",
            PulseWidthLow   =>  tpw_SCK_ddr80_negedge,
            PulseWidthHigh  =>  tpw_SCK_ddr80_posedge,
            PeriodData      =>  PD_SCK_dual,
            XOn             =>  XOn,
            MsgOn           =>  MsgOn,
            Violation       =>  Pviol_SCK_dual,
            HeaderMsg       =>  InstancePath & PartID,
            CheckEnabled    =>  ddr80);

        -- Pulse Width Check SCK for FAST_READ, serial mode
        VitalPeriodPulseCheck (
            TestSignal      =>  SCK_ipd,
            TestSignalName  =>  "SCK",
            PulseWidthLow   =>  tpw_SCK_fast_negedge,
            PulseWidthHigh  =>  tpw_SCK_fast_posedge,
            PeriodData      =>  PD_SCK_fast,
            XOn             =>  XOn,
            MsgOn           =>  MsgOn,
            Violation       =>  Pviol_SCK_fast,
            HeaderMsg       =>  InstancePath & PartID,
            CheckEnabled    =>  fast_rd );

        -- Pulse Width Check SCK for QPP
        VitalPeriodPulseCheck (
            TestSignal      =>  SCK_ipd,
            TestSignalName  =>  "SCK",
            PulseWidthLow   =>  tpw_SCK_quadpg_negedge,
            PulseWidthHigh  =>  tpw_SCK_quadpg_posedge,
            PeriodData      =>  PD_SCK_quadpg,
            XOn             =>  XOn,
            MsgOn           =>  MsgOn,
            Violation       =>  Pviol_SCK_quadpg,
            HeaderMsg       =>  InstancePath & PartID,
            CheckEnabled    =>  quadpg);

        -- Pulse Width Check CS# for READ, serial mode
        VitalPeriodPulseCheck (
            TestSignal      =>  CSNeg_ipd,
            TestSignalName  =>  "CS#",
            PulseWidthHigh  =>  tpw_CSNeg_read_posedge,
            PeriodData      =>  PD_CSNeg_read,
            XOn             =>  XOn,
            MsgOn           =>  MsgOn,
            Violation       =>  Pviol_CSNeg_read,
            HeaderMsg       =>  InstancePath & PartID,
            CheckEnabled    =>  any_read );

        -- Pulse Width Check CS# for Program/Erase, serial mode
        VitalPeriodPulseCheck (
            TestSignal      =>  CSNeg_ipd,
            TestSignalName  =>  "CS#",
            PulseWidthHigh  =>  tpw_CSNeg_pgers_posedge,
            PeriodData      =>  PD_CSNeg_pgers,
            XOn             =>  XOn,
            MsgOn           =>  MsgOn,
            Violation       =>  Pviol_CSNeg_pgers,
            HeaderMsg       =>  InstancePath & PartID,
            CheckEnabled    =>  NOT(any_read));

        -- Pulse Width Check RSTNeg
        VitalPeriodPulseCheck (
            TestSignal        => RSTNeg_ipd,
            TestSignalName    => "RSTNeg",
            PulseWidthLow     => tpw_RSTNeg_negedge,
            PulseWidthHigh    => tpw_RSTNeg_posedge,
            CheckEnabled      => TRUE,
            HeaderMsg         => InstancePath & PartID,
            PeriodData        => PD_RSTNeg,
            Violation         => Pviol_RSTNeg
            );

        -- Pulse Width Check SCK for DDR READ
        VitalPeriodPulseCheck (
            TestSignal      =>  SCK_ipd,
            TestSignalName  =>  "SCK",
            PulseWidthLow   =>  tpw_SCK_DDR_negedge,
            PulseWidthHigh  =>  tpw_SCK_DDR_posedge,
            PeriodData      =>  PD_SCK_DDR,
            XOn             =>  XOn,
            MsgOn           =>  MsgOn,
            Violation       =>  Pviol_SCK_DDR,
            HeaderMsg       =>  InstancePath & PartID,
            CheckEnabled    =>  ddr);

        -- Pulse Width Check SCK for DDR READ
        VitalPeriodPulseCheck (
            TestSignal      =>  SCK_ipd,
            TestSignalName  =>  "SCK",
            PulseWidthLow   =>  tpw_SCK_DDR80_negedge,
            PulseWidthHigh  =>  tpw_SCK_DDR80_posedge,
            PeriodData      =>  PD_SCK_DDR80,
            XOn             =>  XOn,
            MsgOn           =>  MsgOn,
            Violation       =>  Pviol_SCK_DDR80,
            HeaderMsg       =>  InstancePath & PartID,
            CheckEnabled    =>  ddr80);

        -- Period Check SCK for READ, serial mode
        VitalPeriodPulseCheck (
            TestSignal      =>  SCK_ipd,
            TestSignalName  =>  "SCK",
            Period          =>  tperiod_SCK_serial_rd,
            PeriodData      =>  PD_SCK_serial_rd,
            XOn             =>  XOn,
            MsgOn           =>  MsgOn,
            Violation       =>  Pviol_SCK_serial_rd,
            HeaderMsg       =>  InstancePath & PartID,
            CheckEnabled    =>  rd );

        -- Period Check SCK for FAST READ, serial mode
        VitalPeriodPulseCheck (
            TestSignal      =>  SCK_ipd,
            TestSignalName  =>  "SCK",
            Period          =>  tperiod_SCK_fast_rd,
            PeriodData      =>  PD_SCK_fast_rd,
            XOn             =>  XOn,
            MsgOn           =>  MsgOn,
            Violation       =>  Pviol_SCK_fast_rd,
            HeaderMsg       =>  InstancePath & PartID,
            CheckEnabled    =>  fast_rd );

        -- Period Check SCK for DUAL READ, serial mode
        VitalPeriodPulseCheck (
            TestSignal      =>  SCK_ipd,
            TestSignalName  =>  "SCK",
            Period          =>  tperiod_SCK_dual_rd,
            PeriodData      =>  PD_SCK_dual_rd,
            XOn             =>  XOn,
            MsgOn           =>  MsgOn,
            Violation       =>  Pviol_SCK_dual_rd,
            HeaderMsg       =>  InstancePath & PartID,
            CheckEnabled    =>  dual );

        -- Period Check SCK for QPP
        VitalPeriodPulseCheck (
            TestSignal      =>  SCK_ipd,
            TestSignalName  =>  "SCK",
            Period          =>  tperiod_SCK_quadpg,
            PeriodData      =>  PD_SCK_quad_pg,
            XOn             =>  XOn,
            MsgOn           =>  MsgOn,
            Violation       =>  Pviol_SCK_quad_pg,
            HeaderMsg       =>  InstancePath & PartID,
            CheckEnabled    =>  quadpg );

        -- Period Check SCK for DDR READ
        VitalPeriodPulseCheck (
            TestSignal      =>  SCK_ipd,
            TestSignalName  =>  "SCK",
            Period          =>  tperiod_SCK_DDR_rd,
            PeriodData      =>  PD_SCK_DDR_rd,
            XOn             =>  XOn,
            MsgOn           =>  MsgOn,
            Violation       =>  Pviol_SCK_DDR_rd,
            HeaderMsg       =>  InstancePath & PartID,
            CheckEnabled    =>  ddr );

        -- Period Check SCK for DDR READ
        VitalPeriodPulseCheck (
            TestSignal      =>  SCK_ipd,
            TestSignalName  =>  "SCK",
            Period          =>  tperiod_SCK_DDR80_rd,
            PeriodData      =>  PD_SCK_DDR80_rd,
            XOn             =>  XOn,
            MsgOn           =>  MsgOn,
            Violation       =>  Pviol_SCK_DDR80_rd,
            HeaderMsg       =>  InstancePath & PartID,
            CheckEnabled    =>  ddr80 );

        Violation :=   Tviol_CSNeg_SCK_normal OR
                       Tviol_CSNeg_SCK_DDR OR
                       Tviol_CSNeg_RSTNeg OR
                       Tviol_SI_SCK OR
                       Tviol_WPNeg_CSNeg_setup OR
                       Tviol_WPNeg_CSNeg_hold OR
                       Tviol_HOLDNeg_SCK OR
                       Tviol_SI_SCK_DDR_R OR
                       Tviol_SI_SCK_DDR_F OR
                       Tviol_RSTNeg_CSNeg OR
                       Pviol_SCK_serial OR
                       Pviol_SCK_dual OR
                       Pviol_SCK_fast OR
                       Pviol_SCK_quadpg OR
                       Pviol_CSNeg_read OR
                       Pviol_CSNeg_pgers OR
                       Pviol_SCK_DDR OR
                       Pviol_SCK_DDR80 OR
                       Pviol_SCK_serial_rd OR
                       Pviol_SCK_fast_rd OR
                       Pviol_SCK_dual_rd OR
                       Pviol_SCK_quad_pg OR
                       Pviol_SCK_DDR_rd OR
                       Pviol_SCK_DDR80_rd;

            Viol <= Violation;

            ASSERT Violation = '0'
                REPORT InstancePath & partID & ": simulation may be" &
                    " inaccurate due to timing violations"
                SEVERITY WARNING;

        END IF;
    END PROCESS VITALTimingCheck;

    ----------------------------------------------------------------------------
    -- sequential process for FSM state transition
    ----------------------------------------------------------------------------
    StateTransition : PROCESS(next_state, RST, PoweredUp, RST_out)

    BEGIN
        IF PoweredUp = '1' THEN
            IF RSTNeg_pullup = '1' and RST_out= '1' THEN
                IF next_state'EVENT THEN
                    current_state <= next_state;
                END IF;
            ELSIF RSTNeg_pullup = '0' AND falling_edge(RST) THEN
                --no state transition while RESET# low
                current_state <= RESET_STATE;
                RST_in <= '1', '0' AFTER 1 ns;
            END IF;
        END IF;
    END PROCESS StateTransition;

    Threset : PROCESS(RST_in)
    BEGIN
        IF rising_edge(RST_in) THEN
            RST_out <= '0', '1' AFTER HOLD_CSNeg_RSTNeg;
        END IF;
    END PROCESS Threset;

    ---------------------------------------------------------------------------
    --  Write cycle decode
    ---------------------------------------------------------------------------
    BusCycleDecode : PROCESS(SCK_ipd, CSNeg_ipd, HOLDNeg_pullup, SIIn, RST_out,
                             WPNeg_pullup, current_state)

        TYPE quad_data_type IS ARRAY (0 TO 1023) OF INTEGER RANGE 0 TO 15;

        VARIABLE bit_cnt            : NATURAL := 0;
        VARIABLE Data_in            : std_logic_vector(4095 downto 0)
                                                    := (others => '0');

        VARIABLE opcode             : std_logic_vector(7 downto 0);
        VARIABLE opcode_in          : std_logic_vector(7 downto 0);
        VARIABLE addr_bytes         : std_logic_vector(31 downto 0);
        VARIABLE hiaddr_bytes       : std_logic_vector(31 downto 0);
        VARIABLE Address_in         : std_logic_vector(31 downto 0);
        VARIABLE mode_bytes         : std_logic_vector(7 downto 0);
        VARIABLE mode_in            : std_logic_vector(7 downto 0);
        VARIABLE quad_data_in       : quad_data_type;
        VARIABLE quad_nybble        : std_logic_vector(3 downto 0);
        VARIABLE Quad_slv           : std_logic_vector(3 downto 0);
        VARIABLE Byte_slv           : std_logic_vector(7 downto 0);

        VARIABLE CLK_PER            : time;
        VARIABLE LAST_CLK           : time;
        VARIABLE Check_freq         : boolean := FALSE;

    BEGIN

        IF (rising_edge(CSNeg_ipd) AND NOT(bus_cycle_state = DATA_BYTES))
        OR current_state = RESET_STATE THEN
            bus_cycle_state := STAND_BY;
        ELSE
            CASE bus_cycle_state IS
                WHEN STAND_BY =>
                    IF falling_edge(CSNeg_ipd) THEN
                        Instruct  <= NONE;
                        write     <= '1';
                        cfg_write <= '0';
                        opcode_cnt:= 0;
                        addr_cnt  := 0;
                        mode_cnt  := 0;
                        dummy_cnt := 0;
                        dummy_cnt_act_temp <= dummy_cnt;
                        data_cnt  := 0;
                        DOUBLE    := FALSE;
                        CLK_PER   := 0 ns;
                        LAST_CLK  := 0 ns;
                        IF current_state = AUTOBOOT THEN
                            bus_cycle_state := DATA_BYTES;
                        ELSE
                            bus_cycle_state := OPCODE_BYTE;
                        END IF;
                    END IF;

                WHEN OPCODE_BYTE =>
                    IF rising_edge(SCK_ipd)  THEN

                        Latency_code := to_nat(LC1 & LC0);
                        CLK_PER  := NOW - LAST_CLK;
                        LAST_CLK := NOW;
                        IF Check_freq THEN
                            IF (CLK_PER < 20 ns AND Latency_code = 3)  OR
                               (CLK_PER < 12.5 ns AND Latency_code = 0) OR
                               (CLK_PER < 11.1 ns AND Latency_code = 1) OR
                               (CLK_PER < 9.6 ns AND Latency_code = 2) THEN
                                ASSERT FALSE
                                REPORT "More wait states are required for " &
                                       "this clock frequency value"
                                SEVERITY warning;
                                IF (Instruct = DDRFR   OR Instruct = DDRFR4 OR
                                    Instruct = DDRDIOR OR Instruct = DDRDIOR4 OR
                                    Instruct = DDRQIOR OR Instruct = DDRQIOR4)  THEN
                                    IF (CLK_PER < 12.5 ns) THEN
                                        ddr80 <= TRUE;
                                    ELSE
                                        ddr80 <= FALSE;
                                    END IF;
                                END IF;
                            END IF;
                        END IF;
                        Check_freq := FALSE;

                        IF ((HOLDNeg_pullup='1' AND QUAD='0') OR QUAD='1') THEN
                        --One-byte of instruction opcode is shifted into the
                        --device on the SI serial input pin with the most
                        --significant bit (MSB) first.Each bit input on the
                        --SI serial input pin is latched on the rising edge of
                        --the SCK serial clock signal.
                            opcode_in(opcode_cnt) := SIIn;
                            opcode_cnt := opcode_cnt + 1;
                            IF opcode_cnt = BYTE THEN
                                --MSB first
                                FOR I IN 7 DOWNTO 0 LOOP
                                    opcode(i) := opcode_in(7-i);
                                END LOOP;

                                CASE opcode IS
                                    WHEN "00000110"  => --06h
                                        Instruct <= WREN;
                                        bus_cycle_state := DATA_BYTES;
                                    WHEN "00000100"  => --04h
                                        Instruct <= WRDI;
                                        bus_cycle_state := DATA_BYTES;
                                    WHEN "00000001"  => --01h
                                        Instruct <= WRR;
                                        bus_cycle_state := DATA_BYTES;
                                    WHEN "00000011"  => --03h
                                        Instruct <= READ;
                                        bus_cycle_state := ADDRESS_BYTES;
                                    WHEN "00010011"  => --13h
                                        Instruct <= RD4;
                                        bus_cycle_state := ADDRESS_BYTES;
                                    WHEN "01001011"  => --4Bh
                                        Instruct <= OTPR;
                                        bus_cycle_state := ADDRESS_BYTES;
                                    WHEN "00000101"  => --05h
                                        Instruct <= RDSR;
                                        bus_cycle_state := DATA_BYTES;
                                    WHEN "00000111"  => --07h
                                        Instruct <= RDSR2;
                                        bus_cycle_state := DATA_BYTES;
                                    WHEN "00110101"  => --35h
                                        Instruct <= RDCR;
                                        bus_cycle_state := DATA_BYTES;
                                    WHEN "10010000"  => --90h
                                        Instruct <= REMS;
                                        bus_cycle_state := ADDRESS_BYTES;
                                    WHEN "10011111"  => --9Fh
                                        Instruct <= RDID;
                                        bus_cycle_state := DATA_BYTES;
                                    WHEN "10101011"  => --ABh
                                        Instruct <= RES;
                                        bus_cycle_state := DUMMY_BYTES;
                                    WHEN "00001011"  => --0Bh
                                        Instruct <= FSTRD;
                                        bus_cycle_state := ADDRESS_BYTES;
                                        Check_freq := TRUE;
                                    WHEN "00001100"  => --0Ch
                                        Instruct <= FSTRD4;
                                        bus_cycle_state := ADDRESS_BYTES;
                                        Check_freq := TRUE;
                                    WHEN "00001101"  => --0Dh
                                        Instruct <= DDRFR;
                                        bus_cycle_state := ADDRESS_BYTES;
                                        Check_freq := TRUE;
                                    WHEN "00001110"  => --0Eh
                                        Instruct <= DDRFR4;
                                        bus_cycle_state := ADDRESS_BYTES;
                                        Check_freq := TRUE;
                                    WHEN "00111011"  => --3Bh
                                        Instruct <= DOR;
                                        Check_freq := TRUE;
                                        bus_cycle_state := ADDRESS_BYTES;
                                    WHEN "00111100"  => --3Ch
                                        Instruct <= DOR4;
                                        bus_cycle_state := ADDRESS_BYTES;
                                        Check_freq := TRUE;
                                    WHEN "10111011"  => --BBh
                                        Instruct <= DIOR;
                                        bus_cycle_state := ADDRESS_BYTES;
                                        Check_freq := TRUE;
                                    WHEN "10111100"  => --BCh
                                        Instruct <= DIOR4;
                                        bus_cycle_state := ADDRESS_BYTES;
                                        Check_freq := TRUE;
                                    WHEN "10111101"  => --BDh
                                        Instruct <= DDRDIOR;
                                        bus_cycle_state := ADDRESS_BYTES;
                                        Check_freq := TRUE;
                                    WHEN "10111110"  => --BEh
                                        Instruct <= DDRDIOR4;
                                        bus_cycle_state := ADDRESS_BYTES;
                                        Check_freq := TRUE;
                                    WHEN "01101011"  => --6Bh
                                        Instruct <= QOR;
                                        bus_cycle_state := ADDRESS_BYTES;
                                        Check_freq := TRUE;
                                    WHEN "01101100"  => --6Ch
                                        Instruct <= QOR4;
                                        bus_cycle_state := ADDRESS_BYTES;
                                        Check_freq := TRUE;
                                    WHEN "11101011"  => --EBh
                                        Instruct <= QIOR;
                                        bus_cycle_state := ADDRESS_BYTES;
                                        Check_freq := TRUE;
                                    WHEN "11101100"  => --ECh
                                        Instruct <= QIOR4;
                                        bus_cycle_state := ADDRESS_BYTES;
                                        Check_freq := TRUE;
                                    WHEN "11101101"  => --EDh
                                        Instruct <= DDRQIOR;
                                        bus_cycle_state := ADDRESS_BYTES;
                                        Check_freq := TRUE;
                                    WHEN "11101110"  => --EEh
                                        Instruct <= DDRQIOR4;
                                        bus_cycle_state := ADDRESS_BYTES;
                                        Check_freq := TRUE;
                                    WHEN "00000010"  => --02h
                                        Instruct <= PP;
                                        bus_cycle_state := ADDRESS_BYTES;
                                    WHEN "00010010"  => --12h
                                        Instruct <= PP4;
                                        bus_cycle_state := ADDRESS_BYTES;
                                    WHEN "00110010"  => --32h
                                        Instruct <= QPP;
                                        bus_cycle_state := ADDRESS_BYTES;
                                        quadpg   <= TRUE;
                                    WHEN "00111000"  => --38h
                                        Instruct <= QPP;
                                        bus_cycle_state := ADDRESS_BYTES;
                                        quadpg   <= TRUE;
                                    WHEN "00110100"  => --34h
                                        Instruct <= QPP4;
                                        bus_cycle_state := ADDRESS_BYTES;
                                        quadpg   <= TRUE;
                                    WHEN "01000010"  => --42h
                                        Instruct <= OTPP;
                                        bus_cycle_state := ADDRESS_BYTES;
                                    WHEN "10000101"  => --85h
                                        Instruct <= PGSP;
                                        bus_cycle_state := DATA_BYTES;
                                    WHEN "10001010"  => --8Ah
                                        Instruct <= PGRS;
                                        bus_cycle_state := DATA_BYTES;
                                    WHEN "11000111"  => --C7h
                                        Instruct <= BE;
                                        bus_cycle_state := DATA_BYTES;
                                    WHEN "01100000"  => --60h
                                        Instruct <= BE;
                                        bus_cycle_state := DATA_BYTES;
                                    WHEN "11011000"  => --D8h
                                        Instruct <= SE;
                                        bus_cycle_state := ADDRESS_BYTES;
                                    WHEN "11011100"  => --DCh
                                        Instruct <= SE4;
                                        bus_cycle_state := ADDRESS_BYTES;
                                    WHEN "00100000"  => --20h
                                        Instruct <= P4E;
                                        bus_cycle_state := ADDRESS_BYTES;
                                    WHEN "00100001"  => --21h
                                        Instruct <= P4E4;
                                        bus_cycle_state := ADDRESS_BYTES;
                                    WHEN "01110101"  => --75h
                                        Instruct <= ERSP;
                                        bus_cycle_state := DATA_BYTES;
                                    WHEN "01111010"  => --7Ah
                                        Instruct <= ERRS;
                                        bus_cycle_state := DATA_BYTES;
                                    WHEN "00010100"  => --14h
                                        Instruct <= ABRD;
                                        bus_cycle_state := DATA_BYTES;
                                    WHEN "00010101"  => --15h
                                        Instruct <= ABWR;
                                        bus_cycle_state := DATA_BYTES;
                                    WHEN "00010110"  => --16h
                                        Instruct <= BRRD;
                                        bus_cycle_state := DATA_BYTES;
                                    WHEN "00010111"  => --17h
                                        Instruct <= BRWR;
                                        bus_cycle_state := DATA_BYTES;
                                    WHEN "10111001"  => --B9h
                                        Instruct <= BRAC;
                                        bus_cycle_state := DATA_BYTES;
                                    WHEN "00101011"  => --2Bh
                                        Instruct <= ASPRD;
                                        bus_cycle_state := DATA_BYTES;
                                    WHEN "00101111"  => --2Fh
                                        Instruct <= ASPP;
                                        bus_cycle_state := DATA_BYTES;
                                    WHEN "11100000"  => --E0h
                                        Instruct <= DYBRD;
                                        bus_cycle_state := ADDRESS_BYTES;
                                    WHEN "11100001"  => --E1h
                                        Instruct <= DYBWR;
                                        bus_cycle_state := ADDRESS_BYTES;
                                    WHEN "11100010"  => --E2h
                                        Instruct <= PPBRD;
                                        bus_cycle_state := ADDRESS_BYTES;
                                    WHEN "11100011"  => --E3h
                                        Instruct <= PPBP;
                                        bus_cycle_state := ADDRESS_BYTES;
                                    WHEN "00011000"  => --18h
                                        Instruct <= ECCRD;
                                        bus_cycle_state := ADDRESS_BYTES;
                                    WHEN "11100100"  => --E4h
                                        Instruct <= PPBERS;
                                        bus_cycle_state := DATA_BYTES;
                                    WHEN "10100110"  => --A6h
                                        Instruct <= PLBWR;
                                        bus_cycle_state := DATA_BYTES;
                                    WHEN "10100111"  => --A7h
                                        Instruct <= PLBRD;
                                        bus_cycle_state := DATA_BYTES;
                                    WHEN "11100111"  => --E7h
                                        Instruct <= PASSRD;
                                        bus_cycle_state := DATA_BYTES;
                                    WHEN "11101000"  => --E8h
                                        Instruct <= PASSP;
                                        bus_cycle_state := DATA_BYTES;
                                    WHEN "11101001"  => --E9h
                                        Instruct <= PASSU;
                                        bus_cycle_state := DATA_BYTES;
                                    WHEN "11110000"  => --F0h
                                        Instruct <= RESET;
                                        bus_cycle_state := DATA_BYTES;
                                    WHEN "11111111"  => --FFh
                                        Instruct <= MBR;
                                        bus_cycle_state := MODE_BYTE;
                                    WHEN "01000001"  => -- 41h
                                        Instruct <= DLPRD;
                                        bus_cycle_state := DATA_BYTES;
                                    WHEN "01000011"  => -- 43h
                                        Instruct <= PNVDLR;
                                        bus_cycle_state := DATA_BYTES;
                                    WHEN "01001010"  => -- 4Ah
                                        Instruct <= WVDLR;
                                        bus_cycle_state := DATA_BYTES;
                                    WHEN "00110000"  => --30h
                                        Instruct <= CLSR;
                                        bus_cycle_state := DATA_BYTES;
                                    WHEN "01011010"  => --5Ah
                                        Instruct <= RSFDP;
                                        bus_cycle_state := ADDRESS_BYTES;
                                    WHEN others =>
                                        null;

                                END CASE;
                            END IF;
                        END IF;
                    END IF;

                WHEN ADDRESS_BYTES =>
                    IF Instruct= DDRFR OR Instruct= DDRFR4 OR Instruct= DDRDIOR
                       OR Instruct = DDRDIOR4 OR Instruct = DDRQIOR OR
                       Instruct = DDRQIOR4 THEN
                       DOUBLE := TRUE;
                    ELSE
                       DOUBLE := FALSE;
                    END IF;

                    IF (rising_edge(SCK_ipd) AND NOT(DOUBLE) AND
                       (CSNeg_ipd= '0')) THEN
                        IF (((Instruct=FSTRD AND EXTADD= '0' )
                         OR  (Instruct=DOR  AND EXTADD= '0')
                         OR Instruct=OTPR OR Instruct=RSFDP) AND
                         ((HOLDNeg_pullup='1' AND QUAD='0') OR QUAD='1'))
                         OR  (Instruct=QOR  AND QUAD= '1' AND EXTADD='0') THEN
                            --Instruction + 3 Bytes Address + Dummy Byte
                            Address_in(addr_cnt) := SIIn;
                            addr_cnt := addr_cnt + 1;
                            IF addr_cnt = 3*BYTE THEN
                                FOR I IN 23 DOWNTO 0 LOOP
                                    addr_bytes(23-i) := Address_in(i);
                                END LOOP;
                                addr_bytes(31 downto 26) := "000000";
                                addr_bytes(25 downto 24) :=
                                           Bank_Addr_reg (1 downto 0);
                                Address <= to_nat(addr_bytes);
                                change_addr <= '1','0' AFTER 1 ns;
                                IF (Instruct=FSTRD OR Instruct=DOR OR
                                    Instruct=QOR) THEN
                                    IF (Latency_code = 3) THEN
                                        bus_cycle_state := DATA_BYTES;
                                    ELSE
                                        bus_cycle_state := DUMMY_BYTES;
                                    END IF;
                                ELSE
                                    bus_cycle_state := DUMMY_BYTES;
                                END IF;
                            END IF;
                        ELSIF (Instruct=ECCRD) THEN
                            --Instruction + 4 Bytes Address + Dummy Byte
                            Address_in(addr_cnt) := SIIn;
                            addr_cnt := addr_cnt + 1;
                            IF addr_cnt = 4*BYTE THEN
                                FOR I IN 31 DOWNTO 0 LOOP
                                    hiaddr_bytes(31-i) := Address_in(i);
                                END LOOP;
                                hiaddr_bytes(3 downto 0) := "0000";
                                Address<= to_nat(hiaddr_bytes);
                                change_addr <= '1','0' AFTER 1 ns;
                                bus_cycle_state := DUMMY_BYTES;
                            END IF;
                        ELSIF ((((Instruct=FSTRD4) OR (Instruct=DOR4) OR
                            ((Instruct=FSTRD) AND EXTADD= '1') OR
                            ((Instruct=DOR) AND EXTADD= '1')) AND
                            ((HOLDNeg_pullup='1' AND QUAD='0') OR QUAD='1')) OR
                            ((Instruct=QOR4) AND QUAD='1') OR
                            ((Instruct=QOR) AND QUAD='1' AND EXTADD='1')) THEN
                            --Instruction + 4 Bytes Address + Dummy Byte
                            Address_in(addr_cnt) := SIIn;
                            addr_cnt := addr_cnt + 1;
                            IF addr_cnt = 4*BYTE THEN
                                FOR I IN 31 DOWNTO 0 LOOP
                                    hiaddr_bytes(31-i) := Address_in(i);
                                END LOOP;
                                Address <= to_nat(hiaddr_bytes(25 downto 0));
                                change_addr <= '1','0' AFTER 1 ns;
                                IF Latency_code = 3 THEN
                                    bus_cycle_state := DATA_BYTES;
                                ELSE
                                    bus_cycle_state := DUMMY_BYTES;
                                    IF (DOUBLE AND NOT(hold_mode) AND
                                        VDLR_reg /= "00000000") THEN
                                        read_out <= '1', '0' AFTER 1 ns;
                                    END IF;
                                END IF;
                            END IF;
                        ELSIF Instruct = DIOR AND EXTADD= '0' AND
                           ((HOLDNeg_pullup='1' AND QUAD='0') OR QUAD='1') THEN
                            -- DUAL I/O High Performance Read (3Bytes Address)
                            IF SOIn /= 'Z' THEN
                                Address_in(2*addr_cnt)   := SOIn;
                                Address_in(2*addr_cnt+1) := SIIn;
                                read_cnt := 0;
                                addr_cnt := addr_cnt + 1;
                                IF addr_cnt = (3*BYTE) / 2 THEN
                                    addr_cnt := 0;
                                    FOR I IN 23 DOWNTO 0 LOOP
                                        addr_bytes(23-i) := Address_in(i);
                                    END LOOP;
                                    addr_bytes(31 downto 26) := "000000";
                                    addr_bytes(25 downto 24) :=
                                            Bank_Addr_reg (1 downto 0);
                                    Address <= to_nat(addr_bytes);
                                    change_addr <= '1','0' AFTER 1 ns;
                                    IF EHP THEN
                                        bus_cycle_state := MODE_BYTE;
                                    ELSE
                                        bus_cycle_state := DUMMY_BYTES;
                                    END IF;
                                END IF;
                            ELSE
                                bus_cycle_state := STAND_BY;
                            END IF;
                        ELSIF ((Instruct = DIOR4 OR
                              (Instruct = DIOR AND EXTADD= '1')) AND
                              ((HOLDNeg_pullup = '1' AND QUAD = '0') OR
                                QUAD = '1')) THEN
                            -- DUAL I/O High Performance Read (4Bytes Address)
                            IF SOIn /= 'Z' THEN
                                Address_in(2*addr_cnt)   := SOIn;
                                Address_in(2*addr_cnt+1) := SIIn;
                                read_cnt := 0;
                                addr_cnt := addr_cnt + 1;
                                IF addr_cnt = (4*BYTE) / 2 THEN
                                    addr_cnt := 0;
                                    FOR I IN 31 DOWNTO 0 LOOP
                                        hiaddr_bytes(31-i) := Address_in(i);
                                    END LOOP;
                                    Address <= to_nat(hiaddr_bytes(25 downto 0));
                                    change_addr <= '1','0' AFTER 1 ns;
                                    IF EHP THEN
                                        bus_cycle_state := MODE_BYTE;
                                    ELSE
                                        bus_cycle_state := DUMMY_BYTES;
                                    END IF;
                                END IF;
                            ELSE
                                bus_cycle_state := STAND_BY;
                            END IF;
                        ELSIF (Instruct = QIOR AND EXTADD= '0') THEN
                            -- QUAD I/O High Performance Read (3Bytes Address)
                            IF QUAD = '1' THEN
                                IF SOIn /= 'Z' THEN
                                    Address_in(4*addr_cnt)   := HOLDNegIn;
                                    Address_in(4*addr_cnt+1) := WPNegIn;
                                    Address_in(4*addr_cnt+2) := SOIn;
                                    Address_in(4*addr_cnt+3) := SIIn;
                                    read_cnt := 0;
                                    addr_cnt := addr_cnt + 1;
                                    IF addr_cnt = (3*BYTE) / 4 THEN
                                        addr_cnt := 0;
                                        FOR I IN 23 DOWNTO 0 LOOP
                                            addr_bytes(23-i) := Address_in(i);
                                        END LOOP;
                                        addr_bytes(31 downto 26) := "000000";
                                        addr_bytes(25 downto 24) :=
                                                Bank_Addr_reg (1 downto 0);
                                        Address <= to_nat(addr_bytes);
                                        change_addr <= '1','0' AFTER 1 ns;
                                        bus_cycle_state := MODE_BYTE;
                                    END IF;
                                END IF;
                            ELSE
                                bus_cycle_state := STAND_BY;
                            END IF;
                        ELSIF (Instruct = QIOR4 OR
                              (Instruct = QIOR AND EXTADD= '1')) THEN
                            -- QUAD I/O High Performance Read (4Bytes Address)
                            IF QUAD = '1' THEN
                                IF SOIn /= 'Z' THEN
                                    Address_in(4*addr_cnt)   := HOLDNegIn;
                                    Address_in(4*addr_cnt+1) := WPNegIn;
                                    Address_in(4*addr_cnt+2) := SOIn;
                                    Address_in(4*addr_cnt+3) := SIIn;
                                    read_cnt := 0;
                                    addr_cnt := addr_cnt + 1;
                                    IF addr_cnt = (4*BYTE) / 4 THEN
                                        addr_cnt := 0;
                                        FOR I IN 31 DOWNTO 0 LOOP
                                            hiaddr_bytes(31-i)
                                                        := Address_in(i);
                                        END LOOP;
                                        Address <= to_nat(hiaddr_bytes(25 downto 0));
                                        change_addr <= '1','0' AFTER 1 ns;
                                        bus_cycle_state := MODE_BYTE;
                                    END IF;
                                END IF;
                            ELSE
                                bus_cycle_state := STAND_BY;
                            END IF;
                        ELSIF (((Instruct = RD4  OR Instruct = PP4 OR
                              Instruct = SE4 OR Instruct = PPBRD OR
                              Instruct = DYBRD OR Instruct = DYBWR OR
                              Instruct = PPBP OR Instruct = P4E4 OR
                              (Instruct = READ AND EXTADD= '1' ) OR
                              (Instruct = PP AND EXTADD= '1' ) OR
                              (Instruct = P4E AND EXTADD= '1' ) OR
                              (Instruct = SE AND EXTADD= '1' )) AND
                              ((HOLDNeg_pullup='1' AND QUAD='0') OR QUAD='1'))
                              OR (QUAD ='1' AND (Instruct=QPP4 OR
                              (Instruct = QPP AND EXTADD= '1' ))))THEN
                            Address_in(addr_cnt) := SIIn;
                            addr_cnt := addr_cnt + 1;
                            IF addr_cnt = 4*BYTE THEN
                                FOR I IN 31 DOWNTO 0 LOOP
                                    hiaddr_bytes(31-i) := Address_in(i);
                                END LOOP;
                                Address <= to_nat(hiaddr_bytes(25 downto 0));
                                change_addr <= '1','0' AFTER 1 ns;
                                bus_cycle_state := DATA_BYTES;
                            END IF;
                        ELSIF (((HOLDNeg_pullup='1' AND QUAD='0')
                                 OR QUAD='1') AND EXTADD= '0') THEN
                            Address_in(addr_cnt) := SIIn;
                            addr_cnt := addr_cnt + 1;
                            IF addr_cnt = 3*BYTE THEN
                                FOR I IN 23 DOWNTO 0 LOOP
                                    addr_bytes(23-i) := Address_in(i);
                                END LOOP;
                                addr_bytes(31 downto 26) := "000000";
                                addr_bytes(25 downto 24) :=
                                           Bank_Addr_reg (1 downto 0);
                                Address <= to_nat(addr_bytes);
                                change_addr <= '1','0' AFTER 1 ns;
                                bus_cycle_state := DATA_BYTES;
                            END IF;
                        END IF;
                    ELSIF (SCK_ipd'EVENT AND DOUBLE AND addr_cnt /= 0 ) OR
                       (rising_edge(SCK_ipd) AND DOUBLE AND addr_cnt = 0 ) THEN
                        IF (Instruct=DDRFR AND EXTADD= '0' ) THEN
                            --Fast DDR Read Mode
                            Address_in(addr_cnt) := SIIn;
                            addr_cnt := addr_cnt + 1;
                            read_cnt := 0;
                            IF addr_cnt = 3*BYTE THEN
                                addr_cnt := 0;
                                FOR I IN 23 DOWNTO 0 LOOP
                                    addr_bytes(23-i) := Address_in(i);
                                END LOOP;
                                addr_bytes(31 downto 26) := "000000";
                                addr_bytes(25 downto 24) :=
                                           Bank_Addr_reg (1 downto 0);
                                Address <= to_nat(addr_bytes);
                                change_addr <= '1','0' AFTER 1 ns;
                                IF EHP THEN
                                    bus_cycle_state := MODE_BYTE;
                                ELSE
                                    bus_cycle_state := DUMMY_BYTES;
                                END IF;
                            END IF;
                        ELSIF (Instruct=DDRFR4 OR
                              (Instruct=DDRFR AND EXTADD= '1' )) THEN
                            Address_in(addr_cnt) := SIIn;
                            addr_cnt := addr_cnt + 1;
                            read_cnt := 0;
                            IF addr_cnt = 4*BYTE THEN
                                addr_cnt := 0;
                                FOR I IN 31 DOWNTO 0 LOOP
                                    addr_bytes(31-i) := Address_in(i);
                                END LOOP;
                                Address <= to_nat(addr_bytes(25 downto 0));
                                change_addr <= '1','0' AFTER 1 ns;
                                IF EHP THEN
                                    bus_cycle_state := MODE_BYTE;
                                ELSE
                                    bus_cycle_state := DUMMY_BYTES;
                                    IF ((DOUBLE AND NOT(hold_mode)) AND
                                         VDLR_reg /= "00000000") THEN
                                        read_out <= '1', '0' AFTER 1 ns;
                                    END IF;
                                END IF;
                            END IF;
                        ELSIF (Instruct= DDRDIOR AND EXTADD= '0' ) THEN
                            --Dual I/O DDR Read Mode
                            Address_in(2*addr_cnt)  := SOIn;
                            Address_in(2*addr_cnt+1):= SIIn;
                            addr_cnt := addr_cnt + 1;
                            read_cnt := 0;
                            IF addr_cnt = (3*BYTE)/2 THEN
                                addr_cnt := 0;
                                FOR I IN 23 DOWNTO 0 LOOP
                                    addr_bytes(23-i) := Address_in(i);
                                END LOOP;
                                addr_bytes(31 downto 26) := "000000";
                                addr_bytes(25 downto 24) :=
                                           Bank_Addr_reg (1 downto 0);
                                Address <= to_nat(addr_bytes);
                                change_addr <= '1','0' AFTER 1 ns;
                                IF EHP THEN
                                    bus_cycle_state := MODE_BYTE;
                                ELSE
                                    bus_cycle_state := DUMMY_BYTES;
                                END IF;
                            END IF;
                        ELSIF (Instruct=DDRDIOR4 OR
                              (Instruct=DDRDIOR AND EXTADD= '1' )) THEN
                             --Dual I/O DDR Read Mode
                            Address_in(2*addr_cnt)   := SOIn;
                            Address_in(2*addr_cnt+1) := SIIn;
                            addr_cnt := addr_cnt + 1;
                            read_cnt := 0;
                            IF addr_cnt = (4*BYTE)/2 THEN
                                addr_cnt := 0;
                                FOR I IN 31 DOWNTO 0 LOOP
                                    addr_bytes(31-i) := Address_in(i);
                                END LOOP;
                                Address <= to_nat(addr_bytes(25 downto 0));
                                change_addr <= '1','0' AFTER 1 ns;
                                IF EHP THEN
                                    bus_cycle_state := MODE_BYTE;
                                ELSE
                                    bus_cycle_state := DUMMY_BYTES;
                                    IF ((DOUBLE AND NOT(hold_mode)) AND
                                         VDLR_reg /= "00000000") THEN
                                        read_out <= '1', '0' AFTER 1 ns;
                                    END IF;
                                END IF;
                            END IF;
                        ELSIF (Instruct=DDRQIOR AND EXTADD= '0' ) AND
                               QUAD = '1' THEN
                            --Quad I/O DDR Read Mode
                            Address_in(4*addr_cnt)   := HOLDNegIn;
                            Address_in(4*addr_cnt+1) := WPNegIn;
                            Address_in(4*addr_cnt+2) := SOIn;
                            Address_in(4*addr_cnt+3) := SIIn;
                            addr_cnt := addr_cnt + 1;
                            read_cnt := 0;
                            IF addr_cnt = (3*BYTE)/4 THEN
                                addr_cnt := 0;
                                FOR I IN 23 DOWNTO 0 LOOP
                                    addr_bytes(23-i) := Address_in(i);
                                END LOOP;
                                addr_bytes(31 downto 26) := "000000";
                                addr_bytes(25 downto 24) :=
                                           Bank_Addr_reg (1 downto 0);
                                Address <= to_nat(addr_bytes);
                                change_addr <= '1','0' AFTER 5 ns;
                                bus_cycle_state := MODE_BYTE;
                            END IF;
                        ELSIF QUAD = '1' AND (Instruct=DDRQIOR4 OR
                              (Instruct=DDRQIOR AND EXTADD= '1' )) THEN
                            Address_in(4*addr_cnt)   := HOLDNegIn;
                            Address_in(4*addr_cnt+1) := WPNegIn;
                            Address_in(4*addr_cnt+2) := SOIn;
                            Address_in(4*addr_cnt+3) := SIIn;
                            addr_cnt := addr_cnt + 1;
                            read_cnt := 0;
                            IF addr_cnt = (4*BYTE)/4 THEN
                                addr_cnt := 0;
                                FOR I IN 31 DOWNTO 0 LOOP
                                    addr_bytes(31-i) := Address_in(i);
                                END LOOP;
                                Address <= to_nat(addr_bytes(25 downto 0));
                                change_addr <= '1','0' AFTER 1 ns;
                                bus_cycle_state := MODE_BYTE;
                            END IF;
                        END IF;
                    END IF;

                WHEN MODE_BYTE =>
                    IF rising_edge(SCK_ipd) THEN
                        IF ((Instruct=DIOR OR Instruct = DIOR4) AND
                         ((HOLDNeg_pullup='1' AND QUAD='0') OR QUAD='1')) THEN
                            mode_in(2*mode_cnt)   := SOIn;
                            mode_in(2*mode_cnt+1) := SIIn;
                            mode_cnt := mode_cnt + 1;
                            IF mode_cnt = BYTE/2 THEN
                                mode_cnt := 0;
                                FOR I IN 7 DOWNTO 0 LOOP
                                    mode_bytes(i) := mode_in(7-i);
                                END LOOP;
                                IF Latency_code = 0 OR Latency_code = 3 THEN
                                    bus_cycle_state := DATA_BYTES;
                                ELSE
                                    bus_cycle_state := DUMMY_BYTES;
                                END IF;
                            END IF;
                        ELSIF (Instruct=QIOR OR Instruct = QIOR4)
                               AND QUAD = '1' THEN
                            mode_in(4*mode_cnt)   := HOLDNegIn;
                            mode_in(4*mode_cnt+1) := WPNegIn;
                            mode_in(4*mode_cnt+2) := SOIn;
                            mode_in(4*mode_cnt+3) := SIIn;
                            mode_cnt := mode_cnt + 1;
                            IF mode_cnt = BYTE/4 THEN
                                mode_cnt := 0;
                                FOR I IN 7 DOWNTO 0 LOOP
                                    mode_bytes(i) := mode_in(7-i);
                                END LOOP;
                                bus_cycle_state := DUMMY_BYTES;
                            END IF;
                        ELSIF Instruct=DDRFR OR Instruct = DDRFR4 THEN
                            mode_in(2*mode_cnt)   := SIIn;
                        ELSIF Instruct=DDRDIOR OR Instruct = DDRDIOR4 THEN
                            mode_in(4*mode_cnt)   := SOIn;
                            mode_in(4*mode_cnt+1) := SIIn;
                        ELSIF (Instruct=DDRQIOR OR Instruct = DDRQIOR4)
                             AND QUAD = '1' THEN
                            mode_in(0) := HOLDNegIn;
                            mode_in(1) := WPNegIn;
                            mode_in(2) := SOIn;
                            mode_in(3) := SIIn;
                        END IF;
                        dummy_cnt := 0;
                        dummy_cnt_act_temp <= dummy_cnt;
                    ELSIF falling_edge(SCK_ipd) THEN
                        IF Instruct=DDRFR OR Instruct = DDRFR4 THEN
                            mode_in(2*mode_cnt+1)   := SIIn;
                            mode_cnt := mode_cnt + 1;
                            IF mode_cnt = BYTE/2 THEN
                                mode_cnt := 0;
                                FOR I IN 7 DOWNTO 0 LOOP
                                    mode_bytes(i) := mode_in(7-i);
                                END LOOP;
                                bus_cycle_state := DUMMY_BYTES;
                            END IF;
                        ELSIF Instruct=DDRDIOR OR Instruct = DDRDIOR4 THEN
                            mode_in(4*mode_cnt+2) := SOIn;
                            mode_in(4*mode_cnt+3) := SIIn;
                            mode_cnt := mode_cnt + 1;
                            IF mode_cnt = BYTE/4 THEN
                                mode_cnt := 0;
                                FOR I IN 7 DOWNTO 0 LOOP
                                    mode_bytes(i) := mode_in(7-i);
                                END LOOP;
                                bus_cycle_state := DUMMY_BYTES;
                            END IF;
                        ELSIF (Instruct=DDRQIOR OR Instruct = DDRQIOR4)
                               AND QUAD = '1' THEN
                            mode_in(4) := HOLDNegIn;
                            mode_in(5) := WPNegIn;
                            mode_in(6) := SOIn;
                            mode_in(7) := SIIn;
                            FOR I IN 7 DOWNTO 0 LOOP
                                mode_bytes(i) := mode_in(7-i);
                            END LOOP;
                            bus_cycle_state := DUMMY_BYTES;
                        END IF;
                    END IF;

                WHEN DUMMY_BYTES =>
                    IF rising_edge(SCK_ipd) THEN
                        IF (((Instruct=FSTRD OR Instruct=FSTRD4 OR
                              Instruct=DOR OR Instruct=DOR4 OR
                              Instruct=OTPR OR Instruct=RSFDP) AND
                           ((HOLDNeg_pullup='1' AND QUAD='0') OR QUAD='1')) OR
                           ((Instruct = QOR OR Instruct = QOR4) AND
                             QUAD = '1'))THEN
                            dummy_cnt := dummy_cnt + 1;
                            dummy_cnt_act_temp <= dummy_cnt;
                            IF dummy_cnt = BYTE THEN
                                bus_cycle_state := DATA_BYTES;
                            END IF;
                        ELSIF (Instruct=DDRFR OR Instruct=DDRFR4) THEN
                            dummy_cnt := dummy_cnt + 1;
                            dummy_cnt_act_temp <= dummy_cnt;
                            IF EHP THEN
                                IF ((Latency_code = 3 AND dummy_cnt=1) OR
                                (Latency_code = 0 AND dummy_cnt=2) OR
                                (Latency_code = 1 AND dummy_cnt=4) OR
                                (Latency_code = 2 AND dummy_cnt=5)) THEN
                                    bus_cycle_state := DATA_BYTES;
                                END IF;
                                IF VDLR_reg/="00000000" AND Latency_code=2 THEN
                                    IF dummy_cnt>1 THEN
                                        read_out <= '1', '0' AFTER 1 ns;
                                        IF dummy_cnt=5 THEN
                                            DummyBytes_act  := '1';
                                        END IF;
                                    END IF;
                                END IF;
                            ELSE
                                IF ((Latency_code = 3 AND dummy_cnt = 4) OR
                                (Latency_code = 0 AND dummy_cnt = 5) OR
                                (Latency_code = 1 AND dummy_cnt = 6) OR
                                (Latency_code = 2 AND dummy_cnt = 7)) THEN
                                    bus_cycle_state := DATA_BYTES;
                                 END IF;
                                IF VDLR_reg/="00000000" THEN
                                    IF Latency_code=0 THEN
                                        IF dummy_cnt>1 THEN
                                            read_out <= '1', '0' AFTER 1 ns;
                                            IF dummy_cnt=5 THEN
                                                DummyBytes_act  := '1';
                                            END IF;
                                        END IF;
                                    ELSIF Latency_code = 1 THEN
                                        IF dummy_cnt>2 THEN
                                            read_out <= '1', '0' AFTER 1 ns;
                                            IF dummy_cnt=6 THEN
                                                DummyBytes_act  := '1';
                                            END IF;
                                        END IF;
                                    ELSIF Latency_code = 2 THEN
                                        IF dummy_cnt>3 THEN
                                            read_out <= '1', '0' AFTER 1 ns;
                                            IF dummy_cnt=7 THEN
                                                DummyBytes_act  := '1';
                                            END IF;
                                        END IF;
                                    END IF;
                                END IF;
                             END IF;
                        ELSIF Instruct=RES THEN
                            dummy_cnt := dummy_cnt + 1;
                            dummy_cnt_act_temp <= dummy_cnt;
                            IF dummy_cnt = 3*BYTE THEN
                                bus_cycle_state := DATA_BYTES;
                            END IF;
                        ELSIF Instruct=ECCRD THEN
                            dummy_cnt := dummy_cnt + 1;
                            dummy_cnt_act_temp <= dummy_cnt;
                            IF dummy_cnt = BYTE THEN
                                bus_cycle_state := DATA_BYTES;
                            END IF;
                        ELSIF (Instruct = DIOR OR Instruct = DIOR4) AND
                           ((HOLDNeg_pullup='1' AND QUAD='0') OR QUAD='1') THEN
                            dummy_cnt := dummy_cnt + 1;
                            dummy_cnt_act_temp <= dummy_cnt;
                            IF EHP THEN
                                IF ((Latency_code = 1 AND dummy_cnt = 1) OR
                                    (Latency_code = 2 AND dummy_cnt = 2)) THEN
                                    bus_cycle_state := DATA_BYTES;
                                END IF;
                            ELSE
                                IF ((Latency_code = 3 AND dummy_cnt = 4) OR
                                    (Latency_code = 0 AND dummy_cnt = 4) OR
                                    (Latency_code = 1 AND dummy_cnt = 5) OR
                                    (Latency_code = 2 AND dummy_cnt = 6)) THEN
                                    bus_cycle_state := DATA_BYTES;
                                END IF;
                            END IF;
                        ELSIF Instruct = DDRDIOR OR Instruct = DDRDIOR4 THEN
                            dummy_cnt := dummy_cnt + 1;
                            dummy_cnt_act_temp <= dummy_cnt;
                            IF EHP THEN
                                IF ((Latency_code = 3 AND dummy_cnt = 2) OR
                                (Latency_code = 0 AND dummy_cnt = 4) OR
                                (Latency_code = 1 AND dummy_cnt = 5) OR
                                (Latency_code = 2 AND dummy_cnt = 6)) THEN
                                    bus_cycle_state := DATA_BYTES;
                                END IF;
                                IF VDLR_reg/="00000000" THEN
                                    IF Latency_code=1 THEN
                                        IF dummy_cnt>1 THEN
                                            read_out <= '1', '0' AFTER 1 ns;
                                            IF dummy_cnt=5 THEN
                                                DummyBytes_act  := '1';
                                            END IF;
                                        END IF;
                                    ELSIF Latency_code=2 THEN
                                        IF dummy_cnt>2 THEN
                                            read_out <= '1', '0' AFTER 1 ns;
                                            IF dummy_cnt=6 THEN
                                                DummyBytes_act  := '1';
                                            END IF;
                                        END IF;
                                    END IF;
                                END IF;
                            ELSE
                                IF ((Latency_code = 3 AND dummy_cnt = 4) OR
                                (Latency_code = 0 AND dummy_cnt = 6) OR
                                (Latency_code = 1 AND dummy_cnt = 7) OR
                                (Latency_code = 2 AND dummy_cnt = 8)) THEN
                                    bus_cycle_state := DATA_BYTES;
                                END IF;
                                IF VDLR_reg/="00000000" THEN
                                    IF Latency_code=0  THEN
                                        IF dummy_cnt>2 THEN
                                            read_out <= '1', '0' AFTER 1 ns;
                                            IF dummy_cnt=6 THEN
                                                DummyBytes_act  := '1';
                                            END IF;
                                        END IF;
                                    ELSIF Latency_code = 1 THEN
                                        IF dummy_cnt>3 THEN
                                            read_out <= '1', '0' AFTER 1 ns;
                                            IF dummy_cnt=7 THEN
                                                DummyBytes_act  := '1';
                                            END IF;
                                        END IF;
                                    ELSIF Latency_code = 2 THEN
                                        IF dummy_cnt>4 THEN
                                            read_out <= '1', '0' AFTER 1 ns;
                                            IF dummy_cnt=8 THEN
                                                DummyBytes_act  := '1';
                                            END IF;
                                        END IF;
                                    END IF;
                                END IF;
                            END IF;
                        ELSIF ((Instruct = QIOR OR Instruct = QIOR4) AND
                              QUAD = '1') THEN
                            dummy_cnt := dummy_cnt + 1;
                            dummy_cnt_act_temp <= dummy_cnt;
                            IF ((Latency_code = 3 AND dummy_cnt = 1) OR
                                (Latency_code = 0 AND dummy_cnt = 4) OR
                                (Latency_code = 1 AND dummy_cnt = 4) OR
                                (Latency_code = 2 AND dummy_cnt = 5)) THEN
                                bus_cycle_state := DATA_BYTES;
                            END IF;
                        ELSIF (Instruct = DDRQIOR OR Instruct = DDRQIOR4)
                              AND QUAD = '1' THEN
                            dummy_cnt := dummy_cnt + 1;
                            dummy_cnt_act_temp <= dummy_cnt;
                            IF ((Latency_code = 3 AND dummy_cnt = 3) OR
                            (Latency_code = 0 AND dummy_cnt = 6) OR
                            (Latency_code = 1 AND dummy_cnt = 7) OR
                            (Latency_code = 2 AND dummy_cnt = 8)) THEN
                                bus_cycle_state := DATA_BYTES;
                            END IF;
                            IF VDLR_reg/="00000000" THEN
                               IF Latency_code = 0 THEN
                                    IF dummy_cnt>2 THEN
                                        read_out <= '1', '0' AFTER 1 ns;
                                        IF dummy_cnt=6 THEN
                                            DummyBytes_act  := '1';
                                        END IF;
                                    END IF;
                                ELSIF Latency_code = 1 THEN
                                    IF dummy_cnt>3 THEN
                                        read_out <= '1', '0' AFTER 1 ns;
                                        IF dummy_cnt=7 THEN
                                            DummyBytes_act  := '1';
                                        END IF;
                                    END IF;
                                ELSIF Latency_code = 2 THEN
                                    IF dummy_cnt>4 THEN
                                        read_out <= '1', '0' AFTER 1 ns;
                                        IF dummy_cnt=8 THEN
                                            DummyBytes_act  := '1';
                                        END IF;
                                    END IF;
                                END IF;
                            END IF;
                        END IF;
                    ELSIF falling_edge(SCK_ipd) THEN
                        IF VDLR_reg /= "00000000" THEN
                            IF (Instruct=DDRFR OR Instruct=DDRFR4) THEN
                                IF EHP THEN
                                    IF (Latency_code = 2 AND dummy_cnt>=1) THEN
                                        read_out <= '1', '0' AFTER 1 ns;
                                    END IF;
                                ELSE
                                    IF (Latency_code = 0 AND dummy_cnt>=1) THEN
                                        read_out <= '1', '0' AFTER 1 ns;
                                    ELSIF (Latency_code = 1 AND dummy_cnt>=2) THEN
                                        read_out <= '1', '0' AFTER 1 ns;
                                    ELSIF (Latency_code = 2 AND dummy_cnt>=3) THEN
                                        read_out <= '1', '0' AFTER 1 ns;
                                    END IF;
                                END IF;
                            ELSIF (Instruct=DDRDIOR OR Instruct=DDRDIOR4) THEN
                                IF EHP THEN
                                    IF (Latency_code = 1 AND dummy_cnt>=1) THEN
                                        read_out <= '1', '0' AFTER 1 ns;
                                    ELSIF (Latency_code = 2 AND dummy_cnt>=2) THEN
                                        read_out <= '1', '0' AFTER 1 ns;
                                    END IF;
                                ELSE
                                    IF (Latency_code = 0 AND dummy_cnt>=2) THEN
                                        read_out <= '1', '0' AFTER 1 ns;
                                    ELSIF (Latency_code = 1 AND dummy_cnt>=3) THEN
                                        read_out <= '1', '0' AFTER 1 ns;
                                    ELSIF (Latency_code = 2 AND dummy_cnt>=4) THEN
                                        read_out <= '1', '0' AFTER 1 ns;
                                    END IF;
                                END IF;
                            ELSIF (Instruct=DDRQIOR OR Instruct=DDRQIOR4) THEN
                                IF (Latency_code = 0 AND dummy_cnt>=2) THEN
                                    read_out <= '1', '0' AFTER 1 ns;
                                ELSIF (Latency_code = 1 AND dummy_cnt>=3) THEN
                                    read_out <= '1', '0' AFTER 1 ns;
                                ELSIF (Latency_code = 2 AND dummy_cnt>=4) THEN
                                    read_out <= '1', '0' AFTER 1 ns;
                                END IF;
                            END IF;
                        END IF;
                    END IF;

                WHEN DATA_BYTES =>
                    IF rising_edge(CSNeg_ipd) THEN
                        IF ((mode_bytes(7 downto 4) = "1010" AND
                          (Instruct = DIOR OR Instruct = DIOR4 OR
                           Instruct = QIOR OR Instruct = QIOR4)) OR
                          ((mode_bytes(7 downto 4) =
                                    NOT(mode_bytes(3 downto 0))) AND
                          (Instruct = DDRFR  OR Instruct = DDRFR4  OR
                           Instruct = DDRDIOR OR Instruct = DDRDIOR4 OR
                           Instruct = DDRQIOR OR Instruct = DDRQIOR4))) THEN
                            bus_cycle_state := ADDRESS_BYTES;
                        ELSE
                            bus_cycle_state := STAND_BY;
                        END IF;
                    END IF;
                    IF falling_edge(SCK_ipd) AND CSNeg_ipd = '0' AND
                       NOT(DOUBLE) THEN
                        IF ((Instruct = READ OR Instruct=RD4 OR
                            Instruct = FSTRD OR Instruct = FSTRD4 OR
                            Instruct = RDSR  OR Instruct = RDSR2 OR
                            Instruct = RDCR  OR Instruct = OTPR OR
                            Instruct = RSFDP OR
                            Instruct = DOR  OR Instruct = DOR4 OR
                            Instruct = DIOR OR Instruct = DIOR4 OR
                            Instruct = ABRD  OR Instruct = BRRD OR
                            Instruct = ASPRD OR Instruct = DYBRD OR
                            Instruct = PPBRD OR Instruct = ECCRD OR
                            Instruct = PASSRD OR Instruct = RDID OR
                            Instruct = RES  OR Instruct = REMS OR
                            Instruct = PLBRD OR Instruct = DLPRD)
                           AND ((HOLDNeg_pullup='1' AND QUAD='0') OR QUAD='1'))
                           OR (current_state = AUTOBOOT AND start_delay = 0)OR
                          ((Instruct=QOR OR Instruct=QIOR OR Instruct=QOR4
                            OR Instruct= QIOR4) AND QUAD = '1') THEN
                            read_out <= '1', '0' AFTER 1 ns;
                        END IF;
                    ELSIF SCK_ipd'EVENT AND CSNeg_ipd = '0' AND DOUBLE THEN
                        read_out <= '1', '0' AFTER 1 ns;
                    END IF;

                    IF rising_edge(SCK_ipd) THEN
                        IF QUAD = '1' AND (Instruct=QPP OR Instruct = QPP4)THEN
                            quad_nybble := HOLDNegIn & WPNegIn & SOIn & SIIn;
                            IF data_cnt > (PageSize*2-1) THEN
                            --In case of quad mode and QPP,
                            --if more than 512 bytes are sent to the device
                                FOR I IN 0 TO (PageSize*2-2) LOOP
                                    quad_data_in(i) := quad_data_in(i+1);
                                END LOOP;
                                quad_data_in((PageSize*2-1)) :=
                                                    to_nat(quad_nybble);
                                data_cnt := data_cnt +1;
                            ELSE
                                IF quad_nybble /= "ZZZZ" THEN
                                    quad_data_in(data_cnt) :=
                                    to_nat(quad_nybble);
                                END IF;
                                    data_cnt := data_cnt +1;
                            END IF;
                        ELSIF ((HOLDNeg_pullup='1' AND QUAD='0')
                                OR QUAD='1') THEN
                            IF data_cnt > (PageSize*8-1) THEN
                            --In case of serial mode and PP,
                            -- if more than 512 bytes are sent to the device
                            -- previously latched data are discarded and last
                            -- 512 data bytes are guaranteed to be programmed
                            -- correctly within the same page.
                                IF bit_cnt = 0 THEN
                                    FOR I IN 0 TO ((PageSize-1)*BYTE - 1) LOOP
                                        Data_in(i) := Data_in(i+8);
                                    END LOOP;
                                END IF;
                                Data_in((PageSize-1)*BYTE + bit_cnt) := SIIn;
                                bit_cnt := bit_cnt + 1;
                                IF bit_cnt = 8 THEN
                                    bit_cnt := 0;
                                END IF;
                                data_cnt := data_cnt + 1;
                            ELSE
                                Data_in(data_cnt) := SIIn;
                                data_cnt := data_cnt + 1;
                                bit_cnt := 0;
                            END IF;
                        END IF;
                    END IF;

                    IF rising_edge(CSNeg_ipd) THEN
                    CASE Instruct IS
                        WHEN WREN | WRDI | BE | SE | SE4 | CLSR | RESET |
                             PPBP | PPBERS | PGSP | PGRS | ERSP | BRAC |
                             ERRS | P4E | P4E4 | PLBWR =>
                            IF ((HOLDNeg_pullup='1' AND QUAD='0')
                                OR QUAD='1') THEN
                                IF data_cnt = 0 THEN
                                    write <= '0';
                                END IF;
                            END IF;

                        WHEN WRR =>
                            IF ((HOLDNeg_pullup='1' AND QUAD='0')
                                OR QUAD='1') THEN
                                IF ((data_cnt mod 8) = 0 AND data_cnt > 0) THEN
                                    IF data_cnt = 8 THEN
                                    --If CS# is driven high after eight
                                    --cycle,only the Status Register is
                                    --written to.
                                        write <= '0';
                                        IF BAR_ACC = '0' THEN
                                            FOR i IN 0 TO 7 LOOP
                                                Status_reg1_in(i) <=
                                                                   Data_in(7-i);
                                            END LOOP;
                                        ELSIF P_ERR = '0' AND E_ERR = '0' THEN
                                            FOR i IN 0 TO 7 LOOP
                                                Bank_Addr_reg_in(i) <=
                                                                   Data_in(7-i);
                                            END LOOP;
                                        END IF;
                                    ELSIF data_cnt = 16 THEN
                                    --After the 16th cycle both the
                                    --Status and Configuration Registers
                                    --are written to.
                                        write <= '0';
                                        IF BAR_ACC = '0' THEN
                                            FOR i IN 0 TO 7 LOOP
                                                cfg_write  <= '1';
                                                FOR i IN 0 TO 7 LOOP
                                                    Status_reg1_in(i) <=
                                                                   Data_in(7-i);
                                                    Config_reg1_in(i) <=
                                                                  Data_in(15-i);
                                                END LOOP;
                                            END LOOP;
                                        ELSIF P_ERR = '0' AND E_ERR = '0' THEN
                                            FOR i IN 0 TO 7 LOOP
                                                Bank_Addr_reg_in(i) <=
                                                                   Data_in(7-i);
                                            END LOOP;
                                        END IF;
                                    END IF;
                                END IF;
                            END IF;

                        WHEN PP | PP4 | OTPP =>
                            IF ((HOLDNeg_pullup='1' AND QUAD='0') OR
                                 QUAD='1') THEN
                                IF data_cnt > 0 THEN
                                    IF (data_cnt mod 8) = 0 THEN
                                        write <= '0';
                                        FOR I IN 0 TO (PageSize-1) LOOP
                                            FOR J IN 7 DOWNTO 0 LOOP
                                                IF Data_in((i*8) + (7-j))
                                                                  /= 'X' THEN
                                                    Byte_slv(j) :=
                                                    Data_in((i*8) + (7-j));
                                                END IF;
                                            END LOOP;
                                            WByte(i) <=
                                                to_nat(Byte_slv);
                                        END LOOP;
                                        IF data_cnt > PageSize*BYTE THEN
                                            Byte_number <= PageSize-1;
                                        ELSE
                                            Byte_number <= data_cnt/8-1;
                                        END IF;
                                    END IF;
                                END IF;
                            END IF;

                        WHEN QPP | QPP4=>
                            IF data_cnt > 0 THEN
                                IF data_cnt mod 2 = 0 THEN
                                    quadpg   <= FALSE;
                                    write <= '0';
                                    FOR I IN 0 TO (PageSize-1) LOOP
                                        FOR J IN 1 DOWNTO 0 LOOP
                                            Quad_slv :=
                                            to_slv(quad_data_in((i*2) +
                                              (1-j)),4);

                                            Byte_slv(4*j+3 DOWNTO 4*j) :=
                                                Quad_slv;
                                        END LOOP;
                                        WByte(i) <= to_nat(Byte_slv);
                                    END LOOP;
                                    IF data_cnt > PageSize*2 THEN
                                        Byte_number <= PageSize-1;
                                    ELSE
                                        Byte_number <= data_cnt/2-1;
                                    END IF;
                                END IF;
                            END IF;

                        WHEN ABWR =>
                            IF ((HOLDNeg_pullup='1' AND QUAD='0') OR
                                 QUAD='1') THEN
                                IF data_cnt = 32 THEN
                                    write <= '0';
                                    FOR J IN 0 TO 31 LOOP
                                        AutoBoot_reg_in(J) <=
                                                Data_in(31-J);
                                    END LOOP;
                                END IF;
                            END IF;

                        WHEN BRWR =>
                            IF ((HOLDNeg_pullup='1' AND QUAD='0') OR
                                 QUAD='1') THEN
                                IF data_cnt = 8 THEN
                                    write <= '0';
                                    FOR J IN 0 TO 7 LOOP
                                        Bank_Addr_reg_in(J) <=
                                              Data_in(7-J);
                                    END LOOP;
                                END IF;
                            END IF;

                        WHEN ASPP =>
                            IF ((HOLDNeg_pullup='1' AND QUAD='0') OR
                                 QUAD='1') THEN
                                IF data_cnt = 16 THEN
                                    write <= '0';
                                    FOR J IN 0 TO 15 LOOP
                                        ASP_reg_in(J) <=
                                              Data_in(15-J);
                                    END LOOP;
                                END IF;
                            END IF;

                        WHEN DYBWR =>
                            IF ((HOLDNeg_pullup='1' AND QUAD='0') OR
                                 QUAD='1') THEN
                                IF data_cnt = 8 THEN
                                    write <= '0';
                                    FOR J IN 0 TO 7 LOOP
                                        DYBAR_in(J) <=
                                              Data_in(7-J);
                                    END LOOP;
                                END IF;
                            END IF;

                        WHEN PNVDLR =>
                            IF ((HOLDNeg_pullup='1' AND QUAD='0') OR
                                 QUAD='1') THEN
                                IF data_cnt = 8 THEN
                                    write <= '0';
                                    FOR J IN 0 TO 7 LOOP
                                        NVDLR_reg_in(J) <= Data_in(7-J);
                                    END LOOP;
                                END IF;
                            END IF;

                        WHEN WVDLR =>
                            IF ((HOLDNeg_pullup='1' AND QUAD='0') OR
                                 QUAD='1') THEN
                                IF data_cnt = 8 THEN
                                    write <= '0';
                                    FOR J IN 0 TO 7 LOOP
                                        VDLR_reg_in(J) <= Data_in(7-J);
                                    END LOOP;
                                END IF;
                            END IF;

                        WHEN PASSP =>
                            IF ((HOLDNeg_pullup='1' AND QUAD='0') OR
                                 QUAD='1') THEN
                                IF data_cnt = 64 THEN
                                    write <= '0';
                                    FOR J IN 1 TO 8 LOOP
                                        FOR K IN 1 TO 8 LOOP
                                            Password_reg_in(J*8-K) <=
                                                       Data_in(8*(J-1)+K-1);
                                        END LOOP;
                                    END LOOP;
                                END IF;
                            END IF;

                        WHEN PASSU =>
                            IF ((HOLDNeg_pullup='1' AND QUAD='0') OR
                                 QUAD='1') THEN
                                IF data_cnt = 64 THEN
                                    write <= '0';
                                    FOR J IN 1 TO 8 LOOP
                                        FOR K IN 1 TO 8 LOOP
                                            PASS_TEMP(J*8-K) <=
                                                       Data_in(8*(J-1)+K-1);
                                        END LOOP;
                                    END LOOP;
                                END IF;
                            END IF;

                        WHEN others =>
                            null;

                    END CASE;
                END IF;
            END CASE;
        END IF;

    END PROCESS BusCycleDecode;

    ---------------------------------------------------------------------------
    -- Timing control for the Page Program
    ---------------------------------------------------------------------------
    ProgTime : PROCESS(PSTART, PGSUSP, PGRES, RST_in, Reseted)
        VARIABLE pob      : time;
        VARIABLE elapsed  : time;
        VARIABLE start    : time;
        VARIABLE duration : time;
    BEGIN
        IF rising_edge(PSTART) AND PDONE = '1' THEN
            IF Instruct = PP OR Instruct = PP4 OR Instruct = OTPP OR
               Instruct = QPP OR Instruct = QPP4 THEN
                IF LongTimming THEN
                   pob  := tdevice_PP;
               ELSE
                   pob  := tdevice_PP / 10;
               END IF;
            ELSE
                IF LongTimming THEN
                   pob  := tdevice_BP;
               ELSE
                   pob  := tdevice_BP / 10;
               END IF;
            END IF;
            elapsed := 0 ns;
            start := NOW;
            PDONE <= '0', '1' AFTER pob;
        ELSIF PGSUSP'EVENT AND PGSUSP = '1' AND PDONE /= '1' THEN
            elapsed  := NOW - start;
            duration := pob - elapsed;
            PDONE <= '0';
        ELSIF PGRES'EVENT AND PGRES = '1' AND PDONE /= '1'THEN
            start := NOW;
            PDONE <= '0', '1' AFTER duration;
        END IF;

        IF rising_edge(RST_in) THEN
            PDONE <= '1';  -- reset done, programing terminated
        ELSIF rising_edge(Reseted) THEN
            PDONE <= '1';  -- reset done, programing terminated
        END IF;

    END PROCESS ProgTime;

    ---------------------------------------------------------------------------
    -- Timing control for the Max Setup Time
    ---------------------------------------------------------------------------
    SetupTime : PROCESS(SI, SCK)
        VARIABLE start_tsu       : time;
        VARIABLE elapsed_tsu     : time;
        VARIABLE duration_tsu    : time;
    BEGIN
        IF rising_edge(SI) OR falling_edge(SI) THEN
            IF Instruct = PGSP OR Instruct = PGRS OR
               Instruct = ERSP OR Instruct = ERRS THEN
               start_tsu := NOW;
            END IF;
        END IF;

        IF rising_edge(SCK) THEN
            IF Instruct = PGSP OR Instruct = PGRS OR
               Instruct = ERSP OR Instruct = ERRS THEN
               elapsed_tsu := NOW - start_tsu;
               duration_tsu := tdevice_TSU - elapsed_tsu;
               IF (duration_tsu < 0 ns) THEN
                   REPORT "Warning!" &
                    "tSU max time violation"
                   SEVERITY WARNING;
               END IF;
            END IF;
        END IF;

    END PROCESS SetupTime;
    ---------------------------------------------------------------------------
    -- Timing control for the Write Status Register
    ---------------------------------------------------------------------------
    WriteTime : PROCESS(WSTART, RST_in)
        VARIABLE wob      : time;
    BEGIN
        IF LongTimming THEN
            wob  := tdevice_WRR;
        ELSE
            wob  := tdevice_WRR / 1000;
        END IF;
        IF rising_edge(WSTART) AND WDONE = '1' THEN
            WDONE <= '0', '1' AFTER wob;
        END IF;

        IF rising_edge(RST_in) THEN
            WDONE <= '1';  -- reset done, programing terminated
        ELSIF rising_edge(Reseted) THEN
            WDONE <= '1';  -- reset done, programing terminated
        END IF;

    END PROCESS WriteTime;

    ---------------------------------------------------------------------------
    -- Timing control for the Bulk Erase
    ---------------------------------------------------------------------------
    ErsTime : PROCESS(ESTART, ESUSP, ERES, RST_in, Reseted)
        VARIABLE seo      : time;
        VARIABLE beo      : time;
        VARIABLE elapsed  : time;
        VARIABLE start    : time;
        VARIABLE duration : time;
    BEGIN
        IF LongTimming THEN
            seo := tdevice_SE;
            beo := tdevice_BE;
        ELSE
            seo := tdevice_SE / 1000;
            beo := tdevice_BE / 100000;
        END IF;
        IF rising_edge(ESTART) AND EDONE = '1' THEN
            IF Instruct = BE THEN
                duration := beo;
            ELSE --Instruct = SE OR SE4
                duration := seo;
            END IF;
            elapsed := 0 ns;
            EDONE <= '0', '1' AFTER duration;
            start := NOW;
        ELSIF ESUSP'EVENT AND ESUSP = '1' AND EDONE /= '1' THEN
            elapsed  := NOW - start;
            duration := duration - elapsed;
            EDONE <= '0';
        ELSIF ERES'EVENT AND ERES = '1' AND EDONE /= '1' THEN
            start := NOW;
            EDONE <= '0', '1' AFTER duration;
        END IF;

        IF rising_edge(RST_in) THEN
            EDONE <= '1';  -- reset done, eras terminated
        ELSIF rising_edge(Reseted) THEN
            EDONE <= '1';  -- reset done, erase terminated
        END IF;

    END PROCESS ErsTime;

    CheckCEOnPowerUP :PROCESS(CSNeg_ipd)
    BEGIN
        IF (PoweredUp = '0' AND falling_edge(CSNeg_ipd)) THEN
            REPORT InstancePath & partID &
            ": Device is selected during Power Up"
            SEVERITY WARNING;
        END IF;
    END PROCESS;

    ---------------------------------------------------------------------------
    -- Main Behavior Process
    -- combinational process for next state generation
    ---------------------------------------------------------------------------
    StateGen :PROCESS(PoweredUp, write, CSNeg_ipd, RSTNeg_pullup, WDONE, PDONE,
                      ERSSUSP_out, PRGSUSP_out, EDONE, RST_out, PPBERASE_in,
                      PASSULCK_in)

    VARIABLE sect      : NATURAL RANGE 0 TO SecNum;

    BEGIN

        IF rising_edge(PoweredUp) AND RSTNeg = '1' AND RST_out = '1' THEN
            IF ABE = '1' AND RPME /= '0' THEN
            --Autoboot is enabled and The Read Password feature is not enabled
                next_state <= AUTOBOOT;
                read_cnt    := 0;
                byte_cnt    := 1;
                read_addr   := to_nat(AutoBoot_reg(31 DOWNTO 9)&"000000000");
                start_delay := to_nat(AutoBoot_reg(8 DOWNTO 1));
                ABSD        := to_nat(AutoBoot_reg(8 DOWNTO 1));
            ELSE
                next_state <= IDLE;
            END IF;
        ELSIF PoweredUp = '1' THEN
            IF RST_out= '0' then
                next_state <= current_state;
            ELSIF falling_edge(write) AND Instruct = RESET THEN
                IF ABE = '1' AND RPME /= '0' THEN
                    read_cnt   := 0;
                    byte_cnt    := 1;
                    read_addr  := to_nat(AutoBoot_reg(31 DOWNTO 9)&
                                                      "000000000");
                    start_delay:= to_nat(AutoBoot_reg(8 DOWNTO 1));
                    ABSD       := to_nat(AutoBoot_reg(8 DOWNTO 1));
                    next_state <= AUTOBOOT;
                ELSE
                    next_state <= IDLE;
                END IF;
            ELSE
            CASE current_state IS
                WHEN  RESET_STATE =>
                    IF (rising_edge(RST_out) AND RSTNeg = '1') OR
                    (rising_edge(RSTNeg_pullup) AND RST_out = '1') THEN
                        IF ABE = '1' AND RPME /= '0'
                        AND RdPswdProtMode = '0' THEN
                            next_state <= AUTOBOOT;
                            read_cnt    := 0;
                            byte_cnt    := 1;
                            read_addr   := to_nat(AutoBoot_reg(31 DOWNTO 9)&
                                                               "000000000");
                            start_delay := to_nat(AutoBoot_reg(8 DOWNTO 1));
                            ABSD        := to_nat(AutoBoot_reg(8 DOWNTO 1));
                        ELSE
                            next_state <= IDLE;
                        END IF;
                    END IF;

                WHEN  IDLE =>
                    IF falling_edge(write) AND RdPswdProtMode = '0' THEN

                        IF (Instruct = WRR AND WEL = '1' AND BAR_ACC = '0') THEN
                            IF ((not(SRWD = '1' AND WPNeg_pullup = '0') AND
                                     QUAD = '0') OR QUAD = '1') THEN
                                -- can not execute if HPM is entered
                                -- or if WEL bit is zero
                                IF ((TBPROT='1' AND Config_reg1_in(5)='0') OR
                                    (BPNV  ='1' AND Config_reg1_in(3)='0')) AND
                                    cfg_write = '1' THEN
                                    ASSERT cfg_write = '0'
                                    REPORT "Changing value of Configuration " &
                                           "Register OTP bit from 1 to 0 is " &
                                           "not allowed!!!"
                                    SEVERITY WARNING;
                                ELSE
                                    next_state <= WRITE_SR;
                                END IF;
                            END IF;
                        ELSIF (Instruct = PP OR Instruct = QPP OR
                                Instruct = PP4 OR Instruct = QPP4) AND
                                WEL = '1' THEN
                            sect := Address/(SecSize+1);
                            pgm_page := Address/PageSize;
                            IF (Sec_Prot(sect) = '0' AND
                                PPB_bits(sect)='1' AND DYB_bits(sect)='1') THEN
--                                 IF (Instruct = QPP OR Instruct = QPP4)  AND
--                                     QPP_page(pgm_page) = '1' THEN
--                                     REPORT "WARNING:The same page must not be" &
--                                            " programmed more than once!!!"
--                                     SEVERITY WARNING;
--                                     next_state <=  PAGE_PG;
--                                 ELSE
                                    next_state <=  PAGE_PG;
--                                END IF;
                            END IF;
                        ELSIF Instruct=OTPP AND WEL = '1' AND FREEZE = '0' THEN
                            IF ((((Address >= 16#10# AND Address <= 16#13#) OR
                               (Address >= 16#20# AND Address <= 16#FF#))
                               AND LOCK_BYTE1(Address/32) = '1') OR
                               ((Address >= 16#100# AND Address <= 16#1FF#)
                               AND LOCK_BYTE2((Address-16#100#)/32) = '1') OR
                               ((Address >= 16#200# AND Address <= 16#2FF#)
                               AND LOCK_BYTE3((Address-16#200#)/32) = '1') OR
                               ((Address >= 16#300# AND Address <= 16#3FF#)
                               AND LOCK_BYTE4((Address-16#300#)/32) = '1')) AND
                              (Address + Byte_number <= OTPHiAddr) THEN
                                next_state <=  OTP_PG;
                            END IF;
                        ELSIF (Instruct= SE OR Instruct= SE4) AND WEL = '1' THEN
                            sect := Address/(SecSize+1);
                            IF (Sec_Prot(sect) = '0' AND PPB_bits(sect)='1'
                                AND DYB_bits(sect)='1') THEN
                                next_state <=  SECTOR_ERS;
                            END IF;
                        ELSIF (Instruct=P4E OR Instruct=P4E4) AND WEL='1' THEN
                            --A P4E instruction applied to a sector that
                            --is larger than 4 KB will not be executed
                            --and will not set the E_ERR status.
                            REPORT "Warning! Parameter 4KB-sector Erase "&
                                   "Instruction is not allowed for "&
                                   "512Mbit memory size!!! "&
                                   "Instruction is ignored!!!"
                            SEVERITY warning;
                        ELSIF Instruct = BE AND WEL = '1' AND
                          (BP0='0' AND BP1='0' AND BP2='0') THEN
                            next_state <= BULK_ERS;
                        ELSIF Instruct = ABWR   AND WEL = '1' THEN
                        --Autoboot Register Write Command
                            next_state <= AUTOBOOT_PG;
                        ELSIF Instruct = ASPP   AND WEL = '1' THEN
                        --ASP Register Program Command
                            IF not(ASPOTPFLAG) THEN
                                next_state <= ASP_PG;
                            END IF;
                        ELSIF Instruct = PLBWR  AND WEL = '1' AND
                            RdPswdProtEnable = '0' THEN
                            next_state <= PLB_PG;
                        ELSIF Instruct = PASSP  AND WEL = '1' THEN
                            IF not(PWDMLB='0' AND PSTMLB='1') THEN
                                next_state <= PASS_PG;
                            END IF;
                        ELSIF Instruct = PASSU  AND WEL= '1' AND WIP= '0' THEN
                            next_state <= PASS_UNLOCK;
                        ELSIF Instruct = PPBP   AND WEL = '1' THEN
                            next_state <= PPB_PG;
                        ELSIF (Instruct=PPBERS AND WEL='1' AND PPBOTP='1') THEN
                            next_state <= PPB_ERS;
                        ELSIF Instruct = DYBWR  AND WEL = '1' THEN
                            next_state <= DYB_PG;
                        ELSIF Instruct = PNVDLR  AND WEL = '1' THEN
                            next_state <= NVDLR_PG;
                        ELSE
                            next_state <= IDLE;
                        END IF;
                    END IF;
                    IF falling_edge(write) AND RdPswdProtMode = '1' AND
                       WIP = '0' THEN
                        IF Instruct = PASSU THEN
                            next_state <= PASS_UNLOCK;
                        END IF;
                    END IF;

                WHEN  AUTOBOOT =>
                    IF rising_edge(CSNeg_ipd) THEN
                        next_state <= IDLE;
                    END IF;

                WHEN WRITE_SR       =>
                    IF rising_edge(WDONE) THEN
                        next_state <= IDLE;
                    END IF;

                WHEN PAGE_PG        =>
                    IF  PRGSUSP_out'EVENT AND PRGSUSP_out = '1' THEN
                        next_state <= PG_SUSP;
                    ELSIF rising_edge(PDONE) THEN
                        next_state <= IDLE;
                    END IF;

                WHEN PG_SUSP      =>
                    IF falling_edge(write) THEN
                        IF Instruct = PGRS THEN
                            next_state <=  PAGE_PG;
                        ELSE
                            next_state <= PG_SUSP;
                        END IF;
                    ELSE
                        next_state <= PG_SUSP;
                    END IF;

                WHEN OTP_PG         =>
                    IF rising_edge(PDONE) THEN
                        next_state <= IDLE;
                    END IF;

                WHEN BULK_ERS       =>
                    IF rising_edge(EDONE) THEN
                        next_state <= IDLE;
                    END IF;

                WHEN SECTOR_ERS     =>
                    IF ERSSUSP_out'EVENT AND ERSSUSP_out = '1' THEN
                        next_state <= ERS_SUSP;
                    ELSIF rising_edge(EDONE) THEN
                        next_state <= IDLE;
                    END IF;

                WHEN ERS_SUSP      =>
                    IF falling_edge(write) THEN
                        IF (Instruct = PP OR Instruct = QPP OR
                            Instruct = PP4 OR Instruct = QPP4) AND
                            WEL = '1' THEN
                            IF SectorSuspend /= (Address/(SecSize+1)) THEN
                                pgm_page := Address / PageSize;
                                IF PPB_bits(Address/SecSize)='1' AND
                                   DYB_bits(Address/SecSize)='1' THEN
                                    next_state <=  ERS_SUSP_PG;
                                END IF;
                            END IF;
                        ELSIF (Instruct = DYBWR AND WEL = '1') THEN
                            next_state <=  DYB_PG;
                        ELSIF Instruct = ERRS THEN
                            next_state <=  SECTOR_ERS;
                        END IF;
                    ELSE
                        next_state <= ERS_SUSP;
                    END IF;

                WHEN ERS_SUSP_PG         =>
                    IF PRGSUSP_out'EVENT AND PRGSUSP_out = '1' THEN
                        next_state <= ERS_SUSP_PG_SUSP;
                    ELSIF rising_edge(PDONE) THEN
                        next_state <= ERS_SUSP;
                    END IF;

                WHEN ERS_SUSP_PG_SUSP      =>
                    IF falling_edge(write) THEN
                        IF Instruct = PGRS THEN
                            next_state <=  ERS_SUSP_PG;
                        ELSE
                            next_state <= ERS_SUSP_PG_SUSP;
                        END IF;
                    ELSE
                        next_state <= ERS_SUSP_PG_SUSP;
                    END IF;

                WHEN PASS_PG        =>
                    IF rising_edge(PDONE) THEN
                        next_state <= IDLE;
                    END IF;

                WHEN PASS_UNLOCK    =>
                    IF falling_edge(PASSULCK_in) THEN
                        next_state <= IDLE;
                    END IF;

                WHEN PPB_PG         =>
                    IF rising_edge(PDONE) THEN
                        next_state <= IDLE;
                    END IF;

                WHEN PPB_ERS        =>
                    IF falling_edge(PPBERASE_in) THEN
                        next_state <= IDLE;
                    END IF;

                WHEN AUTOBOOT_PG    =>
                    IF rising_edge(PDONE) THEN
                        next_state <= IDLE;
                    END IF;

                WHEN PLB_PG         =>
                    IF rising_edge(PDONE) THEN
                        next_state <= IDLE;
                    END IF;

                WHEN DYB_PG         =>
                    IF rising_edge(PDONE) THEN
                        IF ES = '1' THEN
                            next_state <= ERS_SUSP;
                        ELSE
                            next_state <= IDLE;
                        END IF;
                    END IF;

                WHEN ASP_PG         =>
                    IF rising_edge(PDONE) THEN
                        next_state <= IDLE;
                    END IF;

                WHEN NVDLR_PG         =>
                    IF rising_edge(PDONE) THEN
                        next_state <= IDLE;
                    END IF;

                END CASE;
            END IF;
        END IF;

    END PROCESS StateGen;

    ReadEnable: PROCESS (read_out)
    BEGIN
        oe_z <= rising_edge(read_out) AND PoweredUp = '1';

        IF read_out'EVENT AND read_out = '0' AND PoweredUp = '1' THEN
            oe   <= TRUE, FALSE AFTER 1 ns;
        END IF;
    END PROCESS ReadEnable;
    ---------------------------------------------------------------------------
    --FSM Output generation and general funcionality
    ---------------------------------------------------------------------------
    Functional : PROCESS(write,current_state,start_autoboot,CSNeg_ipd,
                         HOLDNeg_pullup, Instruct, Address, WByte,change_addr,
                         PoweredUp, WPNeg_pullup, WDONE, PDONE, EDONE,
                         PRGSUSP_out,ERSSUSP_out, PASSACC_out, oe, oe_z)

        VARIABLE WData          : WByteType:= (OTHERS => MaxData);

        VARIABLE AddrLo         : NATURAL;
        VARIABLE AddrHi         : NATURAL;
        VARIABLE Addr           : NATURAL;
        VARIABLE Addr_tmp       : NATURAL;

        VARIABLE data_out       : std_logic_vector(7 downto 0);
        VARIABLE ident_out      : std_logic_vector(647 downto 0);

        VARIABLE ExtendedID     : NATURAL;

        VARIABLE old_bit        : std_logic_vector(7 downto 0);
        VARIABLE new_bit        : std_logic_vector(7 downto 0);
        VARIABLE old_int        : INTEGER RANGE -1 to MaxData;
        VARIABLE new_int        : INTEGER RANGE -1 to MaxData;
        VARIABLE old_pass       : std_logic_vector(63 downto 0);
        VARIABLE new_pass       : std_logic_vector(63 downto 0);
        VARIABLE wr_cnt         : NATURAL RANGE 0 TO 511;
        --Data Learning Pattern Enable
        VARIABLE dlp_act        : BOOLEAN   := FALSE;

        VARIABLE sect           : NATURAL RANGE 0 TO SecNum;
        VARIABLE cnt            : NATURAL RANGE 0 TO 512 := 0;

    BEGIN

        -----------------------------------------------------------------------
        -- Functionality Section
        -----------------------------------------------------------------------

        IF Instruct'EVENT THEN
            read_cnt := 0;
            read_addr_tmp := 0;
            byte_cnt := 1;
            fast_rd  <= true;
            dual     <= false;
            rd       <= false;
            any_read <= false;
        END IF;

        IF PASSACC_out'EVENT AND PASSACC_out = '1' THEN
            WIP := '0';
            PASSACC_in <= '0';
        END IF;

        IF rising_edge(PoweredUp) THEN
            --the default condition after power-up
            --The Bank Address Register is loaded to all zeroes
            Bank_Addr_reg := (others => '0');
            --The Configuration Register FREEZE bit is cleared.
            FREEZE := '0';
            --The WEL bit is cleared.
            WEL := '0';
            --When BPNV is set to '1'. the BP2-0 bits in Status Register are
            --volatile and will be reset binary 111 after power-on reset
            IF BPNV = '1' AND FREEZE = '0' THEN
                BP0 := '1';
                BP1 := '1';
                BP2 := '1';
                BP_bits := BP2 & BP1 & BP0;
                change_BP <= '1', '0' AFTER 1 ns;
            END IF;
            --As shipped from the factory, all devices default ASP to the
            --Persistent Protection mode, with all sectors unprotected,
            --when power is applied. The device programmer or host system must
            --then choose which sector protection method to use.
            --For Persistent Protection mode, PPBLOCK defaults to "1"
            PPB_LOCK := '1';
            PPB_LOCK_temp <= '1';
            DYB_bits := (OTHERS => '1');

        END IF;

        IF falling_edge(write) AND Instruct = RESET THEN
            --The Configuration Register is set for Address mode
            Bank_Addr_reg := (others => '0');
            --P_ERR bit is cleared
            P_ERR := '0';
            --E_ERR bit is cleared
            E_ERR := '0';
            --The WEL bit is cleared.
            WEL   := '0';
            --The WIP bit is cleared.
            WIP   := '0';
            --The ES bit is cleared.
            ES    := '0';
            --The PS bit is cleared.
            PS    := '0';

            DummyBytes_act := '0';
            --When BPNV is set to '1'. the BP2-0 bits in Status
            --Register are volatile and will be reseted after
            --reset command
            IF BPNV = '1' AND FREEZE = '0' THEN
                BP0 := '1';
                BP1 := '1';
                BP2 := '1';
                BP_bits := BP2 & BP1 & BP0;
                change_BP <= '1', '0' AFTER 1 ns;
            END IF;
        END IF;

        IF change_addr'EVENT THEN
            read_addr := Address;
        END IF;

        CASE current_state IS
            WHEN IDLE          =>
                fast_rd <= true;
                rd      <= false;
                dual    <= false;
                ddr     <= false;
                IF falling_edge(write) AND RdPswdProtMode = '1' THEN
                    IF Instruct = PASSU THEN
                        IF WIP = '0' THEN
                            PASSULCK_in <= '1';
                            IF LongTimming = TRUE THEN
                                 PASSULCK_out <= '0', '1' AFTER (tdevice_PASSULCK);
                              ELSE
                                 PASSULCK_out <= '0', '1' AFTER (tdevice_PASSULCK/10);
                            END IF;
                        ELSE
                            REPORT "The PASSU command cannot be accepted"&
                                   " any faster than once every 100us"
                            SEVERITY WARNING;
                        END IF;
                    ELSIF Instruct = CLSR THEN
                    --The Clear Status Register Command resets bit SR1[5]
                    --(Erase Fail Flag) and bit SR1[6] (Program Fail Flag)
                       E_ERR := '0';
                       P_ERR := '0';
                    END IF;
                END IF;
                IF falling_edge(write) AND RdPswdProtMode = '0' THEN
                    read_cnt := 0;
                    byte_cnt    := 1;
                    IF Instruct = WREN THEN
                        WEL := '1';
                    ELSIF Instruct = WRDI THEN
                        WEL := '0';
                    ELSIF Instruct = WRR AND WEL = '1' AND BAR_ACC = '0' THEN
                        IF (not(SRWD = '1' AND WPNeg_pullup = '0') AND
                                QUAD ='0') OR QUAD ='1' THEN
                            -- can not execute if Hardware Protection Mode
                            -- is entered or if WEL bit is zero

                            IF ((TBPROT='1' AND Config_reg1_in(5)='0') OR
                                (BPNV  ='1' AND Config_reg1_in(3)='0')) AND
                                 cfg_write = '1' THEN
                                P_ERR := '1';
                            ELSE
                                WSTART <= '1', '0' AFTER 5 ns;
                                WIP := '1';
                            END IF;
                        ELSE
                            WEL := '0';
                        END IF;
                    ELSIF (Instruct = PP OR Instruct = PP4) AND WEL = '1' THEN
                        sect := Address/(SecSize+1);
                        pgm_page := Address/PageSize;
                        IF (Sec_Prot(sect) = '0' AND
                           PPB_bits(sect)='1' AND DYB_bits(sect)='1') THEN
                            PSTART <= '1', '0' AFTER 5 ns;
                            PGSUSP  <= '0';
                            PGRES   <= '0';
                            INITIAL_CONFIG <= '1';
                            WIP := '1';
                            SA <= sect;
                            Addr := Address;
                            Addr_tmp := Address;
                            wr_cnt := Byte_number;
                            FOR I IN wr_cnt DOWNTO 0 LOOP
                                IF Viol /= '0' THEN
                                    WData(i) := -1;
                                ELSE
                                    WData(i) := WByte(i);
                                END IF;
                            END LOOP;
                        ELSE
                            P_ERR := '1';
                            WEL   := '0';
                        END IF;
                    ELSIF (Instruct = QPP OR Instruct = QPP4) AND WEL = '1' THEN
                        sect := Address/(SecSize+1);
                        pgm_page := Address/PageSize;
                        IF (Sec_Prot(sect)= '0' AND
                           PPB_bits(sect)='1' AND DYB_bits(sect)='1') THEN
                            PSTART <= '1', '0' AFTER 5 ns;
                            PGSUSP  <= '0';
                            PGRES   <= '0';
                            INITIAL_CONFIG <= '1';
                            WIP := '1';
                            SA <= sect;
                            Addr := Address;
                            Addr_tmp := Address;
                            wr_cnt := Byte_number;
                            FOR I IN wr_cnt DOWNTO 0 LOOP
                                IF Viol /= '0' THEN
                                    WData(i) := -1;
                                ELSE
                                    WData(i) := WByte(i);
                                END IF;
                            END LOOP;
                        ELSE
                            P_ERR := '1';
                            WEL   := '0';
                        END IF;
                    ELSIF Instruct = OTPP AND WEL = '1' THEN
                    -- As long as the FREEZE bit remains cleared to a logic '0'
                    --the OTP address space is programmable.
                        IF FREEZE = '0' THEN
                            IF ((((Address >= 16#0010# AND Address <= 16#13#)OR
                                (Address >= 16#0020# AND Address <= 16#FF#))
                            AND LOCK_BYTE1(Address/32) = '1') OR
                            ((Address >= 16#100# AND Address <= 16#1FF#)
                            AND LOCK_BYTE2((Address-16#100#)/32) = '1') OR
                            ((Address >= 16#200# AND Address <= 16#2FF#)
                            AND LOCK_BYTE3((Address-16#200#)/32) = '1') OR
                            ((Address >= 16#300# AND Address <= 16#3FF#)
                            AND LOCK_BYTE4((Address-16#300#)/32) = '1')) AND
                            (Address + Byte_number <= OTPHiAddr) THEN
                                PSTART <= '1', '0' AFTER 1 ns;
                                WIP := '1';
                                Addr := Address;
                                Addr_tmp := Address;
                                wr_cnt := Byte_number;
                                FOR I IN wr_cnt DOWNTO 0 LOOP
                                    IF Viol /= '0' THEN
                                        WData(i) := -1;
                                    ELSE
                                        WData(i) := WByte(i);
                                    END IF;
                                END LOOP;
                            ELSIF (Address < 16#0010# OR (Address > 16#0013# AND
                                Address < 16#0020#) OR Address > 16#3FF# ) THEN
                                P_ERR := '1';
                                WEL   := '0';
                                IF Address < 16#0020# THEN
                                    ASSERT false
                                        REPORT "Given  address is in" &
                                            "reserved address range"
                                        SEVERITY warning;
                                ELSIF Address > 16#3FF# THEN
                                    ASSERT false
                                        REPORT "Given  address is out of" &
                                            "OTP address range"
                                        SEVERITY warning;
                                END IF;
                            ELSE
                                WEL   := '0';
                                P_ERR := '1';
                            END IF;
                        ELSE
                            WEL   := '0';
                            P_ERR := '1';
                        END IF;
                    ELSIF (Instruct = SE OR Instruct = SE4) AND WEL = '1' THEN
                        sect := Address/(SecSize+1);
                        SectorSuspend <= sect;
                        IF (Sec_Prot(sect) = '0' AND
                            PPB_bits(sect)='1' AND DYB_bits(sect)='1') THEN
                            ESTART <= '1', '0' AFTER 5 ns;
                            ESUSP  <= '0';
                            ERES   <= '0';
                            INITIAL_CONFIG <= '1';
                            WIP  := '1';
                            Addr := Address;
                        ELSE
                            E_ERR := '1';
                            WEL   := '0';
                        END IF;
                    ELSIF (Instruct = P4E OR Instruct = P4E4) AND WEL = '1' THEN
                        WEL := '0';
                    ELSIF Instruct = BE AND WEL = '1' THEN
                        IF (BP0='0' AND BP1='0' AND BP2='0') THEN
                            ESTART <= '1', '0' AFTER 5 ns;
                            ESUSP  <= '0';
                            ERES   <= '0';
                            INITIAL_CONFIG <= '1';
                            WIP := '1';
                        ELSE
                        --The Bulk Erase command will not set E_ERR if a
                        --protected sector is found during the command
                        --execution.
                            WEL   := '0';
                        END IF;
                    ELSIF Instruct = PASSP AND WEL = '1' THEN
                        IF not(PWDMLB='0' AND PSTMLB='1') THEN
                            PSTART <= '1', '0' AFTER 5 ns;
                            WIP := '1';
                        ELSE
                            REPORT "Password programming is not allowed" &
                                   " in Password Protection Mode."
                            SEVERITY warning;
                        END IF;
                    ELSIF Instruct = PASSU  AND WEL= '1' AND WIP= '0' THEN
                        PASSULCK_in <= '1';
                        IF LongTimming = TRUE THEN
                                 PASSULCK_out <= '0', '1' AFTER (tdevice_PASSULCK);
                              ELSE
                                 PASSULCK_out <= '0', '1' AFTER (tdevice_PASSULCK/10);
                            END IF;
                    ELSIF Instruct = BRWR THEN
                        Bank_Addr_reg(7) := Bank_Addr_reg_in(7);
                        Bank_Addr_reg(1) := Bank_Addr_reg_in(1);
                        Bank_Addr_reg(0) := Bank_Addr_reg_in(0);
                    ELSIF Instruct = WRR AND BAR_ACC = '1' THEN
                        IF (P_ERR = '0' AND E_ERR = '0') THEN
                            Bank_Addr_reg(1) := Bank_Addr_reg_in(1);
                            Bank_Addr_reg(0) := Bank_Addr_reg_in(0);
                        END IF;
                    ELSIF Instruct = ASPP AND WEL = '1' THEN
                        IF not(ASPOTPFLAG) THEN
                            PSTART <= '1', '0' AFTER 5 ns;
                            WIP := '1';
                        ELSE
                            WEL := '0';
                            P_ERR := '1';
                            REPORT "Once the Protection Mode is selected," &
                                   "no further changes to the ASP register" &
                                   "is allowed."
                            SEVERITY warning;
                        END IF;
                    ELSIF Instruct = ABWR AND WEL = '1' THEN
                        PSTART <= '1', '0' AFTER 5 ns;
                        WIP := '1';
                    ELSIF Instruct = PPBP AND WEL = '1' THEN
                        sect := Address/(SecSize+1);
                        Addr := Address;
                        pgm_page := Address/PageSize;

                        PSTART <= '1', '0' AFTER 5 ns;
                        WIP := '1';
                    ELSIF Instruct = PPBERS AND WEL = '1' THEN
                        IF PPBOTP = '1' THEN
                            PPBERASE_in <= '1';
                             IF LongTimming = TRUE THEN
                                 PPBERASE_out <= '0', '1' AFTER (tdevice_PPBERASE);
                              ELSE
                                 PPBERASE_out <= '0', '1' AFTER (tdevice_PPBERASE/100);
                            END IF;
                            WIP := '1';
                        ELSE
                            E_ERR := '1';
                        END IF;
                    ELSIF Instruct = PLBWR  AND WEL = '1' AND
                        RdPswdProtEnable = '0' THEN
                        PSTART <= '1', '0' AFTER 5 ns;
                        WIP := '1';
                    ELSIF Instruct = DYBWR  AND WEL = '1' THEN
                        sect := Address/(SecSize+1);
                        PSTART <= '1', '0' AFTER 5 ns;
                        WIP := '1';
                    ELSIF Instruct = PNVDLR  AND WEL = '1' THEN
                        PSTART <= '1', '0' AFTER 5 ns;
                        WIP := '1';
                    ELSIF Instruct = WVDLR  AND WEL = '1' THEN
                        VDLR_reg := VDLR_reg_in;
                        WEL := '0';
                    ELSIF Instruct = CLSR THEN
                    --The Clear Status Register Command resets bit SR1[5]
                    --(Erase Fail Flag) and bit SR1[6] (Program Fail Flag)
                       E_ERR := '0';
                       P_ERR := '0';
                    END IF;

                    IF Instruct = BRAC AND P_ERR = '0' AND E_ERR = '0' THEN
                        BAR_ACC <= '1';
                    ELSE
                        BAR_ACC <= '0';
                    END IF;
                ELSIF oe_z THEN
                    IF Instruct = READ OR Instruct = RD4 OR
                       Instruct = RES  OR Instruct = DLPRD THEN
                        fast_rd <= false;
                        rd      <= true;
                        dual    <= false;
                        ddr     <= false;
                    ELSIF Instruct = DDRFR OR Instruct = DDRFR4 THEN
                        fast_rd <= false;
                        rd      <= false;
                        dual    <= false;
                        ddr     <= true;
                    ELSIF Instruct = DDRDIOR OR Instruct = DDRDIOR4 THEN
                        fast_rd <= false;
                        rd      <= false;
                        dual    <= true;
                        ddr     <= true;
                    ELSIF Instruct = DOR OR Instruct=DOR4 OR
                          Instruct = DIOR OR Instruct = DIOR4 OR
                          Instruct = QOR OR Instruct=QOR4 OR
                          Instruct = QIOR OR Instruct= QIOR4 THEN
                        fast_rd <= false;
                        rd      <= false;
                        dual    <= true;
                        ddr     <= false;
                    ELSIF Instruct = DDRQIOR OR Instruct = DDRQIOR4 THEN
                        fast_rd <= false;
                        rd      <= false;
                        dual    <= true;
                        ddr     <= true;
                    ELSE
                        fast_rd <= true;
                        rd      <= false;
                        dual    <= false;
                        ddr     <= false;
                    END IF;
                    HOLDNegOut_zd <= 'Z';
                    WPNegOut_zd   <= 'Z';
                    SOOut_zd      <= 'Z';
                    SIOut_zd      <= 'Z';
                ELSIF oe THEN
                    any_read <= true;
                    IF Instruct = RDSR THEN
                        --Read Status Register 1
                        SOOut_zd <= Status_reg1(7-read_cnt);
                        read_cnt := read_cnt + 1;
                        IF read_cnt = 8 THEN
                            read_cnt := 0;
                        END IF;
                    ELSIF Instruct = RDSR2 THEN
                        --Read Status Register 2
                        SOOut_zd <= Status_reg2(7-read_cnt);
                        read_cnt := read_cnt + 1;
                        IF read_cnt = 8 THEN
                            read_cnt := 0;
                        END IF;
                    ELSIF Instruct = RDCR THEN
                        --Read Configuration Register 1
                        SOOut_zd <= Config_reg1(7-read_cnt);
                    --     read_cnt := read_cnt + 1;
                    --     IF read_cnt = 8 THEN
                    --         read_cnt := 0;
                    --     END IF;
                    -- ELSIF Instruct = RDCR THEN
                    --     --Read ECC Register
                    --     SOOut_zd <= ECCSR(7-read_cnt);
                        read_cnt := read_cnt + 1;
                        IF read_cnt = 8 THEN
                            read_cnt := 0;
                        END IF;
                    ELSIF Instruct = READ OR Instruct = RD4 OR
                          Instruct = FSTRD OR Instruct = FSTRD4 OR
                          Instruct = DDRFR OR Instruct = DDRFR4 THEN
                        --Read Memory array
                        IF Instruct = READ OR Instruct = RD4 THEN
                            fast_rd <= false;
                            rd      <= true;
                            dual    <= false;
                            ddr     <= false;
                        ELSIF Instruct = DDRFR OR Instruct = DDRFR4 THEN
                            fast_rd <= false;
                            rd      <= false;
                            dual    <= false;
                            ddr     <= true;
                        ELSE
                            fast_rd <= true;
                            rd      <= false;
                            dual    <= false;
                            ddr     <= false;
                        END IF;
                        IF bus_cycle_state = DUMMY_BYTES OR
                           DummyBytes_act='1' THEN
                            DummyBytes_act := '0';
                            IF (Instruct = DDRFR OR Instruct = DDRFR4) THEN
                                data_out := VDLR_reg;
                                SOOut_zd <= data_out(7-read_cnt);
                                read_cnt := read_cnt + 1;
                                IF read_cnt = 8 THEN
                                    read_cnt := 0;
                                END IF;
                            END IF;
                        ELSE
                            read_addr_tmp := read_addr;
                            SecAddr  := read_addr/(SecSize+1);
                            Sec_addr := read_addr - SecAddr*(SecSize+1);
                            SecAddr  := ReturnSectorIDRdPswdMd(TBPROT);
                            read_addr := Sec_addr + SecAddr*(SecSize+1);
                            IF RdPswdProtMode = '0' THEN
                                read_addr := read_addr_tmp;
                            END IF;
                            IF Mem(read_addr) /= -1 THEN
                                data_out := to_slv(Mem(read_addr),8);
                                SOOut_zd <= data_out(7-read_cnt);
                            ELSE
                                SOOut_zd <= 'U';
                            END IF;
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                                IF read_addr = AddrRANGE THEN
                                    read_addr := 0;
                                ELSE
                                    read_addr := read_addr + 1;
                                END IF;
                            END IF;
                        END IF;
                    ELSIF Instruct = DOR OR Instruct=DOR4 OR
                          Instruct = DIOR OR Instruct = DIOR4 OR
                          Instruct = DDRDIOR OR Instruct = DDRDIOR4 THEN
                        --Read Memory array
                        IF Instruct = DDRDIOR OR Instruct = DDRDIOR4 THEN
                            fast_rd <= false;
                            rd      <= false;
                            dual    <= true;
                            ddr     <= true;
                        ELSE
                            fast_rd <= false;
                            rd      <= false;
                            dual    <= true;
                            ddr     <= false;
                        END IF;
                        IF bus_cycle_state = DUMMY_BYTES
                        OR DummyBytes_act = '1' THEN
                            DummyBytes_act := '0';
                            IF (Instruct = DDRDIOR OR Instruct = DDRDIOR4) THEN
                                data_out := VDLR_reg;
                                SOOut_zd <= data_out(7-read_cnt);
                                SIOut_zd <= data_out(7-read_cnt);
                                read_cnt := read_cnt + 1;
                                IF read_cnt = 8 THEN
                                    read_cnt := 0;
                                END IF;
                            END IF;
                        ELSE
                            read_addr_tmp := read_addr;
                            SecAddr := read_addr/(SecSize+1);
                            Sec_addr := read_addr - SecAddr*(SecSize+1);
                            SecAddr := ReturnSectorIDRdPswdMd(TBPROT);
                            read_addr := Sec_addr + SecAddr*(SecSize+1);
                            IF RdPswdProtMode = '0' THEN
                                read_addr := read_addr_tmp;
                            END IF;
                            data_out := to_slv(Mem(read_addr),8);
                            SOOut_zd <= data_out(7-2*read_cnt);
                            SIOut_zd <= data_out(6-2*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 4 THEN
                                read_cnt := 0;
                                IF read_addr = AddrRANGE THEN
                                    read_addr := 0;
                                ELSE
                                    read_addr := read_addr + 1;
                                END IF;
                            END IF;
                        END IF;
                    ELSIF (Instruct = QOR OR Instruct=QOR4 OR
                           Instruct = QIOR OR Instruct= QIOR4 OR
                           Instruct = DDRQIOR OR Instruct= DDRQIOR4)
                           AND QUAD = '1' THEN
                        --Read Memory array
                        IF Instruct = DDRQIOR OR Instruct = DDRQIOR4 THEN
                            fast_rd <= false;
                            rd      <= false;
                            dual    <= true;
                            ddr     <= true;
                        ELSE
                            fast_rd <= false;
                            rd      <= false;
                            dual    <= true;
                            ddr     <= false;
                        END IF;

                        IF bus_cycle_state=DUMMY_BYTES OR DummyBytes_act='1' THEN
                            DummyBytes_act := '0';
                            IF Instruct = DDRQIOR OR Instruct = DDRQIOR4 THEN
                                data_out := VDLR_reg;
                                HOLDNegOut_zd <= data_out(7-read_cnt);
                                WPNegOut_zd   <= data_out(7-read_cnt);
                                SOOut_zd      <= data_out(7-read_cnt);
                                SIOut_zd      <= data_out(7-read_cnt);
                                read_cnt := read_cnt + 1;
                                IF read_cnt = 8 THEN
                                    read_cnt := 0;
                                END IF;
                            END IF;
                        ELSE
                            read_addr_tmp := read_addr;
                            SecAddr := read_addr/(SecSize+1);
                            Sec_addr := read_addr - SecAddr*(SecSize+1);
                            SecAddr := ReturnSectorIDRdPswdMd(TBPROT);
                            read_addr := Sec_addr + SecAddr*(SecSize+1);
                            IF RdPswdProtMode = '0' THEN
                                read_addr := read_addr_tmp;
                            END IF;

                            data_out := to_slv(Mem(read_addr),8);
                            HOLDNegOut_zd   <= data_out(7-4*read_cnt);
                            WPNegOut_zd     <= data_out(6-4*read_cnt);
                            SOOut_zd        <= data_out(5-4*read_cnt);
                            SIOut_zd        <= data_out(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                                IF read_addr = AddrRANGE THEN
                                    read_addr := 0;
                                ELSE
                                    read_addr := read_addr + 1;
                                END IF;
                            END IF;
                        END IF;
                    ELSIF Instruct = OTPR  THEN
                        IF (read_addr>=OTPLoAddr) AND
                           (read_addr<=OTPHiAddr) AND RdPswdProtMode = '0' THEN
                            --Read OTP Memory array
                            fast_rd <= true;
                            rd      <= false;
                            data_out := to_slv(OTPMem(read_addr),8);
                            SOOut_zd <= data_out(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                                read_addr := read_addr + 1;
                            END IF;
                        ELSIF (read_addr > OTPHiAddr)
                        OR RdPswdProtMode = '1' THEN
                        --OTP Read operation will not wrap to the
                        --starting address after the OTP address is at
                        --its maximum or Read Password Protection Mode
                        --is selected; instead, the data beyond the
                        --maximum OTP address will be undefined.
                            SOOut_zd <= 'U';
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        END IF;
                    ELSIF Instruct = REMS THEN
                        --Read Manufacturer and Device ID
                        IF read_addr MOD 2 = 0 THEN
                            data_out := to_slv(Manuf_ID,8);
                            SOOut_zd <= data_out(7 - read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                                read_addr := read_addr + 1;
                            END IF;
                        ELSE
                            data_out := to_slv(DeviceID,8);
                            SOOut_zd <= data_out(7 - read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                                read_addr := 0;
                            END IF;
                        END IF;

                    ELSIF Instruct = RDID THEN
                        IF (read_addr_tmp <= (SFDPHiAddr - 16#1000#)) THEN
                            data_out := to_slv(SFDP_array(16#1000# + read_addr_tmp), 8);
                            IF (read_addr_tmp <= IDCFILength) THEN
                                SOOut_zd      <= data_out(7 - read_cnt);
                                read_cnt := read_cnt + 1;
                                IF read_cnt = 8 THEN
                                    read_cnt := 0;
                                    read_addr_tmp := read_addr_tmp + 1;
                                END IF;
                            ELSE
                                SOOut_zd      <= 'X';
                            END IF;
                        ELSE
                            SOOut_zd      <= '1';
                        END IF;

                    ELSIF Instruct = RSFDP THEN
                        data_out := to_slv(SFDP_array(read_addr + read_addr_tmp),8);
                        IF (read_addr + read_addr_tmp) <= SFDPHiAddr THEN
                            SOOut_zd      <= data_out(7 - read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                                read_addr_tmp := read_addr_tmp + 1;
                            END IF;
                        ELSE
                            SOOut_zd      <= 'X';
                        END IF;
                    ELSIF Instruct = RES THEN
                        fast_rd <= false;
                        rd      <= true;
                        dual    <= false;
                        ddr     <= false;
                        data_out := to_slv(ESignature,8);
                        SOOut_zd <= data_out(7-read_cnt);
                        read_cnt := read_cnt + 1;
                        IF read_cnt = 8 THEN
                            read_cnt := 0;
                        END IF;
                    ELSIF Instruct = DLPRD THEN
                        fast_rd <= false;
                        rd      <= true;
                        dual    <= false;
                        ddr     <= false;
                        SOOut_zd <= VDLR_reg(7-read_cnt);
                        read_cnt := read_cnt + 1;
                        IF read_cnt = 8 THEN
                            read_cnt := 0;
                        END IF;
                    ELSIF Instruct = ABRD THEN
                        --Read AutoBoot register
                        fast_rd <= true;
                        rd      <= false;
                        dual    <= false;
                        ddr     <= false;
                        SOOut_zd <= AutoBoot_reg_in(31-read_cnt);
                        read_cnt := read_cnt + 1;
                        IF read_cnt = 32 THEN
                            read_cnt := 0;
                        END IF;
                    ELSIF Instruct = BRRD THEN
                        --Read Bank Address Register
                        fast_rd <= true;
                        rd      <= false;
                        dual    <= false;
                        ddr     <= false;
                        SOOut_zd <= Bank_Addr_reg(7-read_cnt);
                        read_cnt := read_cnt + 1;
                        IF read_cnt = 8 THEN
                            read_cnt := 0;
                        END IF;
                    ELSIF Instruct = ASPRD THEN
                        --Read ASP Register
                        fast_rd <= true;
                        rd      <= false;
                        dual    <= false;
                        ddr     <= false;
                        SOOut_zd <= ASP_reg(15-read_cnt);
                        read_cnt := read_cnt + 1;
                        IF read_cnt = 16 THEN
                            read_cnt := 0;
                        END IF;
                    ELSIF Instruct = PASSRD THEN
                        --Read Password Register
                        IF not (PWDMLB='0' AND PSTMLB='1') THEN
                            fast_rd <= true;
                            rd      <= false;
                            dual    <= false;
                            ddr     <= false;
                            SOOut_zd <= Password_reg((8*byte_cnt-1)-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                                byte_cnt := byte_cnt + 1;
                                IF byte_cnt = 9 THEN
                                   byte_cnt := 1;
                                END IF;
                            END IF;
                        ELSE
                            SOOut_zd <= 'U';
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 64 THEN
                                read_cnt := 0;
                            END IF;
                            IF read_cnt = 0 THEN
                                ASSERT false
                                REPORT "Verification of password is not " &
                                       " allowed in Password Protection Mode."
                                SEVERITY warning;
                            END IF;
                        END IF;
                    ELSIF Instruct = PLBRD THEN
                        --Read PPB Lock Register
                        fast_rd <= true;
                        rd      <= false;
                        dual    <= false;
                        ddr     <= false;
                        SOOut_zd <= PPBL(7-read_cnt);
                        read_cnt := read_cnt + 1;
                        IF read_cnt = 8 THEN
                            read_cnt := 0;
                        END IF;
                    ELSIF Instruct = DYBRD THEN
                        --Read DYB Access Register
                        sect := Address/(SecSize+1);
                        fast_rd <= true;
                        rd      <= false;
                        dual    <= false;
                        ddr     <= false;
                        DYBAR(7 downto 0) := "UUUUUUUU";
                        IF RdPswdProtMode = '0' THEN
                            IF DYB_bits(sect) = '1' THEN
                                DYBAR(7 downto 0) := "11111111";
                            ELSE
                                DYBAR(7 downto 0) := "00000000";
                            END IF;
                        END IF;
                        SOOut_zd <= DYBAR(7-read_cnt);
                        read_cnt := read_cnt + 1;
                        IF read_cnt = 8 THEN
                            read_cnt := 0;
                        END IF;
                    ELSIF Instruct = PPBRD THEN
                        --Read PPB Access Register
                        sect := Address/(SecSize+1);
                        fast_rd <= true;
                        rd      <= false;
                        dual    <= false;
                        ddr     <= false;
                        PPBAR(7 downto 0) := "UUUUUUUU";
                        IF RdPswdProtMode = '0' THEN
                            IF PPB_bits(sect) = '1' THEN
                                PPBAR(7 downto 0) := "11111111";
                            ELSE
                                PPBAR(7 downto 0) := "00000000";
                            END IF;
                        END IF;
                        SOOut_zd <= PPBAR(7-read_cnt);
                        read_cnt := read_cnt + 1;
                        IF read_cnt = 8 THEN
                            read_cnt := 0;
                        END IF;
                    END IF;
                END IF;

            WHEN AUTOBOOT       =>
                IF start_autoboot = '1' THEN
                    IF (oe) THEN
                        any_read <= true;
                        IF QUAD = '1' THEN
                            IF ABSD > 0 THEN      --If ABSD > 0,
                                fast_rd <= false; --max SCK frequency is 104MHz
                                rd      <= false;
                                dual    <= true;
                                ddr     <= false;
                            ELSE -- If ABSD = 0, max SCK frequency is 50 MHz
                                fast_rd <= false;
                                rd      <= true;
                                dual    <= false;
                                ddr     <= false;
                            END IF;
                            data_out := to_slv(Mem(read_addr),8);
                            HOLDNegOut_zd   <= data_out(7-4*read_cnt);
                            WPNegOut_zd     <= data_out(6-4*read_cnt);
                            SOOut_zd        <= data_out(5-4*read_cnt);
                            SIOut_zd        <= data_out(4-4*read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 2 THEN
                                read_cnt := 0;
                                read_addr := read_addr + 1;
                            END IF;
                        ELSE
                            IF ABSD > 0 THEN      --If ABSD > 0,
                                fast_rd <= true; --max SCK frequency is 133MHz
                                rd      <= false;
                                dual    <= false;
                                ddr     <= false;
                            ELSE -- If ABSD = 0, max SCK frequency is 50 MHz
                                fast_rd <= false;
                                rd      <= true;
                                dual    <= false;
                                ddr     <= false;
                            END IF;
                            data_out := to_slv(Mem(read_addr),8);
                            SOOut_zd <= data_out(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                                read_addr := read_addr + 1;
                            END IF;
                       END IF;
                    ELSIF oe_z THEN
                       IF QUAD = '1' THEN
                           IF ABSD > 0 THEN      --If ABSD > 0,
                               fast_rd <= false; --max SCK frequency is 104MHz
                               rd      <= false;
                               dual    <= true;
                               ddr     <= false;
                           ELSE -- If ABSD = 0, max SCK frequency is 50 MHz
                               fast_rd <= false;
                               rd      <= true;
                               dual    <= false;
                               ddr     <= false;
                           END IF;
                       ELSE
                           IF ABSD > 0 THEN      --If ABSD > 0,
                               fast_rd <= true; --max SCK frequency is 133MHz
                               rd      <= false;
                               dual    <= false;
                               ddr     <= false;
                           ELSE -- If ABSD = 0, max SCK frequency is 50 MHz
                               fast_rd <= false;
                               rd      <= true;
                               dual    <= false;
                               ddr     <= false;
                           END IF;
                       END IF;
                       HOLDNegOut_zd <= 'Z';
                       WPNegOut_zd   <= 'Z';
                       SOOut_zd      <= 'Z';
                       SIOut_zd      <= 'Z';
                    END IF;
                END IF;

            WHEN WRITE_SR       =>
                fast_rd <= true;
                rd      <= false;
                dual    <= false;
                ddr     <= false;
                IF oe THEN
                    any_read <= true;
                    IF Instruct = RDSR THEN
                        --Read Status Register 1
                        SOOut_zd <= Status_reg1(7-read_cnt);
                        read_cnt := read_cnt + 1;
                        IF read_cnt = 8 THEN
                            read_cnt := 0;
                        END IF;
                    ELSIF Instruct = RDSR2 THEN
                        --Read Status Register 2
                        SOOut_zd <= Status_reg2(7-read_cnt);
                        read_cnt := read_cnt + 1;
                        IF read_cnt = 8 THEN
                            read_cnt := 0;
                        END IF;
                    ELSIF Instruct = RDCR THEN
                        --Read Configuration Register 1
                        SOOut_zd <= Config_reg1(7-read_cnt);
                        read_cnt := read_cnt + 1;
                        IF read_cnt = 8 THEN
                            read_cnt := 0;
                        END IF;
                    END IF;
                ELSIF oe_z THEN
                    HOLDNegOut_zd <= 'Z';
                    WPNegOut_zd   <= 'Z';
                    SOOut_zd      <= 'Z';
                    SIOut_zd      <= 'Z';
                END IF;

                IF WDONE = '1' THEN
                    WIP  := '0';
                    WEL  := '0';
                    SRWD := Status_reg1_in(7);--MSB first

--                    IF LOCK='0' THEN
                        IF FREEZE='0' THEN
                        --The Freeze Bit, when set to 1, locks the current
                        --state of the BP2-0 bits in Status Register and
                        --the TBPROT bit in the Config Register
                        --As long as the FREEZE bit remains cleared to logic
                        --'0', the other bits of the Configuration register
                        --including FREEZE are writeable.
                            BP2  := Status_reg1_in(4);
                            BP1  := Status_reg1_in(3);
                            BP0  := Status_reg1_in(2);

                            BP_bits := BP2 & BP1 & BP0;

                            IF TBPROT = '0' AND INITIAL_CONFIG = '0' THEN
                                TBPROT  := Config_reg1_in(5);
                            END IF;

                            change_BP <= '1', '0' AFTER 1 ns;

                            LC1     := Config_reg1_in(7);
                            LC0     := Config_reg1_in(6);
                            QUAD    := Config_reg1_in(1);

                            IF FREEZE = '0' THEN
                                FREEZE    := Config_reg1_in(0);
                            END IF;

--                            IF WRLOCKENABLE AND LOCK = '0' THEN
--                                LOCK    := Config_reg1_in(4);
--                                WRLOCKENABLE <= false;
--                            END IF;

                            IF BPNV = '0' THEN
                                BPNV    := Config_reg1_in(3);
                            END IF;
                        END IF;
--                    END IF;
                END IF;

            WHEN PAGE_PG        =>
                fast_rd <= true;
                rd      <= false;
                dual    <= false;
                ddr     <= false;
                IF oe THEN
                    any_read <= true;
                    IF Instruct = RDSR THEN
                        --Read Status Register 1
                        SOOut_zd <= Status_reg1(7-read_cnt);
                        read_cnt := read_cnt + 1;
                        IF read_cnt = 8 THEN
                            read_cnt := 0;
                        END IF;
                    ELSIF Instruct = RDSR2 THEN
                        --Read Status Register 2
                        SOOut_zd <= Status_reg2(7-read_cnt);
                        read_cnt := read_cnt + 1;
                        IF read_cnt = 8 THEN
                            read_cnt := 0;
                        END IF;
                    ELSIF Instruct = RDCR THEN
                        --Read Configuration Register 1
                        SOOut_zd <= Config_reg1(7-read_cnt);
                        read_cnt := read_cnt + 1;
                        IF read_cnt = 8 THEN
                            read_cnt := 0;
                        END IF;
                    END IF;
                ELSIF oe_z THEN
                    HOLDNegOut_zd <= 'Z';
                    WPNegOut_zd   <= 'Z';
                    SOOut_zd      <= 'Z';
                    SIOut_zd      <= 'Z';
                END IF;

                IF falling_edge(PDONE) THEN
--                    IF (Instruct = QPP OR Instruct = QPP4) THEN
--                        QPP_page(pgm_page) := '1';
--                    END IF;

                    ADDRHILO_PG(AddrLo, AddrHi, Addr);
                    cnt := 0;

                    FOR i IN 0 TO wr_cnt LOOP
                        new_int := WData(i);
                        old_int := Mem(Addr + i - cnt);
                        IF new_int > -1 THEN
                            new_bit := to_slv(new_int,8);
                            IF old_int > -1 THEN
                                old_bit := to_slv(old_int,8);
                                FOR j IN 0 TO 7 LOOP
                                    IF old_bit(j) = '0' THEN
                                        new_bit(j) := '0';
                                    END IF;
                                END LOOP;
                                new_int := to_nat(new_bit);
                            END IF;
                            WData(i) := new_int;
                        ELSE
                            WData(i) := -1;
                        END IF;

                        Mem(Addr + i - cnt) :=  -1;

                        IF (Addr + i) = AddrHi THEN
                            Addr := AddrLo;
                            cnt := i + 1;
                        END IF;
                    END LOOP;
                    cnt :=0;
                END IF;

                IF PDONE = '1' THEN
                    WIP := '0';
                    WEL := '0';
                    FOR i IN 0 TO wr_cnt LOOP
                        Mem(Addr_tmp + i - cnt) := WData(i);
                        IF (Addr_tmp + i) = AddrHi THEN
                            Addr_tmp := AddrLo;
                            cnt := i + 1;
                        END IF;
                    END LOOP;

                ELSIF Instruct = PGSP AND PRGSUSP_in = '0' THEN
                    IF RES_TO_SUSP_MIN_TIME = '0' THEN
                        PGSUSP <= '1', '0' AFTER 1 ns;
                        PRGSUSP_in <= '1';
                        IF LongTimming = TRUE THEN
                            PRGSUSP_out <= '0', '1' AFTER (tdevice_PRGSUSP);
                         ELSE
                            PRGSUSP_out <= '0', '1' AFTER (tdevice_PRGSUSP/100);
                         END IF;
                        ASSERT RES_TO_SUSP_TYP_TIME = '0'
                        REPORT "Typical periods are needed for " &
                               "Program to progress to completion"
                        SEVERITY warning;
                    ELSE
                        ASSERT FALSE
                        REPORT "Minimum for tPRS is not satisfied! " &
                               "PGSP command is ignored"
                        SEVERITY warning;
                    END IF;
                END IF;

            WHEN PG_SUSP      =>
                fast_rd <= true;
                rd      <= false;
                dual    <= false;
                ddr     <= false;
                IF PRGSUSP_out = '1' AND PRGSUSP_in = '1' THEN
                    PRGSUSP_in <= '0';
                    --The RDY/BSY bit in the Status Register will indicate that
                    --the device is ready for another operation.
                    WIP := '0';
                    --The Program Suspend (PS) bit in the Status Register will
                    --be set to the logical “1” state to indicate that the
                    --program operation has been suspended.
                    PS := '1';
                END IF;

                IF oe THEN
                    any_read <= true;
                    IF Instruct = RDSR THEN
                        --Read Status Register 1
                        SOOut_zd <= Status_reg1(7-read_cnt);
                        read_cnt := read_cnt + 1;
                        IF read_cnt = 8 THEN
                            read_cnt := 0;
                        END IF;
                    ELSIF Instruct = RDSR2 THEN
                        --Read Status Register 2
                        SOOut_zd <= Status_reg2(7-read_cnt);
                        read_cnt := read_cnt + 1;
                        IF read_cnt = 8 THEN
                            read_cnt := 0;
                        END IF;
                    ELSIF Instruct = RDCR THEN
                        --Read Configuration Register 1
                        SOOut_zd <= Config_reg1(7-read_cnt);
                        read_cnt := read_cnt + 1;
                        IF read_cnt = 8 THEN
                            read_cnt := 0;
                        END IF;
                    ELSIF Instruct = BRRD THEN
                        --Read Bank Address Register
                        fast_rd <= true;
                        rd      <= false;
                        dual    <= false;
                        ddr     <= false;
                        SOOut_zd <= Bank_Addr_reg(7-read_cnt);
                        read_cnt := read_cnt + 1;
                        IF read_cnt = 8 THEN
                            read_cnt := 0;
                        END IF;
                    --Read Array Operations
                    ELSIF Instruct = READ OR Instruct = RD4 OR
                          Instruct = FSTRD OR Instruct = FSTRD4 OR
                          Instruct = DDRFR OR Instruct = DDRFR4 THEN
                        --Read Memory array
                        IF Instruct = READ OR Instruct = RD4 THEN
                            fast_rd <= false;
                            rd      <= true;
                            dual    <= false;
                            ddr     <= false;
                        ELSIF Instruct = DDRFR OR Instruct = DDRFR4 THEN
                            fast_rd <= false;
                            rd      <= false;
                            dual    <= false;
                            ddr     <= true;
                        ELSE
                            fast_rd <= true;
                            rd      <= false;
                            dual    <= false;
                            ddr     <= false;
                        END IF;
                        IF bus_cycle_state = DUMMY_BYTES
                        OR DummyBytes_act = '1' THEN
                            DummyBytes_act := '0';
                            IF (Instruct=DDRFR OR Instruct=DDRFR4) THEN
                                -- Data Learning Pattern (DLP) is
                                -- enabled, Optional DLP
                                data_out:= VDLR_reg;
                                SOOut_zd<=data_out(7-read_cnt);
                                read_cnt:=read_cnt + 1;
                                IF read_cnt = 8 THEN
                                    read_cnt := 0;
                                END IF;
                            END IF;
                        ELSE
                            IF pgm_page /= read_addr/PageSize THEN
                                data_out := to_slv(Mem(read_addr),8);
                                SOOut_zd <= data_out(7-read_cnt);
                                read_cnt := read_cnt + 1;
                                IF read_cnt = 8 THEN
                                    read_cnt := 0;
                                    IF read_addr = AddrRANGE THEN
                                        read_addr := 0;
                                    ELSE
                                        read_addr := read_addr + 1;
                                    END IF;
                                END IF;
                            ELSE
                                SOOut_zd <= 'U';
                                read_cnt := read_cnt + 1;
                                IF read_cnt = 8 THEN
                                    read_cnt := 0;
                                    IF read_addr = AddrRANGE THEN
                                        read_addr := 0;
                                    ELSE
                                        read_addr := read_addr + 1;
                                    END IF;
                                END IF;
                            END IF;
                        END IF;
                    ELSIF Instruct =DOR OR Instruct=DOR4 OR
                          Instruct = DIOR OR Instruct = DIOR4 OR
                          Instruct = DDRDIOR OR Instruct = DDRDIOR4 THEN
                        --Read Memory array
                        IF Instruct = DDRDIOR OR Instruct = DDRDIOR4 THEN
                            fast_rd <= false;
                            rd      <= false;
                            dual    <= true;
                            ddr     <= true;
                        ELSE
                            fast_rd <= false;
                            rd      <= false;
                            dual    <= true;
                            ddr     <= false;
                        END IF;
                        IF bus_cycle_state = DUMMY_BYTES
                        OR DummyBytes_act = '1' THEN
                            DummyBytes_act := '0';
                            data_out:=VDLR_reg;
                            SOOut_zd<=data_out(7-read_cnt);
                            SIOut_zd<=data_out(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            IF  pgm_page /= read_addr/PageSize THEN
                                data_out := to_slv(Mem(read_addr),8);
                                SOOut_zd <= data_out(7-2*read_cnt);
                                SIOut_zd <= data_out(6-2*read_cnt);
                                read_cnt := read_cnt + 1;
                                IF read_cnt = 4 THEN
                                    read_cnt := 0;
                                    IF read_addr = AddrRANGE THEN
                                        read_addr := 0;
                                    ELSE
                                        read_addr := read_addr + 1;
                                    END IF;
                                END IF;
                            ELSE
                                SOOut_zd <= 'U';
                                SIOut_zd <= 'U';
                                read_cnt := read_cnt + 1;
                                IF read_cnt = 4 THEN
                                    read_cnt := 0;
                                    IF read_addr = AddrRANGE THEN
                                        read_addr := 0;
                                    ELSE
                                        read_addr := read_addr + 1;
                                    END IF;
                                END IF;
                            END IF;
                        END IF;
                    ELSIF (Instruct = QOR OR Instruct=QOR4 OR
                           Instruct = QIOR OR Instruct= QIOR4 OR
                           Instruct = DDRQIOR OR Instruct= DDRQIOR4)
                           AND QUAD = '1' THEN
                        --Read Memory array
                        IF Instruct = DDRQIOR OR Instruct = DDRQIOR4 THEN
                            fast_rd <= false;
                            rd      <= false;
                            dual    <= true;
                            ddr     <= true;
                        ELSE
                            fast_rd <= false;
                            rd      <= false;
                            dual    <= true;
                            ddr     <= false;
                        END IF;
                        IF bus_cycle_state = DUMMY_BYTES
                        OR DummyBytes_act = '1' THEN
                            DummyBytes_act := '0';
                            data_out := VDLR_reg;
                            HOLDNegOut_zd <= data_out(7-read_cnt);
                            WPNegOut_zd   <= data_out(7-read_cnt);
                            SOOut_zd      <= data_out(7-read_cnt);
                            SIOut_zd      <= data_out(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            IF  pgm_page /= read_addr/PageSize  THEN
                                data_out := to_slv(Mem(read_addr),8);
                                HOLDNegOut_zd   <= data_out(7-4*read_cnt);
                                WPNegOut_zd     <= data_out(6-4*read_cnt);
                                SOOut_zd        <= data_out(5-4*read_cnt);
                                SIOut_zd        <= data_out(4-4*read_cnt);
                                read_cnt := read_cnt + 1;
                                IF read_cnt = 2 THEN
                                    read_cnt := 0;
                                    IF read_addr = AddrRANGE THEN
                                        read_addr := 0;
                                    ELSE
                                        read_addr := read_addr + 1;
                                    END IF;
                                END IF;
                            ELSE
                                HOLDNegOut_zd   <= 'U';
                                WPNegOut_zd     <= 'U';
                                SOOut_zd        <= 'U';
                                SIOut_zd        <= 'U';
                                read_cnt := read_cnt + 1;
                                IF read_cnt = 2 THEN
                                    read_cnt := 0;
                                    IF read_addr = AddrRANGE THEN
                                        read_addr := 0;
                                    ELSE
                                        read_addr := read_addr + 1;
                                    END IF;
                                END IF;
                            END IF;
                        END IF;
                    END IF;
                ELSIF oe_z THEN
                    IF Instruct = READ OR Instruct = RD4 THEN
                        fast_rd <= false;
                        rd      <= true;
                        dual    <= false;
                        ddr     <= false;
                    ELSIF Instruct = DDRFR OR Instruct = DDRFR4 THEN
                        fast_rd <= false;
                        rd      <= false;
                        dual    <= false;
                        ddr     <= true;
                    ELSIF Instruct = DDRDIOR OR Instruct = DDRDIOR4 THEN
                        fast_rd <= false;
                        rd      <= false;
                        dual    <= true;
                        ddr     <= true;
                    ELSIF Instruct =DOR OR Instruct=DOR4 OR
                          Instruct = DIOR OR Instruct = DIOR4 OR
                          Instruct = QOR OR Instruct=QOR4 OR
                          Instruct = QIOR OR Instruct= QIOR4 THEN
                        fast_rd <= false;
                        rd      <= false;
                        dual    <= true;
                        ddr     <= false;
                    ELSIF Instruct = DDRQIOR OR Instruct = DDRQIOR4 THEN
                        fast_rd <= false;
                        rd      <= false;
                        dual    <= true;
                        ddr     <= true;
                    ELSE
                        fast_rd <= true;
                        rd      <= false;
                        dual    <= false;
                        ddr     <= false;
                    END IF;
                    HOLDNegOut_zd <= 'Z';
                    WPNegOut_zd   <= 'Z';
                    SOOut_zd      <= 'Z';
                    SIOut_zd      <= 'Z';
                ELSIF falling_edge(write) THEN
                    IF Instruct = BRWR THEN
                        Bank_Addr_reg(7) := Bank_Addr_reg_in(7);
                        Bank_Addr_reg(1) := Bank_Addr_reg_in(1);
                        Bank_Addr_reg(0) := Bank_Addr_reg_in(0);
                    ELSIF Instruct = WRR AND BAR_ACC = '1' THEN
                    -- Write to the lower address bits of the BAR
                        IF (P_ERR = '0' AND E_ERR = '0')THEN
                            Bank_Addr_reg(1) := Bank_Addr_reg_in(1);
                            Bank_Addr_reg(0) := Bank_Addr_reg_in(0);
                        END IF;
                    ELSIF Instruct = PGRS THEN
                        PGRES <= '1', '0' AFTER 5 ns;
                        PS := '0';
                        WIP := '1';
                        RES_TO_SUSP_MIN_TIME <= '1', '0' AFTER 60 ns;
                        RES_TO_SUSP_TYP_TIME <= '1', '0' AFTER 100 us;
                    END IF;

                    IF Instruct = BRAC AND P_ERR = '0' AND E_ERR = '0' AND
                       RdPswdProtMode = '0' THEN
                        BAR_ACC <= '1';
                    ELSE
                        BAR_ACC <= '0';
                    END IF;
                END IF;

            WHEN ERS_SUSP_PG_SUSP      =>
                fast_rd <= true;
                rd      <= false;
                dual    <= false;
                ddr     <= false;
                IF PRGSUSP_out = '1' AND PRGSUSP_in = '1' THEN
                    PRGSUSP_in <= '0';
                    --The RDY/BSY bit in the Status Register will indicate that
                    --the device is ready for another operation.
                    WIP := '0';
                    --The Program Suspend (PS) bit in the Status Register will
                    --be set to the logical “1” state to indicate that the
                    --program operation has been suspended.
                    PS := '1';
                END IF;

                IF oe THEN
                    any_read <= true;
                    IF Instruct = RDSR THEN
                        --Read Status Register 1
                        SOOut_zd <= Status_reg1(7-read_cnt);
                        read_cnt := read_cnt + 1;
                        IF read_cnt = 8 THEN
                            read_cnt := 0;
                        END IF;
                    ELSIF Instruct = RDSR2 THEN
                        --Read Status Register 2
                        SOOut_zd <= Status_reg2(7-read_cnt);
                        read_cnt := read_cnt + 1;
                        IF read_cnt = 8 THEN
                            read_cnt := 0;
                        END IF;
                    ELSIF Instruct = RDCR THEN
                        --Read Configuration Register 1
                        SOOut_zd <= Config_reg1(7-read_cnt);
                        read_cnt := read_cnt + 1;
                        IF read_cnt = 8 THEN
                            read_cnt := 0;
                        END IF;
                    ELSIF Instruct = BRRD THEN
                        --Read Bank Address Register
                        fast_rd <= true;
                        rd      <= false;
                        dual    <= false;
                        ddr     <= false;
                        SOOut_zd <= Bank_Addr_reg(7-read_cnt);
                        read_cnt := read_cnt + 1;
                        IF read_cnt = 8 THEN
                            read_cnt := 0;
                        END IF;
                    --Read Array Operations
                    ELSIF Instruct = READ OR Instruct = RD4 OR
                          Instruct = FSTRD OR Instruct = FSTRD4 OR
                          Instruct = DDRFR OR Instruct = DDRFR4 THEN
                        --Read Memory array
                        IF Instruct = READ OR Instruct = RD4 THEN
                            fast_rd <= false;
                            rd      <= true;
                            dual    <= false;
                            ddr     <= false;
                        ELSIF Instruct = DDRFR OR Instruct = DDRFR4 THEN
                            fast_rd <= false;
                            rd      <= false;
                            dual    <= false;
                            ddr     <= true;
                        ELSE
                            fast_rd <= true;
                            rd      <= false;
                            dual    <= false;
                            ddr     <= false;
                        END IF;
                        IF bus_cycle_state = DUMMY_BYTES
                        OR DummyBytes_act = '1' THEN
                            DummyBytes_act := '0';
                            IF (Instruct=DDRFR OR Instruct=DDRFR4) THEN
                                data_out:= VDLR_reg;
                                SOOut_zd<=data_out(7-read_cnt);
                                read_cnt:=read_cnt + 1;
                                IF read_cnt = 8 THEN
                                    read_cnt := 0;
                                END IF;
                            END IF;
                        ELSE
                            IF (SectorSuspend /= read_addr/(SecSize+1) AND
                            pgm_page /= read_addr/PageSize) THEN
                                data_out := to_slv(Mem(read_addr),8);
                                SOOut_zd <= data_out(7-read_cnt);
                                read_cnt := read_cnt + 1;
                                IF read_cnt = 8 THEN
                                    read_cnt := 0;
                                    IF read_addr = AddrRANGE THEN
                                        read_addr := 0;
                                    ELSE
                                        read_addr := read_addr + 1;
                                    END IF;
                                END IF;
                            ELSE
                                SOOut_zd <= 'U';
                                read_cnt := read_cnt + 1;
                                IF read_cnt = 8 THEN
                                    read_cnt := 0;
                                    IF read_addr = AddrRANGE THEN
                                        read_addr := 0;
                                    ELSE
                                        read_addr := read_addr + 1;
                                    END IF;
                                END IF;
                            END IF;
                        END IF;
                    ELSIF Instruct =DOR OR Instruct=DOR4 OR
                          Instruct = DIOR OR Instruct = DIOR4 OR
                          Instruct = DDRDIOR OR Instruct = DDRDIOR4 THEN
                        --Read Memory array

                        IF Instruct = DDRDIOR OR Instruct = DDRDIOR4 THEN
                            fast_rd <= false;
                            rd      <= false;
                            dual    <= true;
                            ddr     <= true;
                        ELSE
                            fast_rd <= false;
                            rd      <= false;
                            dual    <= true;
                            ddr     <= false;
                        END IF;
                        IF bus_cycle_state = DUMMY_BYTES
                        OR DummyBytes_act = '1' THEN
                            DummyBytes_act := '0';
                            IF (Instruct=DDRDIOR OR Instruct=DDRDIOR4) THEN
                                data_out:=VDLR_reg;
                                SOOut_zd<=data_out(7-read_cnt);
                                SIOut_zd<=data_out(7-read_cnt);
                                read_cnt := read_cnt + 1;
                                IF read_cnt = 8 THEN
                                    read_cnt := 0;
                                END IF;
                            END IF;
                        ELSE
                            IF (SectorSuspend /= read_addr/(SecSize+1) AND
                            pgm_page /= read_addr/PageSize) THEN
                                data_out := to_slv(Mem(read_addr),8);
                                SOOut_zd <= data_out(7-2*read_cnt);
                                SIOut_zd <= data_out(6-2*read_cnt);
                                read_cnt := read_cnt + 1;
                                IF read_cnt = 4 THEN
                                    read_cnt := 0;
                                    IF read_addr = AddrRANGE THEN
                                        read_addr := 0;
                                    ELSE
                                        read_addr := read_addr + 1;
                                    END IF;
                                END IF;
                            ELSE
                                SOOut_zd <= 'U';
                                SIOut_zd <= 'U';
                                read_cnt := read_cnt + 1;
                                IF read_cnt = 4 THEN
                                    read_cnt := 0;
                                    IF read_addr = AddrRANGE THEN
                                        read_addr := 0;
                                    ELSE
                                        read_addr := read_addr + 1;
                                    END IF;
                                END IF;
                            END IF;
                        END IF;
                    ELSIF (Instruct = QOR OR Instruct=QOR4 OR
                           Instruct = QIOR OR Instruct= QIOR4 OR
                           Instruct = DDRQIOR OR Instruct= DDRQIOR4)
                           AND QUAD = '1' THEN
                        --Read Memory array
                        IF Instruct = DDRQIOR OR Instruct = DDRQIOR4 THEN
                            fast_rd <= false;
                            rd      <= false;
                            dual    <= true;
                            ddr     <= true;
                        ELSE
                            fast_rd <= false;
                            rd      <= false;
                            dual    <= true;
                            ddr     <= false;
                        END IF;
                        IF bus_cycle_state = DUMMY_BYTES
                        OR DummyBytes_act = '1' THEN
                            DummyBytes_act := '0';
                            data_out := VDLR_reg;
                            HOLDNegOut_zd <= data_out(7-read_cnt);
                            WPNegOut_zd   <= data_out(7-read_cnt);
                            SOOut_zd      <= data_out(7-read_cnt);
                            SIOut_zd      <= data_out(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            IF (SectorSuspend /= read_addr/(SecSize+1) AND
                            pgm_page /= read_addr/PageSize) THEN
                                data_out := to_slv(Mem(read_addr),8);
                                HOLDNegOut_zd   <= data_out(7-4*read_cnt);
                                WPNegOut_zd     <= data_out(6-4*read_cnt);
                                SOOut_zd        <= data_out(5-4*read_cnt);
                                SIOut_zd        <= data_out(4-4*read_cnt);
                                read_cnt := read_cnt + 1;
                                IF read_cnt = 2 THEN
                                    read_cnt := 0;
                                    IF read_addr = AddrRANGE THEN
                                        read_addr := 0;
                                    ELSE
                                        read_addr := read_addr + 1;
                                    END IF;
                                END IF;
                            ELSE
                                HOLDNegOut_zd   <= 'U';
                                WPNegOut_zd     <= 'U';
                                SOOut_zd        <= 'U';
                                SIOut_zd        <= 'U';
                                read_cnt := read_cnt + 1;
                                IF read_cnt = 2 THEN
                                    read_cnt := 0;
                                    IF read_addr = AddrRANGE THEN
                                        read_addr := 0;
                                    ELSE
                                        read_addr := read_addr + 1;
                                    END IF;
                                END IF;
                            END IF;
                        END IF;
                    END IF;
                ELSIF oe_z THEN
                    IF Instruct = READ OR Instruct = RD4 THEN
                        fast_rd <= false;
                        rd      <= true;
                        dual    <= false;
                        ddr     <= false;
                    ELSIF Instruct = DDRFR OR Instruct = DDRFR4 THEN
                        fast_rd <= false;
                        rd      <= false;
                        dual    <= false;
                        ddr     <= true;
                    ELSIF Instruct = DDRDIOR OR Instruct = DDRDIOR4 THEN
                        fast_rd <= false;
                        rd      <= false;
                        dual    <= true;
                        ddr     <= true;
                    ELSIF Instruct =DOR OR Instruct=DOR4 OR
                          Instruct = DIOR OR Instruct = DIOR4 OR
                          Instruct = QOR OR Instruct=QOR4 OR
                          Instruct = QIOR OR Instruct= QIOR4 THEN
                        fast_rd <= false;
                        rd      <= false;
                        dual    <= true;
                        ddr     <= false;
                    ELSIF Instruct = DDRQIOR OR Instruct = DDRQIOR4 THEN
                        fast_rd <= false;
                        rd      <= false;
                        dual    <= true;
                        ddr     <= true;
                    ELSE
                        fast_rd <= true;
                        rd      <= false;
                        dual    <= false;
                        ddr     <= false;
                    END IF;
                    HOLDNegOut_zd <= 'Z';
                    WPNegOut_zd   <= 'Z';
                    SOOut_zd      <= 'Z';
                    SIOut_zd      <= 'Z';
                ELSIF falling_edge(write) THEN
                    IF Instruct = BRWR THEN
                        Bank_Addr_reg(7) := Bank_Addr_reg_in(7);
                        Bank_Addr_reg(1) := Bank_Addr_reg_in(1);
                        Bank_Addr_reg(0) := Bank_Addr_reg_in(0);
                    ELSIF Instruct = WRR AND BAR_ACC = '1' THEN
                    -- Write to the lower address bits of the BAR
                        IF (P_ERR = '0' AND E_ERR = '0')THEN
                            Bank_Addr_reg(1) := Bank_Addr_reg_in(1);
                            Bank_Addr_reg(0) := Bank_Addr_reg_in(0);
                        END IF;
                    ELSIF Instruct = PGRS THEN
                        PGRES <= '1', '0' AFTER 5 ns;
                        PS := '0';
                        WIP := '1';
                        RES_TO_SUSP_MIN_TIME <= '1', '0' AFTER 60 ns;
                        RES_TO_SUSP_TYP_TIME <= '1', '0' AFTER 100 us;
                    END IF;

                    IF Instruct = BRAC AND P_ERR = '0' AND E_ERR = '0' AND
                    RdPswdProtMode = '0' THEN
                        BAR_ACC <= '1';
                    ELSE
                        BAR_ACC <= '0';
                    END IF;
                END IF;

            WHEN OTP_PG         =>
                fast_rd <= true;
                rd      <= false;
                dual    <= false;
                ddr     <= false;
                IF oe THEN
                    any_read <= true;
                    IF Instruct = RDSR THEN
                        --Read Status Register 1
                        SOOut_zd <= Status_reg1(7-read_cnt);
                        read_cnt := read_cnt + 1;
                        IF read_cnt = 8 THEN
                            read_cnt := 0;
                        END IF;
                    ELSIF Instruct = RDSR2 THEN
                        --Read Status Register 2
                        SOOut_zd <= Status_reg2(7-read_cnt);
                        read_cnt := read_cnt + 1;
                        IF read_cnt = 8 THEN
                            read_cnt := 0;
                        END IF;
                    ELSIF Instruct = RDCR THEN
                        --Read Configuration Register 1
                        SOOut_zd <= Config_reg1(7-read_cnt);
                        read_cnt := read_cnt + 1;
                        IF read_cnt = 8 THEN
                            read_cnt := 0;
                        END IF;
                    END IF;
                ELSIF oe_z THEN
                    HOLDNegOut_zd <= 'Z';
                    WPNegOut_zd   <= 'Z';
                    SOOut_zd      <= 'Z';
                    SIOut_zd      <= 'Z';
                END IF;

                IF Address + wr_cnt <= OTPHiAddr THEN
                    FOR i IN 0 TO wr_cnt LOOP
                        new_int := WData(i);
                        old_int := OTPMem(Addr + i);
                        IF new_int > -1 THEN
                            new_bit := to_slv(new_int,8);
                            IF old_int > -1 THEN
                                old_bit := to_slv(old_int,8);
                                FOR j IN 0 TO 7 LOOP
                                    IF old_bit(j) = '0' THEN
                                        new_bit(j) := '0';
                                    END IF;
                                END LOOP;
                                new_int := to_nat(new_bit);
                            END IF;
                            WData(i) := new_int;
                        ELSE
                            WData(i) := -1;
                        END IF;

                        OTPMem(Addr + i) :=  -1;
                    END LOOP;
                ELSE
                    ASSERT false
                        REPORT "Programming will reach over address limit"&
                        " of OTP array"
                        SEVERITY warning;
                END IF;

                IF PDONE = '1' THEN
                    WIP := '0';
                    WEL := '0';
                    FOR i IN 0 TO wr_cnt LOOP
                        OTPMem(Addr_tmp + i) := WData(i);
                    END LOOP;
                    LOCK_BYTE1 := to_slv(OTPMem(16#10#),8);
                    LOCK_BYTE2 := to_slv(OTPMem(16#11#),8);
                    LOCK_BYTE3 := to_slv(OTPMem(16#12#),8);
                    LOCK_BYTE4 := to_slv(OTPMem(16#13#),8);
                END IF;

            WHEN SECTOR_ERS     =>
                fast_rd <= true;
                rd      <= false;
                dual    <= false;
                ddr     <= false;
                IF oe THEN
                    any_read <= true;
                    IF Instruct = RDSR THEN
                        --Read Status Register 1
                        SOOut_zd <= Status_reg1(7-read_cnt);
                        read_cnt := read_cnt + 1;
                        IF read_cnt = 8 THEN
                            read_cnt := 0;
                        END IF;
                    ELSIF Instruct = RDSR2 THEN
                        --Read Status Register 2
                        SOOut_zd <= Status_reg2(7-read_cnt);
                        read_cnt := read_cnt + 1;
                        IF read_cnt = 8 THEN
                            read_cnt := 0;
                        END IF;
                    ELSIF Instruct = RDCR THEN
                        --Read Configuration Register 1
                        SOOut_zd <= Config_reg1(7-read_cnt);
                        read_cnt := read_cnt + 1;
                        IF read_cnt = 8 THEN
                            read_cnt := 0;
                        END IF;
                    END IF;
                ELSIF oe_z THEN
                    HOLDNegOut_zd <= 'Z';
                    WPNegOut_zd   <= 'Z';
                    SOOut_zd      <= 'Z';
                    SIOut_zd      <= 'Z';
                END IF;

                ADDRHILO_SEC(AddrLo, AddrHi, Addr);
                FOR i IN AddrLo TO AddrHi LOOP
                    Mem(i) := -1;
                END LOOP;
                IF EDONE = '1' THEN
                    WIP := '0';
                    WEL := '0';
                    FOR i IN AddrLo TO AddrHi LOOP
                        Mem(i) :=  MaxData;

                        pgm_page := i/PageSize;
--                        QPP_page(pgm_page) := '0';
                    END LOOP;

                ELSIF Instruct = ERSP AND ERSSUSP_in = '0' THEN
                    ESUSP <= '1', '0' AFTER 10 ns;
                    ERSSUSP_in <= '1';
                    IF LongTimming = TRUE THEN
                            ERSSUSP_out <= '0', '1' AFTER (tdevice_ERSSUSP);
                         ELSE
                            ERSSUSP_out <= '0', '1' AFTER (tdevice_ERSSUSP/100);
                         END IF;
                    ASSERT RES_TO_SUSP_TYP_TIME = '0'
                    REPORT "Typical periods are needed for " &
                           "Program to progress to completion"
                    SEVERITY warning;
                END IF;

            WHEN BULK_ERS       =>
                fast_rd <= true;
                rd      <= false;
                dual    <= false;
                ddr     <= false;
                IF oe THEN
                    any_read <= true;
                    IF Instruct = RDSR THEN
                        --Read Status Register 1
                        SOOut_zd <= Status_reg1(7-read_cnt);
                        read_cnt := read_cnt + 1;
                        IF read_cnt = 8 THEN
                            read_cnt := 0;
                        END IF;
                    ELSIF Instruct = RDSR2 THEN
                        --Read Status Register 2
                        SOOut_zd <= Status_reg2(7-read_cnt);
                        read_cnt := read_cnt + 1;
                        IF read_cnt = 8 THEN
                            read_cnt := 0;
                        END IF;
                    ELSIF Instruct = RDCR THEN
                        --Read Configuration Register 1
                        SOOut_zd <= Config_reg1(7-read_cnt);
                        read_cnt := read_cnt + 1;
                        IF read_cnt = 8 THEN
                            read_cnt := 0;
                        END IF;
                    END IF;
                ELSIF oe_z THEN
                    HOLDNegOut_zd <= 'Z';
                    WPNegOut_zd   <= 'Z';
                    SOOut_zd      <= 'Z';
                    SIOut_zd      <= 'Z';
                END IF;

                IF falling_edge(EDONE) THEN
                    FOR i IN 0 TO AddrRANGE LOOP
                        sect := i/(SecSize+1);
                        IF PPB_bits(sect) = '1' AND DYB_bits(sect) = '1' THEN
                            Mem(i) := -1;
                        END IF;
                    END LOOP;
                END IF;

                IF EDONE = '1' THEN
                    WIP := '0';
                    WEL := '0';
                    FOR i IN 0 TO AddrRANGE LOOP
                        sect := i/(SecSize+1);
                        IF PPB_bits(sect) = '1' AND DYB_bits(sect) = '1' THEN
                            Mem(i) :=  MaxData;

                            pgm_page := i/PageSize;
--                            QPP_page(pgm_page) := '0';
                        END IF;
                    END LOOP;
                END IF;

            WHEN ERS_SUSP       =>
                fast_rd <= true;
                rd      <= false;
                dual    <= false;
                ddr     <= false;
                IF ERSSUSP_out = '1' THEN
                    ERSSUSP_in <= '0';
                    --The Erase Suspend (ES) bit in the Status Register will
                    --be set to the logical “1” state to indicate that the
                    --erase operation has been suspended.
                    ES := '1';
                    --The WIP bit in the Status Register will indicate that
                    --the device is ready for another operation.
                    WIP := '0';
                END IF;

                IF oe THEN
                    any_read <= true;
                    IF Instruct = RDSR THEN
                        --Read Status Register 1
                        SOOut_zd <= Status_reg1(7-read_cnt);
                        read_cnt := read_cnt + 1;
                        IF read_cnt = 8 THEN
                            read_cnt := 0;
                        END IF;
                    ELSIF Instruct = RDSR2 THEN
                        --Read Status Register 2
                        SOOut_zd <= Status_reg2(7-read_cnt);
                        read_cnt := read_cnt + 1;
                        IF read_cnt = 8 THEN
                            read_cnt := 0;
                        END IF;
                    ELSIF Instruct = RDCR THEN
                        --Read Configuration Register 1
                        SOOut_zd <= Config_reg1(7-read_cnt);
                        read_cnt := read_cnt + 1;
                        IF read_cnt = 8 THEN
                            read_cnt := 0;
                        END IF;
                    ELSIF Instruct = DYBRD THEN
                        --Read DYB Access Register
                        sect := Address/(SecSize+1);
                        fast_rd <= true;
                        rd      <= false;
                        dual    <= false;
                        ddr     <= false;
                        IF DYB_bits(sect) = '1' THEN
                            DYBAR(7 downto 0) := "11111111";
                        ELSE
                            DYBAR(7 downto 0) := "00000000";
                        END IF;
                        SOOut_zd <= DYBAR(7-read_cnt);
                        read_cnt := read_cnt + 1;
                        IF read_cnt = 8 THEN
                            read_cnt := 0;
                        END IF;
                    ELSIF Instruct = BRRD THEN
                        --Read Bank Address Register
                        fast_rd <= true;
                        rd      <= false;
                        dual    <= false;
                        ddr     <= false;
                        SOOut_zd <= Bank_Addr_reg(7-read_cnt);
                        read_cnt := read_cnt + 1;
                        IF read_cnt = 8 THEN
                            read_cnt := 0;
                        END IF;
                    ELSIF Instruct = PPBRD THEN
                        --Read PPB Access Register
                        sect := Address/(SecSize+1);
                        fast_rd <= true;
                        rd      <= false;
                        dual    <= false;
                        ddr     <= false;
                        PPBAR(7 downto 0) := "UUUUUUUU";
                        IF RdPswdProtMode = '0' THEN
                            IF PPB_bits(sect) = '1' THEN
                                PPBAR(7 downto 0) := "11111111";
                            ELSE
                                PPBAR(7 downto 0) := "00000000";
                            END IF;
                        END IF;
                        SOOut_zd <= PPBAR(7-read_cnt);
                        read_cnt := read_cnt + 1;
                        IF read_cnt = 8 THEN
                            read_cnt := 0;
                        END IF;
                    --Read Array Operations
                    ELSIF Instruct = READ OR Instruct = RD4 OR
                          Instruct = FSTRD OR Instruct = FSTRD4 OR
                          Instruct = DDRFR OR Instruct = DDRFR4 THEN
                        --Read Memory array
                        IF Instruct = READ OR Instruct = RD4 THEN
                            fast_rd <= false;
                            rd      <= true;
                            dual    <= false;
                            ddr     <= false;
                        ELSIF Instruct = DDRFR OR Instruct = DDRFR4 THEN
                            fast_rd <= false;
                            rd      <= false;
                            dual    <= false;
                            ddr     <= true;
                        ELSE
                            fast_rd <= true;
                            rd      <= false;
                            dual    <= false;
                            ddr     <= false;
                        END IF;
                        IF bus_cycle_state = DUMMY_BYTES
                        OR DummyBytes_act = '1' THEN
                            DummyBytes_act := '0';
                            IF (Instruct=DDRFR OR Instruct=DDRFR4) THEN
                                data_out:= VDLR_reg;
                                SOOut_zd<=data_out(7-read_cnt);
                                read_cnt:=read_cnt + 1;
                                IF read_cnt = 8 THEN
                                    read_cnt := 0;
                                END IF;
                            END IF;
                        ELSE
                            IF SectorSuspend /= read_addr/(SecSize+1) THEN
                                data_out := to_slv(Mem(read_addr),8);
                                SOOut_zd <= data_out(7-read_cnt);
                                read_cnt := read_cnt + 1;
                                IF read_cnt = 8 THEN
                                    read_cnt := 0;
                                    IF read_addr = AddrRANGE THEN
                                        read_addr := 0;
                                    ELSE
                                        read_addr := read_addr + 1;
                                    END IF;
                                END IF;
                            ELSE
                                SOOut_zd <= 'U';
                                read_cnt := read_cnt + 1;
                                IF read_cnt = 8 THEN
                                    read_cnt := 0;
                                    IF read_addr = AddrRANGE THEN
                                        read_addr := 0;
                                    ELSE
                                        read_addr := read_addr + 1;
                                    END IF;
                                END IF;
                            END IF;
                        END IF;
                    ELSIF Instruct =DOR OR Instruct=DOR4 OR
                          Instruct = DIOR OR Instruct = DIOR4 OR
                          Instruct = DDRDIOR OR Instruct = DDRDIOR4 THEN
                        --Read Memory array
                        IF Instruct = DDRDIOR OR Instruct = DDRDIOR4 THEN
                            fast_rd <= false;
                            rd      <= false;
                            dual    <= true;
                            ddr     <= true;
                        ELSE
                            fast_rd <= false;
                            rd      <= false;
                            dual    <= true;
                            ddr     <= false;
                        END IF;
                        IF bus_cycle_state = DUMMY_BYTES
                        OR DummyBytes_act = '1' THEN
                            DummyBytes_act := '0';
                            data_out:=VDLR_reg;
                            SOOut_zd<=data_out(7-read_cnt);
                            SIOut_zd<=data_out(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            IF SectorSuspend /= read_addr/(SecSize+1) THEN
                                data_out := to_slv(Mem(read_addr),8);
                                SOOut_zd <= data_out(7-2*read_cnt);
                                SIOut_zd <= data_out(6-2*read_cnt);
                                read_cnt := read_cnt + 1;
                                IF read_cnt = 4 THEN
                                    read_cnt := 0;
                                    IF read_addr = AddrRANGE THEN
                                        read_addr := 0;
                                    ELSE
                                        read_addr := read_addr + 1;
                                    END IF;
                                END IF;
                            ELSE
                                SOOut_zd <= 'U';
                                SIOut_zd <= 'U';
                                read_cnt := read_cnt + 1;
                                IF read_cnt = 4 THEN
                                    read_cnt := 0;
                                    IF read_addr = AddrRANGE THEN
                                        read_addr := 0;
                                    ELSE
                                        read_addr := read_addr + 1;
                                    END IF;
                                END IF;
                            END IF;
                        END IF;
                    ELSIF (Instruct = QOR OR Instruct=QOR4 OR
                           Instruct = QIOR OR Instruct= QIOR4 OR
                           Instruct = DDRQIOR OR Instruct= DDRQIOR4)
                           AND QUAD = '1' THEN
                        --Read Memory array
                        IF Instruct = DDRQIOR OR Instruct = DDRQIOR4 THEN
                            fast_rd <= false;
                            rd      <= false;
                            dual    <= true;
                            ddr     <= true;
                        ELSE
                            fast_rd <= false;
                            rd      <= false;
                            dual    <= true;
                            ddr     <= false;
                        END IF;
                        IF bus_cycle_state = DUMMY_BYTES
                        OR DummyBytes_act = '1' THEN
                            DummyBytes_act := '0';
                            data_out := VDLR_reg;
                            HOLDNegOut_zd <= data_out(7-read_cnt);
                            WPNegOut_zd   <= data_out(7-read_cnt);
                            SOOut_zd      <= data_out(7-read_cnt);
                            SIOut_zd      <= data_out(7-read_cnt);
                            read_cnt := read_cnt + 1;
                            IF read_cnt = 8 THEN
                                read_cnt := 0;
                            END IF;
                        ELSE
                            IF SectorSuspend /= read_addr/(SecSize+1) THEN
                                data_out := to_slv(Mem(read_addr),8);
                                HOLDNegOut_zd   <= data_out(7-4*read_cnt);
                                WPNegOut_zd     <= data_out(6-4*read_cnt);
                                SOOut_zd        <= data_out(5-4*read_cnt);
                                SIOut_zd        <= data_out(4-4*read_cnt);
                                read_cnt := read_cnt + 1;
                                IF read_cnt = 2 THEN
                                    read_cnt := 0;
                                    IF read_addr = AddrRANGE THEN
                                        read_addr := 0;
                                    ELSE
                                        read_addr := read_addr + 1;
                                    END IF;
                                END IF;
                            ELSE
                                HOLDNegOut_zd   <= 'U';
                                WPNegOut_zd     <= 'U';
                                SOOut_zd        <= 'U';
                                SIOut_zd        <= 'U';
                                read_cnt := read_cnt + 1;
                                IF read_cnt = 2 THEN
                                    read_cnt := 0;
                                    IF read_addr = AddrRANGE THEN
                                        read_addr := 0;
                                    ELSE
                                        read_addr := read_addr + 1;
                                    END IF;
                                END IF;
                            END IF;
                        END IF;
                    END IF;
                ELSIF oe_z THEN
                    IF Instruct = READ OR Instruct = RD4 THEN
                        fast_rd <= false;
                        rd      <= true;
                        dual    <= false;
                        ddr     <= false;
                    ELSIF Instruct = DDRFR OR Instruct = DDRFR4 THEN
                        fast_rd <= false;
                        rd      <= false;
                        dual    <= false;
                        ddr     <= true;
                    ELSIF Instruct = DDRDIOR OR Instruct = DDRDIOR4 THEN
                        fast_rd <= false;
                        rd      <= false;
                        dual    <= true;
                        ddr     <= true;
                    ELSIF Instruct =DOR OR Instruct=DOR4 OR
                          Instruct = DIOR OR Instruct = DIOR4 OR
                          Instruct = QOR OR Instruct=QOR4 OR
                          Instruct = QIOR OR Instruct= QIOR4 THEN
                        fast_rd <= false;
                        rd      <= false;
                        dual    <= true;
                        ddr     <= false;
                    ELSIF Instruct = DDRQIOR OR Instruct = DDRQIOR4 THEN
                        fast_rd <= false;
                        rd      <= false;
                        dual    <= true;
                        ddr     <= true;
                    ELSE
                        fast_rd <= true;
                        rd      <= false;
                        dual    <= false;
                        ddr     <= false;
                    END IF;
                    HOLDNegOut_zd <= 'Z';
                    WPNegOut_zd   <= 'Z';
                    SOOut_zd      <= 'Z';
                    SIOut_zd      <= 'Z';
                END IF;

                IF falling_edge(write) THEN
                    IF (Instruct = PP OR Instruct = PP4) AND WEL = '1' THEN
                        sect := Address/(SecSize+1);
                        pgm_page := Address/PageSize;
                        IF SectorSuspend /= sect THEN
                            IF (Sec_Prot(sect) = '0' AND
                               PPB_bits(sect)='1' AND DYB_bits(sect)='1') THEN
                                PSTART <= '1', '0' AFTER 5 ns;
                                PGSUSP  <= '0';
                                PGRES   <= '0';
                                WIP := '1' ;
                                SA <= sect;
                                Addr := Address;
                                Addr_tmp := Address;
                                wr_cnt := Byte_number;
                                FOR I IN wr_cnt DOWNTO 0 LOOP
                                    IF Viol /= '0' THEN
                                        WData(i) := -1;
                                    ELSE
                                        WData(i) := WByte(i);
                                    END IF;
                                END LOOP;
                            ELSE
                                WEL   := '0';
                                P_ERR := '1';
                            END IF;
                        ELSE
                            WEL := '0';
                            P_ERR := '1';
                        END IF;
                    ELSIF (Instruct = QPP OR Instruct = QPP4) AND WEL = '1' THEN
                        sect := Address/(SecSize+1);
                        pgm_page := Address/PageSize;
                        IF SectorSuspend /= sect THEN
                            IF (Sec_Prot(sect)='0' AND
                                PPB_bits(sect)='1' AND DYB_bits(sect)='1') THEN
                                PSTART <= '1', '0' AFTER 5 ns;
                                PGSUSP  <= '0';
                                PGRES   <= '0';
                                WIP := '1' ;
                                SA <= sect;
                                Addr := Address;
                                Addr_tmp := Address;
                                wr_cnt := Byte_number;
                                FOR I IN wr_cnt DOWNTO 0 LOOP
                                    IF Viol /= '0' THEN
                                        WData(i) := -1;
                                    ELSE
                                        WData(i) := WByte(i);
                                    END IF;
                                END LOOP;
                            ELSE
                                WEL   := '0';
                                P_ERR := '1';
                            END IF;
                        ELSE
                            WEL := '0';
                            P_ERR := '1';
                        END IF;
                    ELSIF Instruct = WREN THEN
                        WEL := '1';
                    ELSIF Instruct = CLSR THEN
                    --The Clear Status Register Command resets bit SR1[5]
                    --(Erase Fail Flag) and bit SR1[6] (Program Fail Flag)
                       E_ERR := '0';
                       P_ERR := '0';
                    ELSIF Instruct = BRWR THEN
                        Bank_Addr_reg(7) := Bank_Addr_reg_in(7);
                        Bank_Addr_reg(1) := Bank_Addr_reg_in(1);
                        Bank_Addr_reg(0) := Bank_Addr_reg_in(0);
                    ELSIF Instruct = WRR AND BAR_ACC = '1' THEN
                    -- Write to the lower address bits of the BAR
                        IF (P_ERR = '0' AND E_ERR = '0')THEN
                            Bank_Addr_reg(1) := Bank_Addr_reg_in(1);
                            Bank_Addr_reg(0) := Bank_Addr_reg_in(0);
                        END IF;
                    ELSIF Instruct = DYBWR  AND WEL = '1' THEN
                        sect := Address/(SecSize+1);
                        PSTART <= '1', '0' AFTER 5 ns;
                        WIP := '1';
                    ELSIF Instruct = ERRS THEN
                        ERES <= '1', '0' AFTER 5 ns;
                        ES := '0';
                        WIP := '1';
                        Addr := SectorSuspend*(SecSize+1);
                        ADDRHILO_SEC(AddrLo, AddrHi, Addr);
                        RES_TO_SUSP_TYP_TIME <= '1', '0' AFTER 100 us;
                    END IF;

                    IF Instruct = BRAC AND P_ERR = '0' AND E_ERR = '0' AND
                       RdPswdProtMode = '0' THEN
                        BAR_ACC <= '1';
                    ELSE
                        BAR_ACC <= '0';
                    END IF;
                END IF;

            WHEN ERS_SUSP_PG    =>
                fast_rd <= true;
                rd      <= false;
                dual    <= false;
                ddr     <= false;
                IF oe THEN
                    any_read <= true;
                    IF Instruct = RDSR THEN
                        --Read Status Register 1
                        SOOut_zd <= Status_reg1(7-read_cnt);
                        read_cnt := read_cnt + 1;
                        IF read_cnt = 8 THEN
                            read_cnt := 0;
                        END IF;
                    ELSIF Instruct = RDSR2 THEN
                        --Read Status Register 2
                        SOOut_zd <= Status_reg2(7-read_cnt);
                        read_cnt := read_cnt + 1;
                        IF read_cnt = 8 THEN
                            read_cnt := 0;
                        END IF;
                    ELSIF Instruct = RDCR THEN
                        --Read Configuration Register 1
                        SOOut_zd <= Config_reg1(7-read_cnt);
                        read_cnt := read_cnt + 1;
                        IF read_cnt = 8 THEN
                            read_cnt := 0;
                        END IF;
                    END IF;
                ELSIF oe_z THEN
                    HOLDNegOut_zd <= 'Z';
                    WPNegOut_zd   <= 'Z';
                    SOOut_zd      <= 'Z';
                    SIOut_zd      <= 'Z';
                END IF;

--                IF (Instruct = QPP OR Instruct = QPP4) THEN
--                    QPP_page(pgm_page) := '1';
--                END IF;

                ADDRHILO_PG(AddrLo, AddrHi, Addr);
                cnt := 0;

                FOR i IN 0 TO wr_cnt LOOP
                    new_int := WData(i);
                    old_int := Mem(Addr + i - cnt);
                    IF new_int > -1 THEN
                        new_bit := to_slv(new_int,8);
                        IF old_int > -1 THEN
                            old_bit := to_slv(old_int,8);
                            FOR j IN 0 TO 7 LOOP
                                IF old_bit(j) = '0' THEN
                                    new_bit(j) := '0';
                                END IF;
                            END LOOP;
                            new_int := to_nat(new_bit);
                        END IF;
                        WData(i) := new_int;
                    ELSE
                        WData(i) := -1;
                    END IF;

                    IF (Addr + i) = AddrHi THEN
                        Addr := AddrLo;
                        cnt := i + 1;
                    END IF;
                END LOOP;
                cnt :=0;

                IF PDONE = '1' THEN
                    WIP := '0';
                    WEL := '0';
                    FOR i IN 0 TO wr_cnt LOOP
                        Mem(Addr_tmp + i - cnt) := WData(i);
                        IF (Addr_tmp + i) = AddrHi THEN
                            Addr_tmp := AddrLo;
                            cnt := i + 1;
                        END IF;
                    END LOOP;
                ELSIF Instruct = PGSP AND PRGSUSP_in = '0' THEN
                    IF RES_TO_SUSP_MIN_TIME = '0' THEN
                        PGSUSP <= '1', '0' AFTER 1 ns;
                        PRGSUSP_in <= '1';
                        IF LongTimming = TRUE THEN
                            PRGSUSP_out <= '0', '1' AFTER (tdevice_PRGSUSP);
                         ELSE
                            PRGSUSP_out <= '0', '1' AFTER (tdevice_PRGSUSP/100);
                         END IF;
                        ASSERT RES_TO_SUSP_TYP_TIME = '0'
                        REPORT "Typical periods are needed for " &
                               "Program to progress to completion"
                        SEVERITY warning;
                    ELSE
                        ASSERT FALSE
                        REPORT "Minimum for tPRS is not satisfied! " &
                               "PGSP command is ignored"
                        SEVERITY warning;
                    END IF;
                END IF;

            WHEN PASS_PG        =>
                fast_rd <= true;
                rd      <= false;
                dual    <= false;
                ddr     <= false;
                IF oe THEN
                    any_read <= true;
                    IF Instruct = RDSR THEN
                        --Read Status Register 1
                        SOOut_zd <= Status_reg1(7-read_cnt);
                        read_cnt := read_cnt + 1;
                        IF read_cnt = 8 THEN
                            read_cnt := 0;
                        END IF;
                    ELSIF Instruct = RDSR2 THEN
                        --Read Status Register 2
                        SOOut_zd <= Status_reg2(7-read_cnt);
                        read_cnt := read_cnt + 1;
                        IF read_cnt = 8 THEN
                            read_cnt := 0;
                        END IF;
                    ELSIF Instruct = RDCR THEN
                        --Read Configuration Register 1
                        SOOut_zd <= Config_reg1(7-read_cnt);
                        read_cnt := read_cnt + 1;
                        IF read_cnt = 8 THEN
                            read_cnt := 0;
                        END IF;
                    END IF;
                ELSIF oe_z THEN
                    HOLDNegOut_zd <= 'Z';
                    WPNegOut_zd   <= 'Z';
                    SOOut_zd      <= 'Z';
                    SIOut_zd      <= 'Z';
                END IF;

                new_pass := Password_reg_in;
                old_pass := Password_reg;
                FOR j IN 0 TO 63 LOOP
                    IF old_pass(j) = '0' THEN
                        new_pass(j) := '0';
                    END IF;
                END LOOP;

                IF PDONE = '1' THEN
                    Password_reg := new_pass;
                    WIP  := '0';
                    WEL  := '0';
                END IF;

            WHEN PASS_UNLOCK    =>
                WIP := '1';
                fast_rd <= true;
                rd      <= false;
                dual    <= false;
                ddr     <= false;
                IF oe THEN
                    any_read <= true;
                    IF Instruct = RDSR THEN
                        --Read Status Register 1
                        SOOut_zd <= Status_reg1(7-read_cnt);
                        read_cnt := read_cnt + 1;
                        IF read_cnt = 8 THEN
                            read_cnt := 0;
                        END IF;
                    ELSIF Instruct = RDSR2 THEN
                        --Read Status Register 2
                        SOOut_zd <= Status_reg2(7-read_cnt);
                        read_cnt := read_cnt + 1;
                        IF read_cnt = 8 THEN
                            read_cnt := 0;
                        END IF;
                    ELSIF Instruct = RDCR THEN
                        --Read Configuration Register 1
                        SOOut_zd <= Config_reg1(7-read_cnt);
                        read_cnt := read_cnt + 1;
                        IF read_cnt = 8 THEN
                            read_cnt := 0;
                        END IF;
                    END IF;
                ELSIF oe_z THEN
                    HOLDNegOut_zd <= 'Z';
                    WPNegOut_zd   <= 'Z';
                    SOOut_zd      <= 'Z';
                    SIOut_zd      <= 'Z';
                END IF;

                IF PASS_TEMP = Password_reg THEN
                    PASS_UNLOCKED <= TRUE;
                ELSE
                    PASS_UNLOCKED <= FALSE;
                END IF;

                IF PASSULCK_out = '1' THEN
                    IF PASS_UNLOCKED AND PWDMLB = '0' THEN
                        PPB_LOCK := '1';
                        PPB_LOCK_temp <= '1';
                        WIP   := '0';
                    ELSE
                        P_ERR := '1';
                        REPORT "Incorrect Password!"
                        SEVERITY warning;
                        PASSACC_in <= '1';
                        IF LongTimming = TRUE THEN
                             PASSACC_out <= '0', '1' AFTER (tdevice_PASSACC);
                          ELSE
                             PASSACC_out <= '0', '1' AFTER (tdevice_PASSACC/10);
                        END IF;
                    END IF;
                    WEL   := '0';
                    PASSULCK_in <= '0';
                END IF;

            WHEN PPB_PG         =>
                fast_rd <= true;
                rd      <= false;
                dual    <= false;
                ddr     <= false;
                IF oe THEN
                    any_read <= true;
                    IF Instruct = RDSR THEN
                        --Read Status Register 1
                        SOOut_zd <= Status_reg1(7-read_cnt);
                        read_cnt := read_cnt + 1;
                        IF read_cnt = 8 THEN
                            read_cnt := 0;
                        END IF;
                    ELSIF Instruct = RDSR2 THEN
                        --Read Status Register 2
                        SOOut_zd <= Status_reg2(7-read_cnt);
                        read_cnt := read_cnt + 1;
                        IF read_cnt = 8 THEN
                            read_cnt := 0;
                        END IF;
                    ELSIF Instruct = RDCR THEN
                        --Read Configuration Register 1
                        SOOut_zd <= Config_reg1(7-read_cnt);
                        read_cnt := read_cnt + 1;
                        IF read_cnt = 8 THEN
                            read_cnt := 0;
                        END IF;
                    END IF;
                ELSIF oe_z THEN
                    HOLDNegOut_zd <= 'Z';
                    WPNegOut_zd   <= 'Z';
                    SOOut_zd      <= 'Z';
                    SIOut_zd      <= 'Z';
                END IF;

                IF PDONE = '1' THEN
                    IF PPB_LOCK /= '0' THEN
                        PPB_bits(sect):= '0';
                        WIP   := '0';
                        WEL   := '0';
                    ELSE
                        P_ERR := '0';
                        WIP   := '0';
                        WEL   := '0';
                    END IF;
                END IF;

            WHEN PPB_ERS        =>
                fast_rd <= true;
                rd      <= false;
                dual    <= false;
                ddr     <= false;
                IF oe THEN
                    any_read <= true;
                    IF Instruct = RDSR THEN
                        --Read Status Register 1
                        SOOut_zd <= Status_reg1(7-read_cnt);
                        read_cnt := read_cnt + 1;
                        IF read_cnt = 8 THEN
                            read_cnt := 0;
                        END IF;
                    ELSIF Instruct = RDSR2 THEN
                        --Read Status Register 2
                        SOOut_zd <= Status_reg2(7-read_cnt);
                        read_cnt := read_cnt + 1;
                        IF read_cnt = 8 THEN
                            read_cnt := 0;
                        END IF;
                    ELSIF Instruct = RDCR THEN
                        --Read Configuration Register 1
                        SOOut_zd <= Config_reg1(7-read_cnt);
                        read_cnt := read_cnt + 1;
                        IF read_cnt = 8 THEN
                            read_cnt := 0;
                        END IF;
                    END IF;
                ELSIF oe_z THEN
                    HOLDNegOut_zd <= 'Z';
                    WPNegOut_zd   <= 'Z';
                    SOOut_zd      <= 'Z';
                    SIOut_zd      <= 'Z';
                END IF;

                IF PPBERASE_out = '1' THEN
                    IF PPB_LOCK /= '0' AND PPBOTP = '1' THEN
                        PPB_bits:= (OTHERS => '1');
                    ELSE
                        E_ERR := '1';
                    END IF;
                    WIP   := '0';
                    WEL   := '0';
                    PPBERASE_in <= '0';
                END IF;

            WHEN AUTOBOOT_PG    =>
                fast_rd <= true;
                rd      <= false;
                dual    <= false;
                ddr     <= false;
                IF oe THEN
                    any_read <= true;
                    IF Instruct = RDSR THEN
                        --Read Status Register 1
                        SOOut_zd <= Status_reg1(7-read_cnt);
                        read_cnt := read_cnt + 1;
                        IF read_cnt = 8 THEN
                            read_cnt := 0;
                        END IF;
                    ELSIF Instruct = RDSR2 THEN
                        --Read Status Register 2
                        SOOut_zd <= Status_reg2(7-read_cnt);
                        read_cnt := read_cnt + 1;
                        IF read_cnt = 8 THEN
                            read_cnt := 0;
                        END IF;
                    ELSIF Instruct = RDCR THEN
                        --Read Configuration Register 1
                        SOOut_zd <= Config_reg1(7-read_cnt);
                        read_cnt := read_cnt + 1;
                        IF read_cnt = 8 THEN
                            read_cnt := 0;
                        END IF;
                    END IF;
                ELSIF oe_z THEN
                    HOLDNegOut_zd <= 'Z';
                    WPNegOut_zd   <= 'Z';
                    SOOut_zd      <= 'Z';
                    SIOut_zd      <= 'Z';
                END IF;

                IF PDONE = '1' THEN
                    FOR I IN 0 TO 3 LOOP
                        FOR J IN 0 TO 7 LOOP
                            AutoBoot_reg(I*8+J) :=
                            AutoBoot_reg_in((3-I)*8+J);
                        END LOOP;
                    END LOOP;
                    WIP  := '0';
                    WEL  := '0';
                END IF;

            WHEN PLB_PG         =>
                fast_rd <= true;
                rd      <= false;
                dual    <= false;
                ddr     <= false;
                IF oe THEN
                    any_read <= true;
                    IF Instruct = RDSR THEN
                        --Read Status Register 1
                        SOOut_zd <= Status_reg1(7-read_cnt);
                        read_cnt := read_cnt + 1;
                        IF read_cnt = 8 THEN
                            read_cnt := 0;
                        END IF;
                    ELSIF Instruct = RDSR2 THEN
                        --Read Status Register 2
                        SOOut_zd <= Status_reg2(7-read_cnt);
                        read_cnt := read_cnt + 1;
                        IF read_cnt = 8 THEN
                            read_cnt := 0;
                        END IF;
                    ELSIF Instruct = RDCR THEN
                        --Read Configuration Register 1
                        SOOut_zd <= Config_reg1(7-read_cnt);
                        read_cnt := read_cnt + 1;
                        IF read_cnt = 8 THEN
                            read_cnt := 0;
                        END IF;
                    END IF;
                ELSIF oe_z THEN
                    HOLDNegOut_zd <= 'Z';
                    WPNegOut_zd   <= 'Z';
                    SOOut_zd      <= 'Z';
                    SIOut_zd      <= 'Z';
                END IF;

                IF PDONE = '1' THEN
                    PPB_LOCK := '0';
                    PPB_LOCK_temp <= '0';
                    WIP  := '0';
                    WEL  := '0';
                END IF;

            WHEN DYB_PG         =>
                fast_rd <= true;
                rd      <= false;
                dual    <= false;
                ddr     <= false;
                IF oe THEN
                    any_read <= true;
                    IF Instruct = RDSR THEN
                        --Read Status Register 1
                        SOOut_zd <= Status_reg1(7-read_cnt);
                        read_cnt := read_cnt + 1;
                        IF read_cnt = 8 THEN
                            read_cnt := 0;
                        END IF;
                    ELSIF Instruct = RDSR2 THEN
                        --Read Status Register 2
                        SOOut_zd <= Status_reg2(7-read_cnt);
                        read_cnt := read_cnt + 1;
                        IF read_cnt = 8 THEN
                            read_cnt := 0;
                        END IF;
                    ELSIF Instruct = RDCR THEN
                        --Read Configuration Register 1
                        SOOut_zd <= Config_reg1(7-read_cnt);
                        read_cnt := read_cnt + 1;
                        IF read_cnt = 8 THEN
                            read_cnt := 0;
                        END IF;
                    END IF;
                ELSIF oe_z THEN
                    HOLDNegOut_zd <= 'Z';
                    WPNegOut_zd   <= 'Z';
                    SOOut_zd      <= 'Z';
                    SIOut_zd      <= 'Z';
                END IF;

                IF PDONE = '1' THEN
                    DYBAR := DYBAR_in;
                    IF DYBAR = "11111111" THEN
                        DYB_bits(sect):= '1';
                    ELSIF DYBAR = "00000000" THEN
                        DYB_bits(sect):= '0';
                    ELSE
                        P_ERR := '1';
                    END IF;
                    WIP  := '0';
                    WEL  := '0';
                END IF;

            WHEN ASP_PG         =>
                fast_rd <= true;
                rd      <= false;
                dual    <= false;
                ddr     <= false;
                IF oe THEN
                    any_read <= true;
                    IF Instruct = RDSR THEN
                        --Read Status Register 1
                        SOOut_zd <= Status_reg1(7-read_cnt);
                        read_cnt := read_cnt + 1;
                        IF read_cnt = 8 THEN
                            read_cnt := 0;
                        END IF;
                    ELSIF Instruct = RDSR2 THEN
                        --Read Status Register 2
                        SOOut_zd <= Status_reg2(7-read_cnt);
                        read_cnt := read_cnt + 1;
                        IF read_cnt = 8 THEN
                            read_cnt := 0;
                        END IF;
                    ELSIF Instruct = RDCR THEN
                        --Read Configuration Register 1
                        SOOut_zd <= Config_reg1(7-read_cnt);
                        read_cnt := read_cnt + 1;
                        IF read_cnt = 8 THEN
                            read_cnt := 0;
                        END IF;
                    END IF;
                ELSIF oe_z THEN
                    HOLDNegOut_zd <= 'Z';
                    WPNegOut_zd   <= 'Z';
                    SOOut_zd      <= 'Z';
                    SIOut_zd      <= 'Z';
                END IF;

                IF PDONE = '1' THEN

                    IF (RPME = '0' AND ASP_reg_in(5) = '1') THEN
                       P_ERR := '1';
                       REPORT "RPME bit is allready programmed"
                       SEVERITY warning;
                    ELSE
                        RPME := ASP_reg_in(5);
                    END IF;

                    IF (PPBOTP = '0' AND ASP_reg_in(3) ='1') THEN
                       P_ERR := '1';
                       REPORT "PPBOTP bit is allready programmed"
                       SEVERITY warning;
                    ELSE
                        PPBOTP := ASP_reg_in(3);
                    END IF;

                    IF (PWDMLB = '1' AND PSTMLB = '1') THEN
                        IF (ASP_reg_in(2) = '0' AND ASP_reg_in(1) = '0') THEN
                            REPORT "ASPR[2:1] = 00  Illegal condition"
                            SEVERITY warning;
                            P_ERR := '1';
                        ELSE
                            IF (ASP_reg_in(2) /= '1' OR
                                ASP_reg_in(1) /= '1') THEN
                                ASPOTPFLAG <= TRUE;
                            END IF;
                            PWDMLB := ASP_reg_in(2);
                            PSTMLB := ASP_reg_in(1);
                        END IF;
                    END IF;
                    WIP  := '0';
                    WEL  := '0';
                END IF;

            WHEN NVDLR_PG    =>
                fast_rd <= true;
                rd      <= false;
                dual    <= false;
                ddr     <= false;
                IF oe THEN
                    any_read <= true;
                    IF Instruct = RDSR THEN
                        --Read Status Register 1
                        SOOut_zd <= Status_reg1(7-read_cnt);
                        read_cnt := read_cnt + 1;
                        IF read_cnt = 8 THEN
                            read_cnt := 0;
                        END IF;
                    ELSIF Instruct = RDSR2 THEN
                        --Read Status Register 2
                        SOOut_zd <= Status_reg2(7-read_cnt);
                        read_cnt := read_cnt + 1;
                        IF read_cnt = 8 THEN
                            read_cnt := 0;
                        END IF;
                    ELSIF Instruct = RDCR THEN
                        --Read Configuration Register 1
                        SOOut_zd <= Config_reg1(7-read_cnt);
                        read_cnt := read_cnt + 1;
                        IF read_cnt = 8 THEN
                            read_cnt := 0;
                        END IF;
                    END IF;
                ELSIF oe_z THEN
                    HOLDNegOut_zd <= 'Z';
                    WPNegOut_zd   <= 'Z';
                    SOOut_zd      <= 'Z';
                    SIOut_zd      <= 'Z';
                END IF;

                IF PDONE = '1' THEN
                    IF NVDLR_reg = "00000000" THEN
                        NVDLR_reg :=NVDLR_reg_in;
                        VDLR_reg := NVDLR_reg_in;
                        WIP := '0';
                        WEL := '0';
                    ELSE
                        WIP   := '0';
                        WEL   := '0';
                        P_ERR := '1';
                        REPORT "NVDLR is allready programmed"
                        SEVERITY warning;
                    END IF;
                END IF;

            WHEN RESET_STATE    =>
                --the default condition hardware reset
                --The Bank Address Register is loaded to all zeroes
                Bank_Addr_reg := (others => '0');
                IF BPNV = '1' AND FREEZE = '0' THEN
                    BP0 := '1';
                    BP1 := '1';
                    BP2 := '1';
                    BP_bits := BP2 & BP1 & BP0;
                    change_BP <= '1', '0' AFTER 1 ns;
                END IF;
                --Resets the Status register 1
                P_ERR     := '0';
                E_ERR     := '0';
                WEL       := '0';
                WIP       := '0';
                --Resets the Status register 2
                ES        := '0';
                PS        := '0';
                --Resets the Configuration register 1
                FREEZE    := '0';
                --On reset cycles the data pattern reverts back
                --to what is in the NVDLR
                VDLR_reg := NVDLR_reg;
                dlp_act := FALSE;
                --Loads the Program Buffer with all ones
                WData := (OTHERS => MaxData);

                IF PWDMLB = '0' THEN
                    PPB_LOCK := '0';
                    PPB_LOCK_temp <= '0';
                ELSE
                    PPB_LOCK := '1';
                    PPB_LOCK_temp <= '1';
                END IF;

        END CASE;

        --Output Disable Control
        IF (CSNeg_ipd = '1') THEN
            SIOut_zd        <= 'Z';
            HOLDNegOut_zd   <= 'Z';
            WPNegOut_zd     <= 'Z';
            SOOut_zd        <= 'Z';
        END IF;

    END PROCESS Functional;

    SFDPPreload:    PROCESS
    BEGIN
        SFDP_array(16#0000#) := 16#53#;
        SFDP_array(16#0001#) := 16#46#;
        SFDP_array(16#0002#) := 16#44#;
        SFDP_array(16#0003#) := 16#50#;
        SFDP_array(16#0004#) := 16#06#;
        SFDP_array(16#0005#) := 16#01#;
        SFDP_array(16#0006#) := 16#05#;
        SFDP_array(16#0007#) := 16#FF#;
        SFDP_array(16#0008#) := 16#00#;
        SFDP_array(16#0009#) := 16#00#;
        SFDP_array(16#000A#) := 16#01#;
        SFDP_array(16#000B#) := 16#09#;
        SFDP_array(16#000C#) := 16#20#;
        SFDP_array(16#000D#) := 16#11#;
        SFDP_array(16#000E#) := 16#00#;
        SFDP_array(16#000F#) := 16#FF#;
        SFDP_array(16#0010#) := 16#00#;
        SFDP_array(16#0011#) := 16#05#;
        SFDP_array(16#0012#) := 16#01#;
        SFDP_array(16#0013#) := 16#10#;
        SFDP_array(16#0014#) := 16#20#;
        SFDP_array(16#0015#) := 16#11#;
        SFDP_array(16#0016#) := 16#00#;
        SFDP_array(16#0017#) := 16#FF#;
        SFDP_array(16#0018#) := 16#00#;
        SFDP_array(16#0019#) := 16#06#;
        SFDP_array(16#001A#) := 16#01#;
        SFDP_array(16#001B#) := 16#10#;
        SFDP_array(16#001C#) := 16#20#;
        SFDP_array(16#001D#) := 16#11#;
        SFDP_array(16#001E#) := 16#00#;
        SFDP_array(16#001F#) := 16#FF#;
        SFDP_array(16#0020#) := 16#81#;
        SFDP_array(16#0021#) := 16#00#;
        SFDP_array(16#0022#) := 16#01#;
        SFDP_array(16#0023#) := 16#02#;
        SFDP_array(16#0024#) := 16#60#;
        SFDP_array(16#0025#) := 16#11#;
        SFDP_array(16#0026#) := 16#00#;
        SFDP_array(16#0027#) := 16#FF#;
        SFDP_array(16#0028#) := 16#84#;
        SFDP_array(16#0029#) := 16#00#;
        SFDP_array(16#002A#) := 16#01#;
        SFDP_array(16#002B#) := 16#02#;
        SFDP_array(16#002C#) := 16#68#;
        SFDP_array(16#002D#) := 16#11#;
        SFDP_array(16#002E#) := 16#00#;
        SFDP_array(16#002F#) := 16#FF#;
        SFDP_array(16#0030#) := 16#01#;
        SFDP_array(16#0031#) := 16#01#;
        SFDP_array(16#0032#) := 16#01#;
        SFDP_array(16#0033#) := 16#5C#;
        SFDP_array(16#0034#) := 16#00#;
        SFDP_array(16#0035#) := 16#10#;
        SFDP_array(16#0036#) := 16#00#;
        SFDP_array(16#0037#) := 16#01#;
        -- Undefined space
        FOR I IN  16#0038# TO 16#1000# LOOP
            SFDP_array(i) := 16#FF#;
        END LOOP;
        -- ID-CFI array data
        -- Manufacturer and Device ID
        SFDP_array(16#1000#) := 16#01#;
        SFDP_array(16#1001#) := 16#02#;
        SFDP_array(16#1002#) := 16#20#;
        SFDP_array(16#1003#) := 16#00#;
        -- Uniform 256kB sectors
        SFDP_array(16#1004#) := 16#00#;
        SFDP_array(16#1005#) := 16#80#;
        SFDP_array(16#1006#) := 16#FF#;
        SFDP_array(16#1007#) := 16#FF#;
        SFDP_array(16#1008#) := 16#FF#;
        SFDP_array(16#1009#) := 16#FF#;
        SFDP_array(16#100A#) := 16#FF#;
        SFDP_array(16#100B#) := 16#FF#;
        SFDP_array(16#100C#) := 16#FF#;
        SFDP_array(16#100D#) := 16#FF#;
        SFDP_array(16#100E#) := 16#FF#;
        SFDP_array(16#100F#) := 16#FF#;
        -- CFI Query Identification String
        SFDP_array(16#1010#) := 16#51#;
        SFDP_array(16#1011#) := 16#52#;
        SFDP_array(16#1012#) := 16#59#;
        SFDP_array(16#1013#) := 16#02#;
        SFDP_array(16#1014#) := 16#00#;
        SFDP_array(16#1015#) := 16#40#;
        SFDP_array(16#1016#) := 16#00#;
        SFDP_array(16#1017#) := 16#53#;
        SFDP_array(16#1018#) := 16#46#;
        SFDP_array(16#1019#) := 16#51#;
        SFDP_array(16#101A#) := 16#00#;
        -- CFI system interface string
        SFDP_array(16#101B#) := 16#27#;
        SFDP_array(16#101C#) := 16#36#;
        SFDP_array(16#101D#) := 16#00#;
        SFDP_array(16#101E#) := 16#00#;
        SFDP_array(16#101F#) := 16#06#;
        SFDP_array(16#1020#) := 16#09#;
        -- 256kB sector
        SFDP_array(16#1021#) := 16#09#;
        SFDP_array(16#1022#) := 16#11#;
        SFDP_array(16#1023#) := 16#02#;
        SFDP_array(16#1024#) := 16#02#;
        SFDP_array(16#1025#) := 16#03#;
        SFDP_array(16#1026#) := 16#03#;
        -- Device Geometry Definition
        SFDP_array(16#1027#) := 16#1A#;
        SFDP_array(16#1028#) := 16#02#;
        SFDP_array(16#1029#) := 16#01#;
        SFDP_array(16#102A#) := 16#09#;
        SFDP_array(16#102B#) := 16#00#;
        SFDP_array(16#102C#) := 16#01#;
        SFDP_array(16#102D#) := 16#FF#;
        SFDP_array(16#102E#) := 16#00#;
        SFDP_array(16#102F#) := 16#00#;
        SFDP_array(16#1030#) := 16#04#;
        SFDP_array(16#1031#) := 16#FF#;
        SFDP_array(16#1032#) := 16#FF#;
        SFDP_array(16#1033#) := 16#FF#;
        SFDP_array(16#1034#) := 16#FF#;
        SFDP_array(16#1035#) := 16#FF#;
        SFDP_array(16#1036#) := 16#FF#;
        SFDP_array(16#1037#) := 16#FF#;
        SFDP_array(16#1038#) := 16#FF#;
        SFDP_array(16#1039#) := 16#FF#;
        SFDP_array(16#103A#) := 16#FF#;
        SFDP_array(16#103B#) := 16#FF#;
        SFDP_array(16#103C#) := 16#FF#;
        SFDP_array(16#103D#) := 16#FF#;
        SFDP_array(16#103E#) := 16#FF#;
        SFDP_array(16#103F#) := 16#FF#;
        -- CFI Primary Vendor-Specific Extended Query
        SFDP_array(16#1040#) := 16#50#;
        SFDP_array(16#1041#) := 16#52#;
        SFDP_array(16#1042#) := 16#49#;
        SFDP_array(16#1043#) := 16#31#;
        SFDP_array(16#1044#) := 16#33#;
        SFDP_array(16#1045#) := 16#21#;
        SFDP_array(16#1046#) := 16#02#;
        SFDP_array(16#1047#) := 16#01#;
        SFDP_array(16#1048#) := 16#00#;
        SFDP_array(16#1049#) := 16#08#;
        SFDP_array(16#104A#) := 16#00#;
        SFDP_array(16#104B#) := 16#01#;
        SFDP_array(16#104C#) := 16#04#;
        SFDP_array(16#104D#) := 16#00#;
        SFDP_array(16#104E#) := 16#00#;
        SFDP_array(16#104F#) := 16#07#;
        SFDP_array(16#1050#) := 16#01#;
        -- CFI Alternate Vendor Specific Extended Query Header
        SFDP_array(16#1051#) := 16#41#;
        SFDP_array(16#1052#) := 16#4C#;
        SFDP_array(16#1053#) := 16#54#;
        SFDP_array(16#1054#) := 16#32#;
        SFDP_array(16#1055#) := 16#30#;
        -- CFI Alternate Vendor Specific Extended Query Parameter 0
        SFDP_array(16#1056#) := 16#00#;
        SFDP_array(16#1057#) := 16#10#;
        SFDP_array(16#1058#) := 16#53#;
        SFDP_array(16#1059#) := 16#32#;
        SFDP_array(16#105A#) := 16#35#;
        SFDP_array(16#105B#) := 16#46#;
        SFDP_array(16#105C#) := 16#4C#;
        SFDP_array(16#105D#) := 16#35#;
        SFDP_array(16#105E#) := 16#31#;
        SFDP_array(16#105F#) := 16#32#;
        SFDP_array(16#1060#) := 16#53#;
        SFDP_array(16#1061#) := 16#FF#;
        SFDP_array(16#1062#) := 16#FF#;
        SFDP_array(16#1063#) := 16#FF#;
        SFDP_array(16#1064#) := 16#FF#;
        SFDP_array(16#1065#) := 16#FF#;
        SFDP_array(16#1066#) := 16#FF#;
        SFDP_array(16#1067#) := 16#FF#;
        -- CFI Alternate Vendor-Specific Extended Query Parameter 80h
        SFDP_array(16#1068#) := 16#80#;
        SFDP_array(16#1069#) := 16#01#;
        SFDP_array(16#106A#) := 16#F0#;
        -- CFI Alternate Vendor-Specific Extended Query Parameter 84h
        SFDP_array(16#106B#) := 16#84#;
        SFDP_array(16#106C#) := 16#08#;
        SFDP_array(16#106D#) := 16#85#;
        SFDP_array(16#106E#) := 16#28#;
        SFDP_array(16#106F#) := 16#8A#;
        SFDP_array(16#1070#) := 16#64#;
        SFDP_array(16#1071#) := 16#75#;
        SFDP_array(16#1072#) := 16#28#;
        SFDP_array(16#1073#) := 16#7A#;
        SFDP_array(16#1074#) := 16#64#;
        -- CFI Alternate Vendor-Specific Extended Query Parameter 88h
        SFDP_array(16#1075#) := 16#88#;
        SFDP_array(16#1076#) := 16#04#;
        SFDP_array(16#1077#) := 16#0A#;
        SFDP_array(16#1078#) := 16#01#;
        SFDP_array(16#1079#) := 16#FF#;
        SFDP_array(16#107A#) := 16#01#;
        -- CFI Alternate Vendor-Specific Extended Query Parameter 8Ch
        SFDP_array(16#107B#) := 16#8C#;
        SFDP_array(16#107C#) := 16#06#;
        SFDP_array(16#107D#) := 16#96#;
        SFDP_array(16#107E#) := 16#01#;
        SFDP_array(16#107F#) := 16#23#;
        SFDP_array(16#1080#) := 16#00#;
        SFDP_array(16#1081#) := 16#23#;
        SFDP_array(16#1082#) := 16#00#;
        -- CFI Alternate Vendor-Specific Extended Query Parameter 90h
        SFDP_array(16#1083#) := 16#90#;
        SFDP_array(16#1084#) := 16#56#;
        SFDP_array(16#1085#) := 16#06#;
        SFDP_array(16#1086#) := 16#0E#;
        SFDP_array(16#1087#) := 16#46#;
        SFDP_array(16#1088#) := 16#43#;
        SFDP_array(16#1089#) := 16#03#;
        SFDP_array(16#108A#) := 16#13#;
        SFDP_array(16#108B#) := 16#0B#;
        SFDP_array(16#108C#) := 16#0C#;
        SFDP_array(16#108D#) := 16#3B#;
        SFDP_array(16#108E#) := 16#3C#;
        SFDP_array(16#108F#) := 16#6B#;
        SFDP_array(16#1090#) := 16#6C#;
        SFDP_array(16#1091#) := 16#BB#;
        SFDP_array(16#1092#) := 16#BC#;
        SFDP_array(16#1093#) := 16#EB#;
        SFDP_array(16#1094#) := 16#EC#;
        SFDP_array(16#1095#) := 16#32#;
        SFDP_array(16#1096#) := 16#03#;
        SFDP_array(16#1097#) := 16#00#;
        SFDP_array(16#1098#) := 16#00#;
        SFDP_array(16#1099#) := 16#00#;
        SFDP_array(16#109A#) := 16#00#;
        SFDP_array(16#109B#) := 16#00#;
        SFDP_array(16#109C#) := 16#00#;
        SFDP_array(16#109D#) := 16#00#;
        SFDP_array(16#109E#) := 16#00#;
        SFDP_array(16#109F#) := 16#00#;
        SFDP_array(16#10A0#) := 16#04#;
        SFDP_array(16#10A1#) := 16#02#;
        SFDP_array(16#10A2#) := 16#01#;
        SFDP_array(16#10A3#) := 16#50#;
        SFDP_array(16#10A4#) := 16#00#;
        SFDP_array(16#10A5#) := 16#FF#;
        SFDP_array(16#10A6#) := 16#FF#;
        SFDP_array(16#10A7#) := 16#00#;
        SFDP_array(16#10A8#) := 16#08#;
        SFDP_array(16#10A9#) := 16#00#;
        SFDP_array(16#10AA#) := 16#08#;
        SFDP_array(16#10AB#) := 16#00#;
        SFDP_array(16#10AC#) := 16#08#;
        SFDP_array(16#10AD#) := 16#00#;
        SFDP_array(16#10AE#) := 16#04#;
        SFDP_array(16#10AF#) := 16#02#;
        SFDP_array(16#10B0#) := 16#04#;
        SFDP_array(16#10B1#) := 16#5A#;
        SFDP_array(16#10B2#) := 16#01#;
        SFDP_array(16#10B3#) := 16#FF#;
        SFDP_array(16#10B4#) := 16#FF#;
        SFDP_array(16#10B5#) := 16#00#;
        SFDP_array(16#10B6#) := 16#08#;
        SFDP_array(16#10B7#) := 16#00#;
        SFDP_array(16#10B8#) := 16#08#;
        SFDP_array(16#10B9#) := 16#00#;
        SFDP_array(16#10BA#) := 16#08#;
        SFDP_array(16#10BB#) := 16#00#;
        SFDP_array(16#10BC#) := 16#05#;
        SFDP_array(16#10BD#) := 16#02#;
        SFDP_array(16#10BE#) := 16#04#;
        SFDP_array(16#10BF#) := 16#68#;
        SFDP_array(16#10C0#) := 16#02#;
        SFDP_array(16#10C1#) := 16#FF#;
        SFDP_array(16#10C2#) := 16#FF#;
        SFDP_array(16#10C3#) := 16#00#;
        SFDP_array(16#10C4#) := 16#08#;
        SFDP_array(16#10C5#) := 16#00#;
        SFDP_array(16#10C6#) := 16#08#;
        SFDP_array(16#10C7#) := 16#00#;
        SFDP_array(16#10C8#) := 16#08#;
        SFDP_array(16#10C9#) := 16#00#;
        SFDP_array(16#10CA#) := 16#06#;
        SFDP_array(16#10CB#) := 16#02#;
        SFDP_array(16#10CC#) := 16#05#;
        SFDP_array(16#10CD#) := 16#85#;
        SFDP_array(16#10CE#) := 16#02#;
        SFDP_array(16#10CF#) := 16#FF#;
        SFDP_array(16#10D0#) := 16#FF#;
        SFDP_array(16#10D1#) := 16#00#;
        SFDP_array(16#10D2#) := 16#08#;
        SFDP_array(16#10D3#) := 16#FF#;
        SFDP_array(16#10D4#) := 16#FF#;
        -- CFI Alternate Vendor-Specific Extended Query Parameter 9Ah-HPLC
        SFDP_array(16#10D5#) := 16#9A#;
        SFDP_array(16#10D6#) := 16#2A#;
        SFDP_array(16#10D7#) := 16#05#;
        SFDP_array(16#10D8#) := 16#08#;
        SFDP_array(16#10D9#) := 16#46#;
        SFDP_array(16#10DA#) := 16#43#;
        SFDP_array(16#10DB#) := 16#0D#;
        SFDP_array(16#10DC#) := 16#0E#;
        SFDP_array(16#10DD#) := 16#BD#;
        SFDP_array(16#10DE#) := 16#BE#;
        SFDP_array(16#10DF#) := 16#ED#;
        SFDP_array(16#10E0#) := 16#EE#;
        SFDP_array(16#10E1#) := 16#32#;
        SFDP_array(16#10E2#) := 16#03#;
        SFDP_array(16#10E3#) := 16#00#;
        SFDP_array(16#10E4#) := 16#04#;
        SFDP_array(16#10E5#) := 16#00#;
        SFDP_array(16#10E6#) := 16#04#;
        SFDP_array(16#10E7#) := 16#01#;
        SFDP_array(16#10E8#) := 16#03#;
        SFDP_array(16#10E9#) := 16#42#;
        SFDP_array(16#10EA#) := 16#00#;
        SFDP_array(16#10EB#) := 16#00#;
        SFDP_array(16#10EC#) := 16#05#;
        SFDP_array(16#10ED#) := 16#00#;
        SFDP_array(16#10EE#) := 16#06#;
        SFDP_array(16#10EF#) := 16#01#;
        SFDP_array(16#10F0#) := 16#06#;
        SFDP_array(16#10F1#) := 16#42#;
        SFDP_array(16#10F2#) := 16#01#;
        SFDP_array(16#10F3#) := 16#00#;
        SFDP_array(16#10F4#) := 16#06#;
        SFDP_array(16#10F5#) := 16#00#;
        SFDP_array(16#10F6#) := 16#07#;
        SFDP_array(16#10F7#) := 16#01#;
        SFDP_array(16#10F8#) := 16#07#;
        SFDP_array(16#10F9#) := 16#42#;
        SFDP_array(16#10FA#) := 16#02#;
        SFDP_array(16#10FB#) := 16#00#;
        SFDP_array(16#10FC#) := 16#07#;
        SFDP_array(16#10FD#) := 16#00#;
        SFDP_array(16#10FE#) := 16#08#;
        SFDP_array(16#10FF#) := 16#01#;
        SFDP_array(16#1100#) := 16#08#;
        -- CFI Alternate Vendor-Specific Extended Query Parameter 90h-EHPLC
        SFDP_array(16#1101#) := 16#90#;
        SFDP_array(16#1102#) := 16#56#;
        SFDP_array(16#1103#) := 16#06#;
        SFDP_array(16#1104#) := 16#0E#;
        SFDP_array(16#1105#) := 16#46#;
        SFDP_array(16#1106#) := 16#43#;
        SFDP_array(16#1107#) := 16#03#;
        SFDP_array(16#1108#) := 16#13#;
        SFDP_array(16#1109#) := 16#0B#;
        SFDP_array(16#110A#) := 16#0C#;
        SFDP_array(16#110B#) := 16#3B#;
        SFDP_array(16#110C#) := 16#3C#;
        SFDP_array(16#110D#) := 16#6B#;
        SFDP_array(16#110E#) := 16#6C#;
        SFDP_array(16#110F#) := 16#BB#;
        SFDP_array(16#1110#) := 16#BC#;
        SFDP_array(16#1111#) := 16#EB#;
        SFDP_array(16#1112#) := 16#EC#;
        SFDP_array(16#1113#) := 16#32#;
        SFDP_array(16#1114#) := 16#03#;
        SFDP_array(16#1115#) := 16#00#;
        SFDP_array(16#1116#) := 16#00#;
        SFDP_array(16#1117#) := 16#00#;
        SFDP_array(16#1118#) := 16#00#;
        SFDP_array(16#1119#) := 16#00#;
        SFDP_array(16#111A#) := 16#00#;
        SFDP_array(16#111B#) := 16#00#;
        SFDP_array(16#111C#) := 16#00#;
        SFDP_array(16#111D#) := 16#04#;
        SFDP_array(16#111E#) := 16#00#;
        SFDP_array(16#111F#) := 16#02#;
        SFDP_array(16#1120#) := 16#01#;
        SFDP_array(16#1121#) := 16#50#;
        SFDP_array(16#1122#) := 16#00#;
        SFDP_array(16#1123#) := 16#FF#;
        SFDP_array(16#1124#) := 16#FF#;
        SFDP_array(16#1125#) := 16#00#;
        SFDP_array(16#1126#) := 16#08#;
        SFDP_array(16#1127#) := 16#00#;
        SFDP_array(16#1128#) := 16#08#;
        SFDP_array(16#1129#) := 16#00#;
        SFDP_array(16#112A#) := 16#08#;
        SFDP_array(16#112B#) := 16#04#;
        SFDP_array(16#112C#) := 16#00#;
        SFDP_array(16#112D#) := 16#02#;
        SFDP_array(16#112E#) := 16#04#;
        SFDP_array(16#112F#) := 16#5A#;
        SFDP_array(16#1130#) := 16#01#;
        SFDP_array(16#1131#) := 16#FF#;
        SFDP_array(16#1132#) := 16#FF#;
        SFDP_array(16#1133#) := 16#00#;
        SFDP_array(16#1134#) := 16#08#;
        SFDP_array(16#1135#) := 16#00#;
        SFDP_array(16#1136#) := 16#08#;
        SFDP_array(16#1137#) := 16#00#;
        SFDP_array(16#1138#) := 16#08#;
        SFDP_array(16#1139#) := 16#04#;
        SFDP_array(16#113A#) := 16#01#;
        SFDP_array(16#113B#) := 16#02#;
        SFDP_array(16#113C#) := 16#04#;
        SFDP_array(16#113D#) := 16#68#;
        SFDP_array(16#113E#) := 16#02#;
        SFDP_array(16#113F#) := 16#FF#;
        SFDP_array(16#1140#) := 16#FF#;
        SFDP_array(16#1141#) := 16#00#;
        SFDP_array(16#1142#) := 16#08#;
        SFDP_array(16#1143#) := 16#00#;
        SFDP_array(16#1144#) := 16#08#;
        SFDP_array(16#1145#) := 16#00#;
        SFDP_array(16#1146#) := 16#08#;
        SFDP_array(16#1147#) := 16#04#;
        SFDP_array(16#1148#) := 16#02#;
        SFDP_array(16#1149#) := 16#02#;
        SFDP_array(16#114A#) := 16#05#;
        SFDP_array(16#114B#) := 16#85#;
        SFDP_array(16#114C#) := 16#02#;
        SFDP_array(16#114D#) := 16#FF#;
        SFDP_array(16#114E#) := 16#FF#;
        SFDP_array(16#114F#) := 16#00#;
        SFDP_array(16#1150#) := 16#08#;
        SFDP_array(16#1151#) := 16#FF#;
        SFDP_array(16#1152#) := 16#FF#;
        SFDP_array(16#1153#) := 16#FF#;
        SFDP_array(16#1154#) := 16#FF#;
        SFDP_array(16#1155#) := 16#FF#;
        SFDP_array(16#1156#) := 16#FF#;
        SFDP_array(16#1157#) := 16#FF#;
        SFDP_array(16#1158#) := 16#FF#;
        -- CFI Alternate Vendor-Specific Extended Query Parameter 9Ah-EHPLC
        SFDP_array(16#1159#) := 16#9A#;
        SFDP_array(16#115A#) := 16#2A#;
        SFDP_array(16#115B#) := 16#05#;
        SFDP_array(16#115C#) := 16#08#;
        SFDP_array(16#115D#) := 16#46#;
        SFDP_array(16#115E#) := 16#43#;
        SFDP_array(16#115F#) := 16#0D#;
        SFDP_array(16#1160#) := 16#0E#;
        SFDP_array(16#1161#) := 16#BD#;
        SFDP_array(16#1162#) := 16#ED#;
        SFDP_array(16#1163#) := 16#EE#;
        SFDP_array(16#1164#) := 16#32#;
        SFDP_array(16#1165#) := 16#03#;
        SFDP_array(16#1166#) := 16#04#;
        SFDP_array(16#1167#) := 16#01#;
        SFDP_array(16#1168#) := 16#02#;
        SFDP_array(16#1169#) := 16#02#;
        SFDP_array(16#116A#) := 16#01#;
        SFDP_array(16#116B#) := 16#03#;
        SFDP_array(16#116C#) := 16#42#;
        SFDP_array(16#116D#) := 16#00#;
        SFDP_array(16#116E#) := 16#04#;
        SFDP_array(16#116F#) := 16#02#;
        SFDP_array(16#1170#) := 16#02#;
        SFDP_array(16#1171#) := 16#04#;
        SFDP_array(16#1172#) := 16#01#;
        SFDP_array(16#1173#) := 16#06#;
        SFDP_array(16#1174#) := 16#42#;
        SFDP_array(16#1175#) := 16#01#;
        SFDP_array(16#1176#) := 16#04#;
        SFDP_array(16#1177#) := 16#04#;
        SFDP_array(16#1178#) := 16#02#;
        SFDP_array(16#1179#) := 16#05#;
        SFDP_array(16#117A#) := 16#01#;
        SFDP_array(16#117B#) := 16#07#;
        SFDP_array(16#117C#) := 16#42#;
        SFDP_array(16#117D#) := 16#02#;
        SFDP_array(16#117E#) := 16#04#;
        SFDP_array(16#117F#) := 16#05#;
        SFDP_array(16#1180#) := 16#02#;
        SFDP_array(16#1181#) := 16#06#;
        SFDP_array(16#1182#) := 16#01#;
        SFDP_array(16#1183#) := 16#08#;
        -- CFI Alternate Vendor-Specific Extended Query Parameter F0h RFU
        SFDP_array(16#1184#) := 16#F0#;
        SFDP_array(16#1185#) := 16#0F#;
        SFDP_array(16#1186#) := 16#FF#;
        SFDP_array(16#1187#) := 16#FF#;
        SFDP_array(16#1188#) := 16#FF#;
        SFDP_array(16#1189#) := 16#FF#;
        SFDP_array(16#118A#) := 16#FF#;
        SFDP_array(16#118B#) := 16#FF#;
        SFDP_array(16#118C#) := 16#FF#;
        SFDP_array(16#118D#) := 16#FF#;
        SFDP_array(16#118E#) := 16#FF#;
        SFDP_array(16#118F#) := 16#FF#;
        SFDP_array(16#1190#) := 16#FF#;
        SFDP_array(16#1191#) := 16#FF#;
        SFDP_array(16#1192#) := 16#FF#;
        SFDP_array(16#1193#) := 16#FF#;
        SFDP_array(16#1194#) := 16#FF#;
        -- CFI Alternate Vendor-Specific Extended Query Parameter A5h
        -- Jedec SFDP Rev B
        SFDP_array(16#1195#) := 16#A5#;
        SFDP_array(16#1196#) := 16#50#;
        SFDP_array(16#1197#) := 16#E7#;
        SFDP_array(16#1198#) := 16#FF#;
        SFDP_array(16#1199#) := 16#F3#;
        SFDP_array(16#119A#) := 16#FF#;
        SFDP_array(16#119B#) := 16#FF#;
        SFDP_array(16#119C#) := 16#FF#;
        SFDP_array(16#119D#) := 16#FF#;
        SFDP_array(16#119E#) := 16#1F#;
        SFDP_array(16#119F#) := 16#44#;
        SFDP_array(16#11A0#) := 16#EB#;
        SFDP_array(16#11A1#) := 16#08#;
        SFDP_array(16#11A2#) := 16#6B#;
        SFDP_array(16#11A3#) := 16#08#;
        SFDP_array(16#11A4#) := 16#3B#;
        IF EHP THEN
            SFDP_array(16#11A5#) := 16#80#;
        ELSE
            SFDP_array(16#11A5#) := 16#04#;
        END IF;
        SFDP_array(16#11A6#) := 16#BB#;
        SFDP_array(16#11A7#) := 16#EE#;
        SFDP_array(16#11A8#) := 16#FF#;
        SFDP_array(16#11A9#) := 16#FF#;
        SFDP_array(16#11AA#) := 16#FF#;
        SFDP_array(16#11AB#) := 16#FF#;
        SFDP_array(16#11AC#) := 16#FF#;
        SFDP_array(16#11AD#) := 16#FF#;
        SFDP_array(16#11AE#) := 16#FF#;
        SFDP_array(16#11AF#) := 16#FF#;
        SFDP_array(16#11B0#) := 16#FF#;
        SFDP_array(16#11B1#) := 16#FF#;
        SFDP_array(16#11B2#) := 16#EB#;
        SFDP_array(16#11B3#) := 16#00#;
        SFDP_array(16#11B4#) := 16#FF#;
        SFDP_array(16#11B5#) := 16#00#;
        SFDP_array(16#11B6#) := 16#FF#;
        SFDP_array(16#11B7#) := 16#12#;
        SFDP_array(16#11B8#) := 16#D8#;
        SFDP_array(16#11B9#) := 16#00#;
        SFDP_array(16#11BA#) := 16#FF#;
        SFDP_array(16#11BB#) := 16#F2#;
        SFDP_array(16#11BC#) := 16#FF#;
        SFDP_array(16#11BD#) := 16#0F#;
        SFDP_array(16#11BE#) := 16#FF#;
        SFDP_array(16#11BF#) := 16#91#;
        SFDP_array(16#11C0#) := 16#25#;
        SFDP_array(16#11C1#) := 16#07#;
        SFDP_array(16#11C2#) := 16#D9#;
        SFDP_array(16#11C3#) := 16#EC#;
        SFDP_array(16#11C4#) := 16#83#;
        SFDP_array(16#11C5#) := 16#18#;
        SFDP_array(16#11C6#) := 16#45#;
        SFDP_array(16#11C7#) := 16#8A#;
        SFDP_array(16#11C8#) := 16#85#;
        SFDP_array(16#11C9#) := 16#7A#;
        SFDP_array(16#11CA#) := 16#75#;
        SFDP_array(16#11CB#) := 16#F7#;
        SFDP_array(16#11CC#) := 16#FF#;
        SFDP_array(16#11CD#) := 16#FF#;
        SFDP_array(16#11CE#) := 16#FF#;
        SFDP_array(16#11CF#) := 16#00#;
        SFDP_array(16#11D0#) := 16#F6#;
        SFDP_array(16#11D1#) := 16#5D#;
        SFDP_array(16#11D2#) := 16#FF#;
        SFDP_array(16#11D3#) := 16#F0#;
        SFDP_array(16#11D4#) := 16#28#;
        SFDP_array(16#11D5#) := 16#FA#;
        SFDP_array(16#11D6#) := 16#A8#;
        SFDP_array(16#11D7#) := 16#FF#;
        SFDP_array(16#11D8#) := 16#00#;
        SFDP_array(16#11D9#) := 16#00#;
        SFDP_array(16#11DA#) := 16#FF#;
        SFDP_array(16#11DB#) := 16#F4#;
        SFDP_array(16#11DC#) := 16#FF#;
        SFDP_array(16#11DD#) := 16#FF#;
        SFDP_array(16#11DE#) := 16#3F#;
        SFDP_array(16#11DF#) := 16#FF#;
        SFDP_array(16#11E0#) := 16#E8#;
        SFDP_array(16#11E1#) := 16#FF#;
        SFDP_array(16#11E2#) := 16#FF#;
        SFDP_array(16#11E3#) := 16#FF#;
        SFDP_array(16#11E4#) := 16#FF#;
        SFDP_array(16#11E5#) := 16#DC#;
        SFDP_array(16#11E6#) := 16#FF#;
        WAIT;
    END PROCESS SFDPPreload;

    AspRegInit: PROCESS(ASP_INIT)
    BEGIN
        IF ASP_INIT = 0 THEN
            ASP_reg := to_slv(16#FE4F#,16);
        ELSE
            ASP_reg := to_slv(16#FE7F#,16);
        END IF;
    END PROCESS AspRegInit;

    Protect : PROCESS(change_BP)
    BEGIN
        IF rising_edge(change_BP) THEN

            CASE BP_bits IS
                WHEN "000" =>
                    Sec_Prot := (OTHERS => '0');
                WHEN "001" =>
                    IF TBPROT = '0' THEN
                        Sec_Prot(SecNum downto (SecNum+1)*63/64)
                                                    := (OTHERS => '1');
                        Sec_Prot((SecNum+1)*63/64 - 1 downto 0)
                                                    := (OTHERS => '0');
                    ELSE
                        Sec_Prot((SecNum+1)/64 - 1 downto 0)
                                                    := (OTHERS => '1');
                        Sec_Prot(SecNum downto (SecNum+1)/64)
                                                    := (OTHERS => '0');
                    END IF;
                WHEN "010" =>
                    IF TBPROT =  '0' THEN
                        Sec_Prot(SecNum downto (SecNum+1)*31/32)
                                                    := (OTHERS => '1');
                        Sec_Prot((SecNum+1)*31/32 - 1 downto 0)
                                                    := (OTHERS => '0');
                    ELSE
                        Sec_Prot((SecNum+1)/32 - 1 downto 0)
                                                    := (others => '1');
                        Sec_Prot(SecNum downto (SecNum+1)/32)
                                                    := (OTHERS => '0');
                    END IF;
                WHEN "011" =>
                    IF TBPROT =  '0' THEN
                        Sec_Prot(SecNum downto (SecNum+1)*15/16)
                                                    := (OTHERS => '1');
                        Sec_Prot((SecNum+1)*15/16 - 1 downto 0)
                                                    := (OTHERS => '0');
                    ELSE
                        Sec_Prot((SecNum+1)/16 - 1 downto 0)
                                                    := (OTHERS => '1');
                        Sec_Prot(SecNum downto (SecNum+1)/16)
                                                    := (OTHERS => '0');
                    END IF;
                WHEN "100" =>
                    IF TBPROT =  '0' THEN
                        Sec_Prot(SecNum downto (SecNum+1)*7/8)
                                                    := (OTHERS => '1');
                        Sec_Prot((SecNum+1)*7/8 - 1 downto 0)
                                                    := (OTHERS => '0');
                    ELSE
                        Sec_Prot((SecNum+1)/8 - 1 downto 0) := (OTHERS => '1');
                        Sec_Prot(SecNum downto (SecNum+1)/8):= (OTHERS => '0');
                    END IF;
                WHEN "101" =>
                    IF TBPROT =  '0' THEN
                        Sec_Prot(SecNum downto (SecNum+1)*3/4)
                                                    := (OTHERS => '1');
                        Sec_Prot((SecNum+1)*3/4 - 1 downto 0)
                                                    := (OTHERS => '0');
                    ELSE
                        Sec_Prot((SecNum+1)/4 - 1 downto 0) := (OTHERS => '1');
                        Sec_Prot(SecNum downto (SecNum+1)/4):= (OTHERS => '0');
                    END IF;

                WHEN "110" =>
                    IF TBPROT =  '0' THEN
                        Sec_Prot(SecNum downto (SecNum+1)/2):= (OTHERS => '1');
                        Sec_Prot((SecNum+1)/2 - 1 downto 0) := (OTHERS => '0');
                    ELSE
                        Sec_Prot((SecNum+1)/2 - 1 downto 0) := (OTHERS => '1');
                        Sec_Prot(SecNum downto (SecNum+1)/2):= (OTHERS => '0');
                    END IF;

                WHEN OTHERS =>
                    Sec_Prot := (OTHERS => '1');
            END CASE;
        END IF;
    END PROCESS Protect;

    HOLD_FRAME_ON_PO_ZD : PROCESS(SOOut_zd, SIOut_zd, HOLDNeg_pullup)
    BEGIN
        IF (HOLDNeg_pullup = '0' AND QUAD /= '1') THEN
            hold_mode := TRUE;
            SIOut_z <= 'Z';
            SOOut_z <= 'Z';
        ELSE
            IF hold_mode THEN
                SIOut_z <= SIOut_zd AFTER tpd_HOLDNeg_oSO(trz0);
                SOOut_z <= SOOut_zd AFTER tpd_HOLDNeg_oSO(trz0);
                hold_mode := FALSE;
            ELSE
                SIOut_z <= SIOut_zd;
                SOOut_z <= SOOut_zd;
                hold_mode := FALSE;
            END IF;
        END IF;
    END PROCESS HOLD_FRAME_ON_PO_ZD;

    HOLD_PULL_UP : PROCESS(HOLDNegIn)
    BEGIN
        IF (QUAD = '0') THEN
            IF (HOLDNegIn = 'Z') THEN
                HOLDNeg_pullup <= '1';
            ELSE
                HOLDNeg_pullup <= HOLDNegIn;
            END IF;
        END IF;
    END PROCESS HOLD_PULL_UP;

    WP_PULL_UP : PROCESS(WPNegIn)
    BEGIN
        IF (QUAD = '0') THEN
            IF (WPNegIn = 'Z') THEN
                WPNeg_pullup <= '1';
            ELSE
                WPNeg_pullup <= WPNegIn;
            END IF;
        END IF;
    END PROCESS WP_PULL_UP;

    RST_PULL_UP : PROCESS(RSTNeg)
    BEGIN
        IF (RSTNeg = 'Z') THEN
            RSTNeg_pullup <= '1';
        ELSE
            RSTNeg_pullup <= RSTNeg;
        END IF;
    END PROCESS RST_PULL_UP;

    ---------------------------------------------------------------------------
    ---- File Read Section - Preload Control
    ---------------------------------------------------------------------------
    MemPreload : PROCESS

        -- text file input variables
        FILE mem_file         : text  is  mem_file_name;
        FILE otp_file         : text  is  otp_file_name;
        VARIABLE ind          : NATURAL RANGE 0 TO AddrRANGE := 0;
        VARIABLE otp_ind      : NATURAL RANGE 16#000# TO 16#3FF# := 16#000#;
        VARIABLE buf          : line;

    BEGIN
    ---------------------------------------------------------------------------
    --s25fl512s memory preload file format
-----------------------------------
    ---------------------------------------------------------------------------
    --   /       - comment
    --   @aaaaaa - <aaaaaa> stands for address
    --   dd      - <dd> is byte to be written at Mem(aaaaaa++)
    --             (aaaaaa is incremented at every load)
    --   only first 1-7 columns are loaded. NO empty lines !!!!!!!!!!!!!!!!
    ---------------------------------------------------------------------------
         -- memory preload
        IF (mem_file_name /= "none" AND UserPreload) THEN
            ind := 0;
            Mem := (OTHERS => MaxData);
            WHILE (not ENDFILE (mem_file)) LOOP
                READLINE (mem_file, buf);
                IF buf(1) = '/' THEN
                    NEXT;
                ELSIF buf(1) = '@' THEN
                    IF ind > AddrRANGE THEN
                        ASSERT false
                            REPORT "Given preload address is out of" &
                                   "memory address range"
                            SEVERITY warning;
                    ELSE
                        ind := h(buf(2 to 8)); --address
                    END IF;
                ELSE
                    Mem(ind) := h(buf(1 to 2));
                    IF ind < AddrRANGE THEN
                        ind := ind + 1;
                    END IF;
                END IF;
            END LOOP;
        END IF;

    ---------------------------------------------------------------------------
    --s25fl512s_otp memory preload file format
    ---------------------------------------------------------------------------
    --   /       - comment
    --   @aaa - <aaa> stands for address
    --   dd      - <dd> is byte to be written at OTPMem(aaa++)
    --             (aaa is incremented at every load)
    --   only first 1-4 columns are loaded. NO empty lines !!!!!!!!!!!!!!!!
    ---------------------------------------------------------------------------

         -- memory preload
        IF (otp_file_name /= "none" AND UserPreload) THEN
            otp_ind := 16#000#;
            OTPMem := (OTHERS => MaxData);
            WHILE (not ENDFILE (otp_file)) LOOP
                READLINE (otp_file, buf);
                IF buf(1) = '/' THEN
                    NEXT;
                ELSIF buf(1) = '@' THEN
                    IF otp_ind > 16#3FF# OR otp_ind < 16#000# THEN
                        ASSERT false
                            REPORT "Given preload address is out of" &
                                   "OTP address range"
                            SEVERITY warning;
                    ELSE
                        otp_ind := h(buf(2 to 4)); --address
                    END IF;
                ELSE
                    OTPMem(otp_ind) := h(buf(1 to 2));
                    IF otp_ind < 16#3FF# THEN
                        otp_ind := otp_ind + 1;
                    END IF;
                END IF;
            END LOOP;
        END IF;

        LOCK_BYTE1 := to_slv(OTPMem(16#10#),8);
        LOCK_BYTE2 := to_slv(OTPMem(16#11#),8);
        LOCK_BYTE3 := to_slv(OTPMem(16#12#),8);
        LOCK_BYTE4 := to_slv(OTPMem(16#13#),8);

        WAIT;
    END PROCESS MemPreload;

    ----------------------------------------------------------------------------
    -- Path Delay Section
    ----------------------------------------------------------------------------

    SO_Out_PathDelay_Gen : PROCESS(SOOut_z)

            VARIABLE SO_GlitchData : VitalGlitchDataType;
        BEGIN
            VitalPathDelay01Z (
                OutSignal       => SOOut,
                OutSignalName   => "oSO",
                OutTemp         => SOOut_z,
                Mode            => VitalTransport,
                GlitchData      => SO_GlitchData,
                Paths           => (
                    0 => (InputChangeTime => SCK_ipd'LAST_EVENT,
                        PathDelay => VitalExtendtofillDelay(tpd_SCK_oSO_normal),
                        PathCondition   => NOT(ddr)),
                    1 => (InputChangeTime => SCK_ipd'LAST_EVENT,
                        PathDelay   => VitalExtendtofillDelay(tpd_SCK_oSO_DDR),
                        PathCondition   => (ddr OR fast_rd)),
                    2 => (InputChangeTime => CSNeg_ipd'LAST_EVENT,
                        PathDelay       => tpd_CSNeg_oSO,
                        PathCondition   => CSNeg_ipd = '1'),
                    3 => (InputChangeTime => HOLDNegIn'LAST_EVENT,
                        PathDelay       => tpd_HOLDNeg_oSO,
                        PathCondition   => QUAD = '0')
                )
            );
        END PROCESS;

    SI_Out_PathDelay : PROCESS(SIOut_z)

            VARIABLE SI_GlitchData : VitalGlitchDataType;
        BEGIN
            VitalPathDelay01Z (
                OutSignal       => SIOut,
                OutSignalName   => "oSI",
                OutTemp         => SIOut_z,
                Mode            => VitalTransport,
                GlitchData      => SI_GlitchData,
                Paths           => (
                    0 => (InputChangeTime => SCK_ipd'LAST_EVENT,
                        PathDelay => VitalExtendtofillDelay(tpd_SCK_oSO_normal),
                        PathCondition => dual AND NOT(ddr)),
                    1 => (InputChangeTime => SCK_ipd'LAST_EVENT,
                        PathDelay   => VitalExtendtofillDelay(tpd_SCK_oSO_DDR),
                        PathCondition   => dual AND ddr),
                    2 => (InputChangeTime => CSNeg_ipd'LAST_EVENT,
                        PathDelay       => tpd_CSNeg_oSO,
                        PathCondition   => CSNeg_ipd = '1'),
                    3 => (InputChangeTime => HOLDNegIn'LAST_EVENT,
                        PathDelay       => tpd_HOLDNeg_oSO,
                        PathCondition   => dual AND QUAD = '0')
                )
            );
        END PROCESS;

    HOLD_Out_PathDelay : PROCESS(HOLDNegOut_zd)

            VARIABLE HOLD_GlitchData : VitalGlitchDataType;
        BEGIN
            VitalPathDelay01Z (
                OutSignal       => HOLDNegOut,
                OutSignalName   => "oHOLDNeg",
                OutTemp         => HOLDNegOut_zd,
                Mode            => VitalTransport,
                GlitchData      => HOLD_GlitchData,
                Paths           => (
                    0 => (InputChangeTime => SCK_ipd'LAST_EVENT,
                        PathDelay => VitalExtendtofillDelay(tpd_SCK_oSO_normal),
                        PathCondition   => dual AND not(ddr) AND QUAD = '1'),
                    1 => (InputChangeTime => SCK_ipd'LAST_EVENT,
                        PathDelay => VitalExtendtofillDelay(tpd_SCK_oSO_DDR),
                        PathCondition   => ddr AND QUAD = '1'),
                    2 => (InputChangeTime => CSNeg_ipd'LAST_EVENT,
                        PathDelay       => tpd_CSNeg_oSO,
                        PathCondition   => CSNeg_ipd = '1' AND
                                           HOLDNegOut_zd = 'Z' AND QUAD = '1')
                )
            );
        END PROCESS;

    WP_Out_PathDelay : PROCESS(WPNegOut_zd)

            VARIABLE WP_GlitchData : VitalGlitchDataType;
        BEGIN
            VitalPathDelay01Z (
                OutSignal       => WPNegOut,
                OutSignalName   => "oWPNeg",
                OutTemp         => WPNegOut_zd,
                Mode            => VitalTransport,
                GlitchData      => WP_GlitchData,
                Paths           => (
                    0 => (InputChangeTime => SCK_ipd'LAST_EVENT,
                        PathDelay => VitalExtendtofillDelay(tpd_SCK_oSO_normal),
                        PathCondition   => dual AND not(ddr) AND QUAD = '1'),
                    1 => (InputChangeTime => SCK_ipd'LAST_EVENT,
                        PathDelay => VitalExtendtofillDelay(tpd_SCK_oSO_DDR),
                        PathCondition   => ddr AND QUAD = '1'),
                    2 => (InputChangeTime => CSNeg_ipd'LAST_EVENT,
                        PathDelay       => tpd_CSNeg_oSO,
                        PathCondition   => CSNeg_ipd = '1' AND
                                           WPNegOut_zd = 'Z' AND QUAD = '1')
                )
            );
        END PROCESS;

    END BLOCK behavior;
END vhdl_behavioral_static_memory_allocation;
