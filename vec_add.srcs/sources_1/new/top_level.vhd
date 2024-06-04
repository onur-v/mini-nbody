----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 05/28/2024 10:26:41 PM
-- Design Name: 
-- Module Name: top_level - RTL
-- Project Name: 
-- Target Devices: 
-- Tool Versions: 
-- Description: 
-- 
-- Dependencies: 
-- 
-- Revision:
-- Revision 0.01 - File Created
-- Additional Comments:
-- 
----------------------------------------------------------------------------------


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

package subprograms_types_pkg is
    type bus_array is array(natural range <>) of std_logic_vector;
    function ceil_log2(input: positive) return natural;
end package;

package body subprograms_types_pkg is
    function ceil_log2(input: positive) return natural is
        variable result: natural := 0;
    begin
        while 2**result < input loop
            result := result + 1;
        end loop;
        return result;
    end function ceil_log2;
end package body;

-- Begin top_level

entity top_level is
    port   (aclk : in std_logic);
end top_level;

architecture RTL of top_level is

component ps_pl is
    port (
      PL_READ_addr : in STD_LOGIC_VECTOR ( 31 downto 0 );
      PL_READ_clk : in STD_LOGIC;
      PL_READ_din : in STD_LOGIC_VECTOR ( 127 downto 0 );
      PL_READ_dout : out STD_LOGIC_VECTOR ( 127 downto 0 );
      PL_READ_en : in STD_LOGIC;
      PL_READ_rst : in STD_LOGIC;
      PL_READ_we : in STD_LOGIC_VECTOR ( 15 downto 0 );
      PL_WRITE_addr : in STD_LOGIC_VECTOR ( 31 downto 0 );
      PL_WRITE_clk : in STD_LOGIC;
      PL_WRITE_din : in STD_LOGIC_VECTOR ( 127 downto 0 );
      PL_WRITE_dout : out STD_LOGIC_VECTOR ( 127 downto 0 );
      PL_WRITE_en : in STD_LOGIC;
      PL_WRITE_rst : in STD_LOGIC;
      PL_WRITE_we : in STD_LOGIC_VECTOR ( 15 downto 0 )
    );
end component ps_pl;

    constant float_width : integer := 32;
    constant fractional : integer := 24;
    constant rsqrt_latency : integer := 32;
    constant add_latency : integer := 11;
    constant mult_latency : integer := 8;
    constant fma_latency : integer := 16;

    constant num_blocks : positive := 10;
    constant log_ram_depth : positive := 14; -- ceil_log2(16384) -- total BRAM 262144 bytes
    constant log_word_width : positive := 4; -- ceil_log2(4*float_width(bits)/8(bits)) --automate this

    type points is array (natural range <>) of std_logic_vector(float_width - 1 downto 0);
    type machine is (waiting, block_setup, compute, store, complete); 
    signal state : machine := waiting; 

    signal READ_INT_ADDR, WRITE_INT_ADDR : unsigned(log_ram_depth - 1 downto 0);
    signal BASE_PTR : unsigned(log_ram_depth - 1 downto 0) := (0 => '1', others => '0'); -- base ptr, simply 1
    signal THIS_PTR, THIS_PTR_PREV, THIS_PTR_PREV_2 : unsigned(log_ram_depth - 1 downto 0);
    signal TRGT_PTR, TRGT_PTR_PREV, TRGT_PTR_PREV_2 : unsigned(log_ram_depth - 1 downto 0);
    signal WRITE_PTR, WRITE_PTR_PREV, WRITE_PTR_PREV_2 : unsigned(log_ram_depth - 1 downto 0);

    signal block_cnt, blck_cnt_prv, blck_cnt_prv_2 : natural range 0 to num_blocks + 1 := 0;
    signal target_cnt : unsigned(log_ram_depth - 1 downto 0) := 0;

    signal PL_READ_addr : STD_LOGIC_VECTOR ( 31 downto 0 );
    signal PL_READ_clk : STD_LOGIC;
    signal PL_READ_din : STD_LOGIC_VECTOR ( 127 downto 0 );
    signal PL_READ_dout : STD_LOGIC_VECTOR ( 127 downto 0 );
    signal PL_READ_en : STD_LOGIC;
    signal PL_READ_rst : STD_LOGIC;
    signal PL_READ_we : STD_LOGIC_VECTOR ( 15 downto 0 ) := (others => '0');
    signal PL_WRITE_addr : STD_LOGIC_VECTOR ( 31 downto 0 );
    signal PL_WRITE_clk : STD_LOGIC;
    signal PL_WRITE_din : STD_LOGIC_VECTOR ( 127 downto 0 );
    signal PL_WRITE_dout : STD_LOGIC_VECTOR ( 127 downto 0 );
    signal PL_WRITE_en : STD_LOGIC;
    signal PL_WRITE_rst : STD_LOGIC;
    signal PL_WRITE_we : STD_LOGIC_VECTOR ( 15 downto 0 );

    signal BS_ACTV : std_logic := '0';
    signal COMP_ACTV : std_logic := '0';

    signal THIS : points(0 to 3*num_blocks - 1);
    signal WRITE_MASK : std_logic_vector(0 to num_blocks - 1);
    signal TRGT : points(0 to 2);

    signal NUM_PTS : unsigned(log_ram_depth - 1 downto 0);
    signal BEGIN_SIGNAL : std_logic := '0';
    signal BUFF : std_logic_vector(127 downto 0);
    signal P1_WR_EN, P2_WR_EN : std_logic := '0';

begin

    URAM : ps_pl
    port map (PL_READ_addr,
          aclk, 
          PL_READ_din, 
          PL_READ_dout,
          PL_READ_en, --set to '1' ?
          PL_READ_rst,
          PL_READ_we,
          PL_WRITE_addr,
          aclk,
          PL_WRITE_din,
          PL_WRITE_dout,
          PL_WRITE_en, -- set to '1'?
          PL_WRITE_rst,
          PL_WRITE_we);

    process(aclk)
    begin
        if rising_edge(aclk) then
            THIS_PTR_PREV <= THIS_PTR;
            THIS_PTR_PREV_2 <= THIS_PTR_PREV;
            TRGT_PTR_PREV <= TRGT_PTR;
            TRGT_PTR_PREV_2 <= TRGT_PTR_PREV;
            blck_cnt_prv <= block_cnt;
            blck_cnt_prv_2 <= blck_cnt_prv;
        end if ;
    end process;

    process(aclk)
    begin
        if rising_edge(aclk) then
            if state = waiting then
                BEGIN_SIGNAL <= PL_READ_dout(0);
                NUM_PTS <= unsigned(PL_READ_dout(log_ram_depth + 31 downto 32));
            end if;
        end if ;
    end process;

    process(aclk)
    begin
        if rising_edge(aclk) then
            case(state) is
                when waiting => ---------------------------------------- WAITING -------------------------
                    if BEGIN_SIGNAL then
                        state <= block_setup;
                        THIS_PTR <= BASE_PTR;
                        TRGT_PTR <= BASE_PTR;
                        WRITE_PTR <= BASE_PTR;
                        P1_WR_EN <= '0';
                    end if;
                when block_setup => ------------------------------------ BLOCK_SETUP ---------------------
                    if block_cnt = 0 then
                        if(THIS_PTR > NUM_PTS) then
                            state <= complete;
                            block_cnt <= 0;
                        else
                            READ_INT_ADDR <= std_logic_vector(THIS_PTR);
                            block_cnt <= block_cnt + 1;
                        end if;
                        THIS_PTR <= THIS_PTR + 1;
                    elsif block_cnt = 1 then
                        READ_INT_ADDR <= std_logic_vector(THIS_PTR);
                        THIS_PTR <= THIS_PTR + 1;
                        block_cnt <= block_cnt + 1;
                    elsif block_cnt = num_blocks + 1 then
                        if THIS_PTR_PREV_2 > NUM_PTS then
                            WRITE_MASK(blck_cnt_prv_2) <= '0';
                        else
                            WRITE_MASK(blck_cnt_prv_2) <= '1';
                        end if ;
                        THIS(3*(blck_cnt_prv_2)) <= PL_READ_dout(float_width - 1 downto 0);
                        THIS(3*(blck_cnt_prv_2) + 1) <= PL_READ_dout(2*float_width - 1 downto float_width);
                        THIS(3*(blck_cnt_prv_2) + 2) <= PL_READ_dout(3*float_width - 1 downto 2*float_width);
                        state <= compute;
                        TARGET_PTR <= BASE_PTR;
                        target_cnt <= to_unsigned(0, log_ram_depth);
                        block_cnt <= 0;
                    elsif block_cnt = num_blocks then -- skipped if num_blocks = 1 (does the compiler recognize and eliminate?)
                        if THIS_PTR_PREV_2 > NUM_PTS then
                            WRITE_MASK(blck_cnt_prv_2) <= '0';
                        else
                            WRITE_MASK(blck_cnt_prv_2) <= '1';
                        end if ;
                        THIS(3*(blck_cnt_prv_2)) <= PL_READ_dout(float_width - 1 downto 0);
                        THIS(3*(blck_cnt_prv_2) + 1) <= PL_READ_dout(2*float_width - 1 downto float_width);
                        THIS(3*(blck_cnt_prv_2) + 2) <= PL_READ_dout(3*float_width - 1 downto 2*float_width);
                        block_cnt <= block_cnt + 1;
                    else
                        if THIS_PTR_PREV_2 > NUM_PTS then
                            WRITE_MASK(blck_cnt_prv_2) <= '0';
                        else
                            WRITE_MASK(blck_cnt_prv_2) <= '1';
                        end if ;
                        THIS(3*(blck_cnt_prv_2)) <= PL_READ_dout(float_width - 1 downto 0);
                        THIS(3*(blck_cnt_prv_2) + 1) <= PL_READ_dout(2*float_width - 1 downto float_width);
                        THIS(3*(blck_cnt_prv_2) + 2) <= PL_READ_dout(3*float_width - 1 downto 2*float_width);
                        THIS_PTR <= THIS_PTR + 1;
                        READ_INT_ADDR <= std_logic_vector(THIS_PTR);
                        block_cnt <= block_cnt + 1;
                    end if ;
                when compute => --------------------------------------- COMPUTE --------------------------
                    if target_cnt = to_unsigned(0, log_ram_depth) then
                        READ_INT_ADDR <= std_logic_vector(TRGT_PTR);
                        TRGT_PTR <= TRGT_PTR + 1;
                        target_cnt <= target_cnt + 1;
                    elsif target_cnt = to_unsigned(1, log_ram_depth) then
                        READ_INT_ADDR <= std_logic_vector(TRGT_PTR);
                        TRGT_PTR <= TRGT_PTR + 1;
                        target_cnt <= target_cnt + 1;
                    elsif target_cnt = NUM_PTS then
                        target_cnt <= target_cnt + 1;
                    elsif target_cnt = NUM_PTS + 1 then
                        target_cnt <= target_cnt + 1;
                        TARGET_PTR <= BASE_PTR;
                    elsif target_cnt = NUM_PTS + 2 then -- stop computation by changing valid flag
                        state <= store;
                        TRGT_PTR <= TRGT_PTR + 1;
                        target_cnt <= (others => '0');
                    else
                        TRGT_PTR <= TRGT_PTR + 1;
                    end if;
                when store => ----------------------------------------- STORE ----------------------------
                    if STORE_COMPLETE then
                        state <= block_setup;       
                    end if ;
                when complete => -------------------------------------- COMPLETE -------------------------
            end case;
        end if;
    end process;

    PL_READ_addr <= (31 downto 28 => "1010", log_ram_depth + log_word_width - 1 downto log_word_width => READ_INT_ADDR, others => '0');

end RTL;