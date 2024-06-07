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
library work;
use IEEE.std_logic_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use work.subprograms_types_pkg.all;
-- Begin top_level

entity top_level is
end top_level;

architecture RTL of top_level is


    constant float_width : integer := 32;
    constant fractional : integer := 24;
    constant rsqrt_latency : integer := 32;
    constant add_latency : integer := 11;
    constant mult_latency : integer := 6;
    constant fma_latency : integer := 19;
    constant add_final_latency : integer := 11;

    constant num_blocks : positive := 2;
    constant ram_depth : natural := 16384;
    constant log_ram_depth : natural := ceil_log2(ram_depth); -- ceil_log2(16384) -- total BRAM 262144 bytes
    constant log_word_width : natural := ceil_log2(float_width/2); -- ceil_log2(4*float_width(bits)/8(bits)) --automate this

    type points is array (natural range <>) of std_logic_vector(float_width - 1 downto 0);
    type machine is (waiting, block_setup, compute, store, complete); 
    signal state : machine := waiting; 

    signal READ_INT_ADDR, WRITE_INT_ADDR : std_logic_vector(log_ram_depth - 1 downto 0);
    signal BASE_PTR : unsigned(log_ram_depth - 1 downto 0) := (0 => '1', others => '0'); -- base ptr, simply 1
    signal THIS_PTR, THIS_PTR_PREV, THIS_PTR_PREV_2 : unsigned(log_ram_depth - 1 downto 0);
    signal TRGT_PTR : unsigned(log_ram_depth - 1 downto 0);
    signal WRITE_PTR, WRITE_PTR_PREV, WRITE_PTR_PREV_2 : unsigned(log_ram_depth - 1 downto 0);

    signal block_cnt, blck_cnt_prv, blck_cnt_prv_2 : natural range 0 to num_blocks + 1 := 0;
    signal target_cnt : natural range 0 to ram_depth - 1 := 0;
    signal complete_cnt : natural range 0 to 3 := 0;

    signal PL_READ_addr : std_logic_vector ( 31 downto 0 );
    signal PL_READ_clk : std_logic;
    signal PL_READ_din : std_logic_vector ( 4*float_width - 1 downto 0 ) := (others => '0'); -- write 0 when complete
    signal PL_READ_dout : std_logic_vector ( 4*float_width - 1 downto 0 );
    signal PL_READ_en : std_logic := '1';
    signal PL_READ_rst : std_logic;
    signal PL_READ_we : std_logic_vector ( float_width/2 - 1 downto 0 ) := (others => '0');
    signal PL_WRITE_addr : std_logic_vector ( 31 downto 0 );
    signal PL_WRITE_clk : std_logic;
    signal PL_WRITE_din : std_logic_vector ( 4*float_width - 1 downto 0 );
    signal PL_WRITE_dout : std_logic_vector ( 4*float_width - 1 downto 0 );
    signal PL_WRITE_en : std_logic := '1';
    signal PL_WRITE_rst : std_logic;
    signal PL_WRITE_we : std_logic_vector ( float_width/2 - 1 downto 0 ) := (others => '0');

    signal BS_ACTV : std_logic := '0';
    signal COMP_ACTV : std_logic := '0';

    signal X_THIS, Y_THIS, Z_THIS : bus_array(0 to num_blocks - 1)(float_width - 1 downto 0);
    signal WRITE_MASK : std_logic_vector(0 to num_blocks - 1);
    signal TRGT : points(0 to 2);
    signal TRGT_VALID : std_logic := '0';

    signal NUM_PTS : unsigned(log_ram_depth - 1 downto 0);
    signal BEGIN_SIGNAL : std_logic := '0';

    signal STORE_BUSY : std_logic := '0';
    signal RESET_STORE : std_logic := '0';
    signal aclk : std_logic;

component ps_pl is
    port (
      PL_READ_addr : in std_logic_vector ( 31 downto 0 );
      PL_READ_clk : in std_logic;
      PL_READ_din : in std_logic_vector ( 4*float_width - 1 downto 0 );
      PL_READ_dout : out std_logic_vector ( 4*float_width - 1 downto 0 );
      PL_READ_en : in std_logic;
      PL_READ_rst : in std_logic;
      PL_READ_we : in std_logic_vector ( float_width/2 - 1 downto 0 );
      PL_WRITE_addr : in std_logic_vector ( 31 downto 0 );
      PL_WRITE_clk : in std_logic;
      PL_WRITE_din : in std_logic_vector ( 4*float_width - 1 downto 0 );
      PL_WRITE_dout : out std_logic_vector ( 4*float_width - 1 downto 0 );
      PL_WRITE_en : in std_logic;
      PL_WRITE_rst : in std_logic;
      PL_WRITE_we : in std_logic_vector ( float_width/2 - 1 downto 0 );
      clk : out std_logic);
end component ps_pl;

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
          PL_WRITE_we,
          aclk);


    process(aclk)
    begin
        if rising_edge(aclk) then
            THIS_PTR_PREV <= THIS_PTR;
            THIS_PTR_PREV_2 <= THIS_PTR_PREV;
            blck_cnt_prv <= block_cnt;
            blck_cnt_prv_2 <= blck_cnt_prv;
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
                    end if;
                    BEGIN_SIGNAL <= PL_READ_dout(0);
                    NUM_PTS <= unsigned(PL_READ_dout(log_ram_depth + 31 downto 32));
                    PL_READ_we <= (others => '0');
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
                        X_THIS(blck_cnt_prv_2) <= PL_READ_dout(float_width - 1 downto 0);
                        Y_THIS(blck_cnt_prv_2) <= PL_READ_dout(2*float_width - 1 downto float_width);
                        Z_THIS(blck_cnt_prv_2) <= PL_READ_dout(3*float_width - 1 downto 2*float_width);
                        state <= compute;
                        TRGT_PTR <= BASE_PTR;
                        target_cnt <= 0;
                        block_cnt <= 0;
                    elsif block_cnt = num_blocks then -- skipped if num_blocks = 1 (does the compiler recognize and eliminate?)
                        if THIS_PTR_PREV_2 > NUM_PTS then
                            WRITE_MASK(blck_cnt_prv_2) <= '0';
                        else
                            WRITE_MASK(blck_cnt_prv_2) <= '1';
                        end if ;
                        X_THIS(blck_cnt_prv_2) <= PL_READ_dout(float_width - 1 downto 0);
                        Y_THIS(blck_cnt_prv_2) <= PL_READ_dout(2*float_width - 1 downto float_width);
                        Z_THIS(blck_cnt_prv_2) <= PL_READ_dout(3*float_width - 1 downto 2*float_width);
                        block_cnt <= block_cnt + 1;
                    else
                        if THIS_PTR_PREV_2 > NUM_PTS then
                            WRITE_MASK(blck_cnt_prv_2) <= '0';
                        else
                            WRITE_MASK(blck_cnt_prv_2) <= '1';
                        end if ;
                        X_THIS(blck_cnt_prv_2) <= PL_READ_dout(float_width - 1 downto 0);
                        Y_THIS(blck_cnt_prv_2) <= PL_READ_dout(2*float_width - 1 downto float_width);
                        Z_THIS(blck_cnt_prv_2) <= PL_READ_dout(3*float_width - 1 downto 2*float_width);
                        THIS_PTR <= THIS_PTR + 1;
                        READ_INT_ADDR <= std_logic_vector(THIS_PTR);
                        block_cnt <= block_cnt + 1;
                    end if ;
                when compute => --------------------------------------- COMPUTE --------------------------
                    if target_cnt = 0 then
                        READ_INT_ADDR <= std_logic_vector(TRGT_PTR);
                        TRGT_PTR <= TRGT_PTR + 1;
                        target_cnt <= target_cnt + 1;
                    elsif target_cnt = 1 then
                        READ_INT_ADDR <= std_logic_vector(TRGT_PTR);
                        TRGT_PTR <= TRGT_PTR + 1;
                        target_cnt <= target_cnt + 1;
                    elsif target_cnt = NUM_PTS then
                        TRGT(0) <= PL_READ_dout(float_width - 1 downto 0);
                        TRGT(1) <= PL_READ_dout(2*float_width - 1 downto float_width);
                        TRGT(2) <= PL_READ_dout(3*float_width - 1 downto 2*float_width);
                        TRGT_VALID <= '1';
                        target_cnt <= target_cnt + 1;
                    elsif target_cnt = NUM_PTS + 1 then
                        TRGT(0) <= PL_READ_dout(float_width - 1 downto 0);
                        TRGT(1) <= PL_READ_dout(2*float_width - 1 downto float_width);
                        TRGT(2) <= PL_READ_dout(3*float_width - 1 downto 2*float_width);
                        TRGT_VALID <= '1';
                        target_cnt <= target_cnt + 1;
                    elsif target_cnt = NUM_PTS + 2 then -- stop computation by changing valid flag
                        TRGT(0) <= PL_READ_dout(float_width - 1 downto 0);
                        TRGT(1) <= PL_READ_dout(2*float_width - 1 downto float_width);
                        TRGT(2) <= PL_READ_dout(3*float_width - 1 downto 2*float_width);
                        TRGT_VALID <= '0';
                        state <= store;
                        target_cnt <= 0;
                    else
                        TRGT_VALID <= '1';
                        TRGT_PTR <= TRGT_PTR + 1;
                    end if;
                when store => ----------------------------------------- STORE ----------------------------
                    if STORE_BUSY = '0' then
                        state <= block_setup;       
                    end if ;
                when complete => -------------------------------------- COMPLETE -------------------------
                    if complete_cnt = 0 then
                        RESET_STORE <= '1';
                        PL_READ_we <= (others => '1');
                        READ_INT_ADDR <= (others => '0');
                        complete_cnt <= complete_cnt + 1;
                    elsif complete_cnt = 3 then
                        RESET_STORE <= '0';
                        state <= waiting;
                        complete_cnt <= 0;
                    else
                        PL_READ_we <= (others => '0');
                        complete_cnt <= complete_cnt + 1;
                    end if;
            end case;
        end if;
    end process;

    PL_READ_addr <= "1010" & (27 downto log_ram_depth + log_word_width => '0') & (log_ram_depth + log_word_width - 1 downto log_word_width => READ_INT_ADDR) & (log_word_width - 1 downto 0 => '0');

    CSTORE: entity work.compute_store
            generic map(float_width, fractional, rsqrt_latency, add_latency, mult_latency, fma_latency, add_final_latency, num_blocks, log_ram_depth)
            port map(aclk, RESET_STORE, TRGT_VALID, X_THIS, TRGT(0), Y_THIS, TRGT(1), Z_THIS, TRGT(2), WRITE_MASK, PL_WRITE_addr, PL_WRITE_din, PL_WRITE_en, PL_WRITE_rst, PL_WRITE_we, STORE_BUSY);

end RTL;
