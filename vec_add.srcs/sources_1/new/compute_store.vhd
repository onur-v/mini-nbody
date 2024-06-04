----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 06/02/2024 06:14:49 PM
-- Design Name: 
-- Module Name: compute_store - RTL
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
use IEEE.std_logic_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

use work.subprograms_types_pkg.all;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity compute_store is
    generic(float_width : integer := 32;
            fractional : integer := 24;
            rsqrt_latency : integer := 32;
            add_latency : integer := 11;
            mult_latency : integer := 8;
            fma_latency : integer := 16;
            add_final_latency : integer := 11;
            num_blocks : integer := 5;
            log_ram_depth : positive := 14; -- ceil_log2(16384) -- total BRAM 262144 bytes, 16384 words
            log_word_width : positive := 4); -- ceil_log2(4*float_width/8) --automate this
    port   (aclk : in std_logic;
            valid_in : in std_logic;
            x_this : in  bus_array(0 to num_blocks - 1)(float_width - 1 downto 0);
            x_target : in std_logic_vector(float_width - 1 downto 0);
            y_this : in  bus_array(0 to num_blocks - 1)(float_width - 1 downto 0);
            y_target : in std_logic_vector(float_width - 1 downto 0);
            z_this : in  bus_array(0 to num_blocks - 1)(float_width - 1 downto 0);
            z_target : in std_logic_vector(float_width - 1 downto 0);
            mask : in std_logic_vector(0 to num_blocks - 1);
            write_addr : out std_logic_vector ( 31 downto 0 );
            write_din : out std_logic_vector ( 127 downto 0 );
            write_en : out std_logic;
            write_rst : out std_logic;
            write_we : out std_logic_vector ( 15 downto 0 );
            store_busy : out std_logic);
end compute_store;

architecture RTL of compute_store is

    constant add_pipeline_latency : integer := ceil_log2(positive(fma_latency)) * add_final_latency;
    
    signal gather_iter : integer range 0 to add_pipeline_latency := add_pipeline_latency;

    signal block_iter_add : integer range 0 to num_blocks - 1 := 0;
    signal dim_iter_add : integer range 0 to 2 := 0;
    signal block_iter_store : integer range 0 to num_blocks - 1 := 0;
    signal dim_iter_store : integer range 0 to 2 := 0;

    signal ZERO_PTR : std_logic_vector(log_ram_depth - 1 downto 0) := (others => '0');
    signal BASE_PTR : std_logic_vector(log_ram_depth - 1 downto 0) := (0 => '1', others => '0');
    signal STORE_PTR : std_logic_vector(log_ram_depth - 1 downto 0) := ZERO_PTR;

    signal FMA_RES : bus_array(0 to 3*fma_latency*num_blocks - 1)(float_width - 1 downto 0);
    signal FMA_RES_CUR : std_logic_vector(float_width - 1 downto 0);
    signal BUFF_SUM : std_logic_vector(float_width - 1 downto 0);
    signal FMA_BUSY : std_logic := '1';
    signal SCATTER_COMPLETE : std_logic := '0';

begin


    BLOCKS_ALL : for I in 0 to num_blocks - 1 generate

        FIRST_BLOCK: if I = 0 generate
            BLOCK_1 : entity work.fxyz
            generic map(I, float_width, fractional, rsqrt_latency, add_latency, mult_latency. fma_latency, add_final_latency)
            port map(aclk, valid_in, x_this(I), x_target, y_this(I), y_target, z_this(I), z_target, RESULTS(3*fma_latency*I to 3*fma_latency*(I + 1) - 1), FMA_BUSY, SCATTER_COMPLETE);
        end generate FIRST_BLOCK;

        REST_BLOCKS: if I > 0 generate
            BLOCK_O : entity work.fxyz
            generic map(I, float_width, fractional, rsqrt_latency, add_latency, mult_latency. fma_latency, add_final_latency)
            port map(aclk, valid_in, x_this(I), x_target, y_this(I), y_target, z_this(I), z_target, RESULTS(3*fma_latency*I to 3*fma_latency*(I + 1) - 1), open, open);
        end generate REST_BLOCKS;

    end generate BLOCKS_ALL; 

    /*
    FIN_ADDER : entity work.final_adder
        generic map()
        port map();
    */

    process(aclk)
    begin
        if rising_edge(aclk) then
            if FMA_BUSY or SCATTER_COMPLETE or WRITE_ONGOING then
                store_busy <= '1';
            else
                store_busy <= '0';
            end if ;
        end if ;
    end process;

    process(aclk)
    begin
        if rising_edge(aclk) then
            if SCATTER_COMPLETE then
                WRITE_ONGOING <= '1';
            elsif STORE_COMPLETE then
                WRITE_ONGOING <= '0';
            end if;
        end if;
    end process;
    
    process(aclk)
    begin
        if rising_edge(aclk) then
            if dim_iter_add = 2 then
                if block_iter_add = num_blocks - 1 then
                    block_iter_add <= 0;
                else
                    block_iter_add <= block_iter_add + 1;
                end if ;                
            end if ;
        end if ;
    end process;

    process(aclk)
    begin
        if rising_edge(aclk) then
            if SCATTER_COMPLETE or dim_iter_add /= 0 or block_iter_add /= 0 then
                if dim_iter_add = 2 then
                    dim_iter_add <= 0;
                else
                    dim_iter_add <= dim_iter_add + 1;
                end if ;                
            end if ;
        end if ;
    end process;

    process(aclk)
    begin
        if rising_edge(aclk) then
            if SCATTER_COMPLETE then
                gather_iter <= gather_iter - 1;
            elsif gather_iter /= 0 then
                gather_iter <= gather_iter - 1;
            else
                gather_iter <= add_pipeline_latency + 1;
            end if ;
        end if ;
    end process;

    process(aclk)
    begin
        if rising_edge(aclk) then
            if dim_iter_store = 2 then
                if block_iter_store = num_blocks - 1 then
                    block_iter_store <= 0;
                else
                    block_iter_store <= block_iter_store + 1;
                end if ;                
            end if ;
        end if ;
    end process;

    process(aclk)
    begin
        if rising_edge(aclk) then
            if gather_iter = 0 or dim_iter_store /= 0 or block_iter_store /= 0 then
                if dim_iter_store = 2 then
                    dim_iter_store <= 0;
                else
                    dim_iter_store <= dim_iter_store + 1;
                end if ;                
                STORE_COMPLETE <= 0;
            else
                STORE_COMPLETE <= 1;
            end if ;
        end if ;
    end process;

    process(aclk)
    begin
        if rising_edge(aclk) then
            if gather_iter = 0 or dim_iter_store /= 0 or block_iter_store /= 0 then
                if mask(block_iter_store) then
                    write_we <= '1';
                else
                    write_we <= '0';
                end if ;
            else
                write_we <= '0';
            end if ;
        end if ;
    end process;

    write_en <= '1';
    write_addr <= (31 downto 28 => "1011", log_ram_depth + log_word_width - 1 downto log_word_width => STORE_PTR, others => '0');

end RTL;
