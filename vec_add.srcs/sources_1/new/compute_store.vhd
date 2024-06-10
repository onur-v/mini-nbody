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
library work;
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
            num_blocks : integer := 4;
            log_ram_depth : positive := 14); -- ceil_log2(16384) -- total BRAM 262144 bytes, 16384 words
    port   (aclk : in std_logic;
            reset_store : in std_logic;
            valid_in : in std_logic;
            x_this : in  bus_array(0 to num_blocks - 1)(float_width - 1 downto 0);
            x_target : in std_logic_vector(float_width - 1 downto 0);
            y_this : in  bus_array(0 to num_blocks - 1)(float_width - 1 downto 0);
            y_target : in std_logic_vector(float_width - 1 downto 0);
            z_this : in  bus_array(0 to num_blocks - 1)(float_width - 1 downto 0);
            z_target : in std_logic_vector(float_width - 1 downto 0);
            mask : in std_logic_vector(0 to num_blocks - 1);
            write_addr : out std_logic_vector ( 31 downto 0 );
            write_din : out std_logic_vector ( 4*float_width - 1 downto 0 );
            write_en : out std_logic;
            write_rst : out std_logic;
            write_we : out std_logic_vector ( float_width/2 - 1 downto 0 );
            store_busy : out std_logic;
            this_ptr : in std_logic_vector(log_ram_depth - 1 downto 0));
end compute_store;

architecture RTL of compute_store is

    constant add_pipeline_latency : integer := ceil_log2(positive(fma_latency)) * add_final_latency;
    constant log_word_width : positive := ceil_log2(float_width/2); -- originally ceil_log2(4*float_width/8)
    
    signal gather_iter : integer range 0 to add_pipeline_latency + 1 := 0;

    signal block_iter_add : integer range 0 to num_blocks - 1 := 0;
    signal dim_iter_add : integer range 0 to 2 := 0;
    signal block_iter_store : integer range 0 to num_blocks - 1 := 0;
    signal dim_iter_store : integer range 0 to 2 := 0;

    signal ZERO_PTR : unsigned(log_ram_depth - 1 downto 0) := (others => '0');
    signal STORE_PTR : unsigned(log_ram_depth - 1 downto 0) := ZERO_PTR;

    --signal FMA_RES : bus_array(0 to 3*fma_latency*num_blocks - 1)(float_width - 1 downto 0);
    signal FMA_RES : bus_matrix(0 to num_blocks - 1)(0 to 3*fma_latency - 1)(float_width - 1 downto 0);
    signal FMA_RES_CUR : bus_array(0 to fma_latency - 1)(float_width - 1 downto 0);
    signal FMA_VALID_CUR : std_logic := '0';
    signal BUFF_SUM : std_logic_vector(float_width - 1 downto 0) := (others => '0');
    signal FMA_BUSY : std_logic := '0';
    signal SCATTER_COMPLETE : std_logic := '0';
    signal WRITE_ONGOING : std_logic := '0';

    --signal FIRST_RUN : std_logic := '1';

    signal WRITE_INT_DIN : std_logic_vector(3*float_width - 1 downto 0);

begin

    BLOCKS_ALL : for I in 0 to num_blocks - 1 generate


        
        FIRST_BLOCK: if I = 0 generate
            BLOCK_1 : entity work.fxyz
            generic map(I, float_width, fractional, rsqrt_latency, add_latency, mult_latency, fma_latency)
            port map(aclk, valid_in, x_this(I), x_target, y_this(I), y_target, z_this(I), z_target, FMA_RES(I)(0 to 3*fma_latency - 1), FMA_BUSY, SCATTER_COMPLETE);
        end generate FIRST_BLOCK;

        REST_BLOCKS: if I > 0 generate
            BLOCK_R : entity work.fxyz
            generic map(I, float_width, fractional, rsqrt_latency, add_latency, mult_latency, fma_latency)
            port map(aclk, valid_in, x_this(I), x_target, y_this(I), y_target, z_this(I), z_target, FMA_RES(I)(0 to 3*fma_latency - 1), open, open);
        end generate REST_BLOCKS;
        

    end generate BLOCKS_ALL; 

    FIN_ADDER : entity work.final_adder
        generic map(float_width, fma_latency, add_final_latency)
        port map(aclk, FMA_VALID_CUR, FMA_RES_CUR, BUFF_SUM);

    process(aclk)
    begin
        if rising_edge(aclk) then
            if FMA_BUSY = '1' or SCATTER_COMPLETE = '1' or WRITE_ONGOING = '1' then
                store_busy <= '1';
            else
                store_busy <= '0';
            end if ;
        end if ;
    end process;

    process(aclk)
    begin
        if rising_edge(aclk) then
            if SCATTER_COMPLETE = '1' then
                WRITE_ONGOING <= '1';
            elsif dim_iter_store = 2 and block_iter_store = num_blocks - 1 then
                WRITE_ONGOING <= '0';
            end if;
        end if;
    end process;
    
    -- gather and add block iterator update
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

    -- gather and add dimension iterator update
    process(aclk)
    begin
        if rising_edge(aclk) then
            if SCATTER_COMPLETE = '1' or dim_iter_add /= 0 or block_iter_add /= 0 then
                if dim_iter_add = 2 then
                    FMA_RES_CUR <= FMA_RES(block_iter_add)(2*fma_latency to 3*fma_latency - 1);
                    dim_iter_add <= 0;
                elsif dim_iter_add = 1 then
                    FMA_RES_CUR <= FMA_RES(block_iter_add)(fma_latency to 2*fma_latency - 1);
                    dim_iter_add <= dim_iter_add + 1;
                else
                    FMA_RES_CUR <= FMA_RES(block_iter_add)(0 to fma_latency - 1);
                    dim_iter_add <= dim_iter_add + 1;
                end if ;                
                FMA_VALID_CUR <= '1';
            else
                FMA_VALID_CUR <= '0';
            end if ;
        end if ;
    end process;

    -- countdown between gathering -> adder pipeline -> result storage
    process(aclk)
    begin
        if rising_edge(aclk) then
            if SCATTER_COMPLETE = '1' then
                gather_iter <= gather_iter + 1;
            elsif gather_iter = 0  or gather_iter = add_pipeline_latency + 1 then
                gather_iter <= 0;
            else
                gather_iter <= gather_iter + 1;
            end if ;
        end if ;
    end process;

    -- store block iterator update
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

    -- store dimension iterator update
    process(aclk)
    begin
        if rising_edge(aclk) then
            if gather_iter = add_pipeline_latency + 1 or dim_iter_store /= 0 or block_iter_store /= 0 then
                if dim_iter_store = 2 then
                    dim_iter_store <= 0;
                else
                    dim_iter_store <= dim_iter_store + 1;
                end if ;                
                WRITE_INT_DIN((dim_iter_store + 1)*float_width - 1 downto dim_iter_store*float_width) <= BUFF_SUM;
                --WRITE_INT_DIN((dim_iter_store + 1)*float_width - 1 downto dim_iter_store*float_width) <= std_logic_vector(to_unsigned(gather_iter, 8)) & std_logic_vector(to_unsigned(dim_iter_store, 4)) & std_logic_vector(to_unsigned(block_iter_store,4)) & std_logic_vector(this_ptr(7 downto 0)) &  std_logic_vector(STORE_PTR(7 downto 0));
                
            end if ;
        end if ;
    end process;

    -- masked write enable when dimension = 2
    process(aclk)
    begin
        if rising_edge(aclk) then
            if reset_store = '1' then
                STORE_PTR <= ZERO_PTR;
                write_we <= (others => '0');
            elsif dim_iter_store = 2 then
                if mask(block_iter_store) = '1' then
                    write_we <= (others => '1');
                else
                    write_we <= (others => '0');
                end if;
                STORE_PTR <= STORE_PTR + 1;
            else
                write_we <= (others => '0');
            end if;
        end if;
    end process;

    write_en <= '1';
    write_addr <= "1010" & (27 downto log_ram_depth + log_word_width => '0') & (log_ram_depth + log_word_width - 1 downto log_word_width => std_logic_vector(STORE_PTR)) & (log_word_width - 1 downto 0 => '0');
    write_din <= (4*float_width - 1 downto 3*float_width => '0') & (3*float_width - 1 downto 0 => WRITE_INT_DIN);
    
    --WRITE_INT_DIN((dim_iter_store + 1)*float_width - 1 downto dim_iter_store*float_width) <= SCATTER_COMPLETE & (30 downto 16 + this_ptr'length => '0') & std_logic_vector(this_ptr)  & std_logic_vector(to_unsigned(gather_iter, 8)) & std_logic_vector(to_unsigned(block_iter_store,4)) & std_logic_vector(to_unsigned(dim_iter_store, 4));
    --WRITE_INT_DIN((dim_iter_store + 1)*float_width - 1 downto dim_iter_store*float_width) <= std_logic_vector(this_ptr(7 downto 0)) & std_logic_vector(STORE_PTR(7 downto 0)) & std_logic_vector(to_unsigned(gather_iter, 8)) & std_logic_vector(to_unsigned(block_iter_store,4)) & std_logic_vector(to_unsigned(dim_iter_store, 4));


end RTL;
