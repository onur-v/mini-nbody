----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 05/28/2024 12:26:08 AM
-- Design Name: 
-- Module Name: fxyz - Behavioral
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

entity fxyz is
    generic(thread_idx : integer := 0;
            float_width : integer := 32;
            fractional : integer := 24;
            rsqrt_latency : integer := 32;
            add_latency : integer := 11;
            mult_latency : integer := 8;
            fma_latency : integer := 16);
    port   (aclk : in std_logic;
            valid_in : in std_logic;
            x_this : in std_logic_vector(float_width - 1 downto 0);
            x_target : in std_logic_vector(float_width - 1 downto 0);
            y_this : in std_logic_vector(float_width - 1 downto 0);
            y_target : in std_logic_vector(float_width - 1 downto 0);
            z_this : in std_logic_vector(float_width - 1 downto 0);
            z_target : in std_logic_vector(float_width - 1 downto 0);
            results : out bus_array(0 to 3*fma_latency - 1)(float_width - 1 downto 0);
            fma_busy : out std_logic;
            scatter_complete : out std_logic);
end fxyz;

architecture RTL of fxyz is

component fma
    port (
      aclk : in std_logic;
      s_axis_a_tvalid : in std_logic;
      s_axis_a_tdata : in std_logic_vector(31 downto 0);
      s_axis_b_tvalid : in std_logic;
      s_axis_b_tdata : in std_logic_vector(31 downto 0);
      s_axis_c_tvalid : in std_logic;
      s_axis_c_tdata : in std_logic_vector(31 downto 0);
      m_axis_result_tvalid : out std_logic;
      m_axis_result_tdata : out std_logic_vector(31 downto 0) 
    );
end component;

component rsqrt
    port (
      aclk : in std_logic;
      s_axis_a_tvalid : in std_logic;
      s_axis_a_tdata : in std_logic_vector(31 downto 0);
      m_axis_result_tvalid : out std_logic;
      m_axis_result_tdata : out std_logic_vector(31 downto 0) 
    );
end component;

    constant shr_depth : integer := rsqrt_latency + 2*mult_latency;
    type shr_struct_3 is array (natural range<>) of std_logic_vector(3*float_width - 1 downto 0);

    signal SHR_DXDYDZ : shr_struct_3(0 to shr_depth);
    signal VALID_1, VALID_2, VALID_FMA, VALID_FMA_PREV : std_logic := '0';
    signal DIST_SQR, INV_DIST, INV_DIST3 : std_logic_vector(float_width - 1 downto 0);
    signal DXDYDZ : std_logic_vector(3*float_width - 1 downto 0) := (others => '0'); 
    signal VALID_FX, VALID_FY, VALID_FZ : std_logic := '0';
    signal FX_IN, FY_IN, FZ_IN : std_logic_vector(float_width - 1 downto 0);
    signal FX_OUT, FY_OUT, FZ_OUT : std_logic_vector(float_width - 1 downto 0);

    signal FLUSH_CNT : integer range 0 to fma_latency := fma_latency;
    signal SCTTR_CNT : integer range 0 to fma_latency - 1 := 0;

    signal FLUSH_ACTV : std_logic := '0';
    signal SCTTR_ACTV : std_logic := '0';
    signal ADDER_ACTV : std_logic := '0';

begin

    STAGE1 : entity work.dxyz_soft
    generic map(float_width, fractional, add_latency, mult_latency, fma_latency)
    port map(aclk, valid_in, x_this, x_target, y_this, y_target, z_this, z_target, VALID_1, DIST_SQR, DXDYDZ);

    STAGE2 : rsqrt
    port map(aclk, VALID_1, DIST_SQR, VALID_2, INV_DIST);

    STAGE3 : entity work.cube
    generic map(float_width, mult_latency)
    port map(aclk, VALID_2, INV_DIST, VALID_FMA, INV_DIST3);

    SHR_DXDYDZ(0) <= DXDYDZ;
    
    G1: if shr_depth > 0 generate
    process(aclk)
    begin
        if rising_edge(aclk) then
            SHR_DXDYDZ(1 to SHR_DXDYDZ'right) <= SHR_DXDYDZ(0 to SHR_DXDYDZ'right - 1);
        end if;
    end process;
    end generate G1;

    FORCE_X : fma
    port map(aclk, VALID_FMA, SHR_DXDYDZ(SHR_DXDYDZ'right)(3*float_width - 1 downto 2*float_width), VALID_FMA, INV_DIST3, VALID_FMA, FX_IN, VALID_FX, FX_OUT);

    FORCE_Y : fma
    port map(aclk, VALID_FMA, SHR_DXDYDZ(SHR_DXDYDZ'right)(2*float_width - 1 downto float_width), VALID_FMA, INV_DIST3, VALID_FMA, FY_IN, VALID_FZ, FY_OUT);
    
    FORCE_Z : fma
    port map(aclk, VALID_FMA, SHR_DXDYDZ(SHR_DXDYDZ'right)(float_width - 1 downto 0), VALID_FMA, INV_DIST3, VALID_FMA, FZ_IN, VALID_FZ, FZ_OUT);

    -- reset when VALID_FMA is 0, count down whenever flips back to 1
    process(aclk)
    begin
        if rising_edge(aclk) then
            if not VALID_FMA  then
                FLUSH_CNT <= fma_latency;
            elsif FLUSH_CNT /= 0 then
                FLUSH_CNT <= FLUSH_CNT - 1;
            end if;
        end if;
    end process;

    -- fma third term is 0.0 for flushing the effects of the previous pipeline when new cycle is present
    FLUSH_ACTV <= '1' when (VALID_FMA = '0') or (FLUSH_CNT /= 0) else '0';
    FX_IN <= (others => '0') when FLUSH_ACTV else FX_OUT;
    FY_IN <= (others => '0') when FLUSH_ACTV else FY_OUT;
    FZ_IN <= (others => '0') when FLUSH_ACTV else FZ_OUT;

    process(aclk)
    begin
        if rising_edge(aclk) then
            if SCTTR_ACTV then
                if SCTTR_CNT = fma_latency - 1 then
                    SCTTR_CNT <= 0;
                else
                    SCTTR_CNT <= SCTTR_CNT + 1;
                end if ;
            end if ;    
        end if ;
    end process;

    process(aclk)
    begin
        if rising_edge(aclk) then
            VALID_FMA_PREV <= VALID_FMA;
        end if ;
    end process;

    SCTTR_ACTV <= '1' when ((VALID_FMA = '0') and (VALID_FMA_PREV = '1') and SCTTR_CNT = 0) or (SCTTR_CNT /= 0) else '0';
    
    process(aclk)
    begin
        if rising_edge(aclk) then
            if SCTTR_ACTV then
                if VALID_FX then    -- since VALID_FX=VALID_FY=VALID_FZ
                    results(SCTTR_CNT) <= FX_OUT;
                    results(SCTTR_CNT + fma_latency) <= FY_OUT;
                    results(SCTTR_CNT + 2*fma_latency) <= FZ_OUT;
                else
                    results(SCTTR_CNT) <= (others => '0');
                    results(SCTTR_CNT + fma_latency) <= (others => '0');
                    results(SCTTR_CNT + 2*fma_latency) <= (others => '0');
                end if ;
            end if ;
        end if ;
    end process;


    GEN_SGN : if thread_idx = 0 generate
        process(aclk)
        begin
            if rising_edge(aclk) then
                if SCTTR_CNT = fma_latency - 1 then
                    scatter_complete <= '1';
                else
                    scatter_complete <= '0';
                end if ;
            end if ;
        end process;

        process(aclk)
        begin
            if rising_edge(aclk) then
                if SCTTR_CNT = fma_latency - 1 then
                    fma_busy <= '0';
                elsif valid_in then
                    fma_busy <= '1';
                end if ;
            end if ;
        end process;
    else generate
        scatter_complete <= '0';
        fma_busy <= '0';
    end generate GEN_SGN;
        
end RTL;
