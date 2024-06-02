----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 05/25/2024 04:16:46 PM
-- Design Name: 
-- Module Name: dxyz_soft - Behavioral
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

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity dxyz_soft is
    generic(float_width : integer := 32;
            fractional : integer := 24;
            add_latency : integer := 11;
            mult_latency : integer := 8;
            fma_latency : integer := 16);
    port (aclk : in std_logic;
          valid_in : in std_logic;
          x_this : in std_logic_vector(float_width - 1 downto 0);
          x_target : in std_logic_vector(float_width - 1 downto 0);
          y_this : in std_logic_vector(float_width - 1 downto 0);
          y_target : in std_logic_vector(float_width - 1 downto 0);
          z_this : in std_logic_vector(float_width - 1 downto 0);
          z_target : in std_logic_vector(float_width - 1 downto 0);
          valid_out : out std_logic;
          dist_sqr : out std_logic_vector(float_width - 1 downto 0);
          dxdydz_out : out std_logic_vector(3*float_width - 1 downto 0));
end dxyz_soft;

architecture Behavioral of dxyz_soft is

component add
  port (
    aclk : in std_logic;
    s_axis_a_tvalid : in std_logic;
    s_axis_a_tdata : in std_logic_vector(31 downto 0);
    s_axis_b_tvalid : in std_logic;
    s_axis_b_tdata : in std_logic_vector(31 downto 0);
    m_axis_result_tvalid : out std_logic;
    m_axis_result_tdata : out std_logic_vector(31 downto 0) 
  );
end component;

    type shr_struct_1 is array (natural range<>) of std_logic_vector(float_width - 1 downto 0);
    type shr_struct_2 is array (natural range<>) of std_logic_vector(2*float_width - 1 downto 0);

    constant latency_diff : integer := (add_latency + mult_latency - fma_latency);
    constant shr_depth : integer := abs(latency_diff);
    constant shr_depth_dxdy : integer := maximum(-latency_diff, 0) + add_latency;
    constant shr_depth_dz : integer := maximum(latency_diff, 0) + add_latency;

    signal SHR_SUM : shr_struct_1(0 to shr_depth);
    signal SHR_VALID : std_logic_vector(0 to shr_depth);
    signal DXY_S1, DXY_S2, DZSOFT_S1, DZSOFT_S2, DIST_RSQR : std_logic_vector(float_width - 1 downto 0);
    signal INT_VALID_XY_1, INT_VALID_XY_2, INT_VALID_ZSOFT_1, INT_VALID_ZSOFT_2 : std_logic;

    signal DXDY : std_logic_vector(2*float_width - 1 downto 0);
    signal SHR_DXDY : shr_struct_2(0 to shr_depth_dxdy);  
    signal DZ : std_logic_vector(float_width - 1 downto 0);
    signal SHR_DZ : shr_struct_1(0 to shr_depth_dz);

begin

    PATH1 : entity work.dxy
        generic map(float_width, mult_latency, add_latency)
        port map(aclk, valid_in, x_this, x_target, y_this, y_target, INT_VALID_XY_1, DXY_S1, DXDY);

    PATH2 : entity work.dzsoft
        generic map(float_width, fractional, fma_latency)
        port map(aclk, valid_in, z_this, z_target, INT_VALID_ZSOFT_1, DZSOFT_S1, DZ);

    SHR_DXDY(0) <= DXDY;
    SHR_DZ(0) <= DZ;
    dxdydz_out(3*float_width - 1 downto float_width) <= SHR_DXDY(SHR_DXDY'right);
    dxdydz_out(float_width - 1 downto 0) <= SHR_DZ(SHR_DZ'right);

    G3: if shr_depth_dxdy > 0 generate
        process(aclk) 
        begin
            if rising_edge(aclk) then
                SHR_DXDY(1 to SHR_DXDY'right) <= SHR_DXDY(0 to SHR_DXDY'right - 1);
            end if;
        end process;
    end generate G3;
    
    G4: if shr_depth_dz > 0 generate
        process(aclk) 
        begin
            if rising_edge(aclk) then
                SHR_DZ(1 to SHR_DZ'right) <= SHR_DZ(0 to SHR_DZ'right - 1);
            end if;
        end process;
    end generate G4;


    -- assign shr_sum endpoints depending on which path has longer latency
    G1: if latency_diff > 0 generate
        DZSOFT_S2 <= SHR_SUM(SHR_SUM'right);
        SHR_SUM(0) <= DZSOFT_S1;
        DXY_S2 <= DXY_S1;

        INT_VALID_ZSOFT_2 <= SHR_VALID(SHR_VALID'right);
        SHR_VALID(0) <= INT_VALID_ZSOFT_1;
        INT_VALID_XY_2 <= INT_VALID_XY_1;
    else generate
        DXY_S2 <= SHR_SUM(SHR_SUM'right);
        SHR_SUM(0) <= DXY_S1;
        DZSOFT_S2 <= DZSOFT_S1;

        INT_VALID_XY_2 <= SHR_VALID(SHR_VALID'right);
        SHR_VALID(0) <= INT_VALID_XY_1;
        INT_VALID_ZSOFT_2 <= INT_VALID_ZSOFT_1;
    end generate G1;

    -- if latency difference exists, create shift register
    G2: if shr_depth > 0 generate
        process(aclk)
        begin
            if rising_edge(aclk) then
                SHR_SUM(1 to SHR_SUM'right) <= SHR_SUM(0 to SHR_SUM'right - 1);
                SHR_VALID(1 to SHR_VALID'right) <= SHR_VALID(0 to SHR_VALID'right - 1);
            end if; 
        end process;
    end generate G2;

    PATH_CONV : add
        port map(aclk, INT_VALID_XY_2, DXY_S2, INT_VALID_ZSOFT_2, DZSOFT_S2, valid_out, dist_sqr);

end Behavioral;