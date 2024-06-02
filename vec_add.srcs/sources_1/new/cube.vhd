----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 05/26/2024 11:03:37 PM
-- Design Name: 
-- Module Name: cube - Behavioral
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

entity cube is
    generic(float_width : integer := 32;
            mult_latency : integer := 8);
    port   (aclk : in std_logic;
            valid_in : in std_logic;
            inv_dist : in std_logic_vector(float_width - 1 downto 0);
            valid_out : out std_logic;
            result : out std_logic_vector(float_width - 1 downto 0));
end cube;

architecture Behavioral of cube is

component mult
    port (aclk : in std_logic;
          s_axis_a_tvalid : in std_logic;
          s_axis_a_tdata : in std_logic_vector(float_width - 1 downto 0);
          s_axis_b_tvalid : in std_logic;
          s_axis_b_tdata : in std_logic_vector(float_width - 1 downto 0);
          m_axis_result_tvalid : out std_logic;
          m_axis_result_tdata : out std_logic_vector(float_width - 1 downto 0));
end component;

type shr_struct is array (natural range<>) of std_logic_vector(float_width - 1 downto 0);

signal INV_DIST2 : std_logic_vector(float_width - 1 downto 0);
signal INT_VALID : std_logic := '0';
signal SHR_INVDIST : shr_struct(0 to mult_latency);

begin

    SHR_INVDIST(0) <= inv_dist;

    INVDIST2 : mult
        port map(aclk, valid_in, inv_dist, valid_in, inv_dist, INT_VALID, INV_DIST2);
    
    INVDIST3 : mult
        port map(aclk, INT_VALID, SHR_INVDIST(SHR_INVDIST'right), INT_VALID, INV_DIST2, valid_out, result);

    GMULT: if mult_latency > 0 generate
    process(aclk)
    begin
        if rising_edge(aclk) then
            SHR_INVDIST(1 to SHR_INVDIST'right) <= SHR_INVDIST(0 to SHR_INVDIST'right - 1);
        end if;
    end process;    
    end generate GMULT;

end Behavioral;
