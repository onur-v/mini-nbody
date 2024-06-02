----------------------------------------------------------------------------------
-- Company: OGEM 
-- Engineer: Onur Vardar
-- 
-- Create Date: 05/23/2024 11:12:22 PM
-- Design Name: 
-- Module Name: dXYZ - Behavioral
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
--- 
----------------------------------------------------------------------------------

library IEEE;
use IEEE.std_logic_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity dxy is
  generic (float_width : integer := 32;
           mult_latency : integer := 8;
           add_latency : integer := 11);
  Port (aclk : in std_logic;
        valid_in : in std_logic;
        x_this : in std_logic_vector(float_width - 1 downto 0);
        x_target : in std_logic_vector(float_width - 1 downto 0);
        y_this : in std_logic_vector(float_width - 1 downto 0);
        y_target : in std_logic_vector(float_width - 1 downto 0);
        valid_out : out std_logic;
        sum : out std_logic_vector(float_width - 1 downto 0);
        dxdy_out : out std_logic_vector(2*float_width - 1 downto 0));
        
end dxy;

architecture Behavioral of dxy is

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

component mult
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

component diff
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

type shr_struct is array (natural range<>) of std_logic_vector(2*float_width - 1 downto 0);

signal INT_VALID, INT_VALID_X_1, INT_VALID_X_2, INT_VALID_Y_1, INT_VALID_Y_2 : std_logic := '0';
signal DX, DX_SQ : std_logic_vector(float_width - 1 downto 0);
signal DY, DY_SQ : std_logic_vector(float_width - 1 downto 0);

signal SHR_DXDY : shr_struct(0 to add_latency + mult_latency);

begin

  X_DIFF : diff
    port map (aclk, valid_in, x_target, valid_in, x_this, INT_VALID_X_1, DX);

  Y_DIFF : diff
    port map (aclk, valid_in, y_target, valid_in, y_this, INT_VALID_Y_1, DY);

  SHR_DXDY(0)(2*float_width - 1 downto float_width) <= DX;
  SHR_DXDY(0)(float_width - 1 downto 0) <= DY;
  dxdy_out <= SHR_DXDY(SHR_DXDY'right);

  G1 : if mult_latency + add_latency > 0 generate
  process(aclk)
  begin
    if rising_edge(aclk) then
      SHR_DXDY(1 to SHR_DXDY'right) <= SHR_DXDY(0 to SHR_DXDY'right - 1);
    end if;
  end process;
  end generate G1;

  X_DIFF_SQ : mult
    port map (aclk, INT_VALID_X_1, DX, INT_VALID_X_1, DX, INT_VALID_X_2, DX_SQ);

  Y_DIFF_SQ : mult
    port map (aclk, INT_VALID_Y_1, DY, INT_VALID_Y_1, DY, INT_VALID_Y_2, DY_SQ);

  INT_VALID <= INT_VALID_X_2 and INT_VALID_Y_2;

  DIFF_SUM : add
    port map (aclk, INT_VALID, DX_SQ, INT_VALID, DY_SQ, valid_out, sum);

end Behavioral;