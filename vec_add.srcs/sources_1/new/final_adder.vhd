----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 06/04/2024 10:27:14 PM
-- Design Name: 
-- Module Name: final_adder - RTL
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
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

use work.subprograms_types_pkg.all;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity final_adder is
    generic (float_width : integer := 32;
             buffer_width : integer := 16
             add_final_latency : integer := 11);
    port    (aclk : in std_logic;
             buff : in bus_array(0 to buffer_width - 1)(float_width - 1 downto 0)
             sum : out std_logic_vector(float_width - 1 downto 0));
end final_adder;

architecture RTL of final_adder is

    component add_final
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

    constant fit_width : integer := 2**ceil_log2(positive(buffer_width));

begin

    G : if fit_width = 16 and buffer_width = 16 generate
        
    end generate G;



end RTL;
