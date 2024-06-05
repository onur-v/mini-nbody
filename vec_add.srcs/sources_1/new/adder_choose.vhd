----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 06/04/2024 11:29:47 PM
-- Design Name: 
-- Module Name: adder_choose - RTL
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
use IEEE.numeric_std.ALL;

use work.subprograms_types_pkg.all;

entity adder_choose is
    generic (float_width : integer := 32;
             add_final_latency : integer := 11;
             adder_type : mock_adder := real_add;);
    Port    (aclk : in std_logic;
             valid1 : in std_logic;
             input1 : in std_logic_vector(float_width - 1 downto 0);
             valid2 : in std_logic;
             input2 : in std_logic_vector(float_width - 1 downto 0);
             valid_res : out std_logic;
             result : out std_logic_vector(float_width - 1 downto 0));
end adder_choose;

architecture RTL of adder_choose is
    
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

    signal SHR : bus_array(0 to add_final_latency)(float_width downto 0);

begin

    GEN_ADD : if adder_type = zero generate
    
        result <= (others => '0');
        valid_res <= '1';
    
    elsif adder_type = translate generate

        SHR(0)(float_width - 1 downto 0) <= input1;
        SHR(0)(float_width) <= valid1;
        result <= SHR(add_final_latency)(float_width - 1 downto 0);
        valid_res <= SHR(add_final_latency)(float_width);

        process(aclk)
        begin
            if rising_edge(aclk) then
                SHR(1 to add_final_latency) <= SHR(0 to add_final_latency - 1);
            end if ;
        end process;

    else generate

        SINGLE_ADD : add_final
        port map(aclk, valid1, input1, valid2, input2, valid_res, result);

    end generate GEN_ADD;

end RTL;
