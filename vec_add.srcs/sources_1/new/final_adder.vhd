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
             buffer_width : integer := 16;
             add_final_latency : integer := 11);
    port    (aclk : in std_logic;
             buff : in bus_array(0 to buffer_width - 1)(float_width - 1 downto 0);
             sum : out std_logic_vector(float_width - 1 downto 0));
end final_adder;

architecture RTL of final_adder is

    constant max_buff_depth : integer := 4;
    type matrix is array(0 to max_buff_depth - 1, 0 to 2**max_buff_depth - 1) of mock_adder;

    function adder_structure(buffer_width : integer) return matrix is 
        variable depth : natural := ceil_log2(buffer_width);
        variable res : matrix;
    begin
        FIRST : for j in 0 to 2**(depth - 1) - 1 loop
            if 2*j + 1 < buffer_width then
                res(depth - 1, j) := real_add;
            elsif 2*j < buffer_width then
                res(depth - 1, j) := translate;
            else
                res(depth - 1, j) := zero;
            end if ;
        end loop ; --FIRST 

        LAYER : for i in depth - 2 downto 0 loop
            INNER : for j in 0 to 2**i - 1 loop
                if res(i + 1, 2*j + 1) = translate or res(i + 1, 2*j + 1) = real_add then
                    res(i, j) := real_add;
                elsif res(i + 1, 2*j) = translate or res(i + 1, 2*j) = real_add then
                    res(i, j) := translate;
                else
                    res(i, j) := zero;
                end if ;
            end loop ; -- INNER
        end loop ; -- LAYER
        return res;
    end function adder_structure;

    component add_final
    port (aclk : in std_logic;
          s_axis_a_tdata : in std_logic_vector(31 downto 0);
          s_axis_b_tvalid : in std_logic;
          s_axis_b_tdata : in std_logic_vector(31 downto 0);
          m_axis_result_tvalid : out std_logic;
          m_axis_result_tdata : out std_logic_vector(31 downto 0));
    end component;

    --constant
    constant adder_struc : matrix := adder_structure(positive(buffer_width));
    constant log_width : integer := ceil_log2(positive(buffer_width));
    constant fit_width : integer := 2**log_width;

    signal int_signals : bus_array(0 to max_buff_depth * (2**max_buff_depth) - 1)(float_width - 1 downto 0);
    signal int_valids : std_logic_vector(0 to max_buff_depth * (2**max_buff_depth) - 1);
begin

    G_OUTER : for I in 0 to log_width - 1 generate
        G_INNER : for J in 0 to 2**I - 1 generate
           ADD_CHOOSE: entity work.adder_choose 
           generic map(float_width, add_final_latency, adder_struc(I, J))
           port map(aclk, 
                    int_valids((I + 1)*(2**max_buff_depth) + 2*J),
                    int_signals((I + 1)*(2**max_buff_depth) + 2*J),
                    int_valids((I + 1)*(2**max_buff_depth) + 2*J + 1),
                    int_signals((I + 1)*(2**max_buff_depth) + 2*J + 1),
                    int_valids(I*(2**max_buff_depth) + J),
                    int_signals(I*(2**max_buff_depth) + J));
        end generate G_INNER;   
    end generate G_OUTER;


end RTL;
