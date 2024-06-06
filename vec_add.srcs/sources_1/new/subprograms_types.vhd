library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

package subprograms_types_pkg is
    type bus_array is array(natural range <>) of std_logic_vector;
    type mock_adder is (zero, translate, real_add);
    
    function ceil_log2(input: positive) return natural;
end package subprograms_types_pkg;

package body subprograms_types_pkg is
    function ceil_log2(input: positive) return natural is
        variable result: natural := 0;
    begin
        while 2**result < input loop
            result := result + 1;
        end loop;
        return result;
    end function ceil_log2;
end package body subprograms_types_pkg;
