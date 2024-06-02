----------------------------------------------------------------------------------
-- Company: OGEM
-- Engineer: Onur Vardar
-- 
-- Create Date: 05/25/2024 03:19:27 PM
-- Design Name: 
-- Module Name: dzsoft - Behavioral
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
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity dzsoft is
    generic (float_width : integer := 32;
             fractional  : integer := 24;
             fma_latency : integer := 16);
       port (aclk : in std_logic;
             valid_in : in std_logic;
             z_this : in std_logic_vector(float_width - 1 downto 0);
             z_target : in std_logic_vector(float_width - 1 downto 0);
             valid_out : out std_logic;
             sum : out std_logic_vector(float_width - 1 downto 0);
             dz_out : out std_logic_vector(float_width - 1 downto 0));
end dzsoft;

architecture Behavioral of dzsoft is

type floating_point_special_t is (normal, zero_pos, zero_neg, inf_pos, inf_neg, nan, out_of_range);  

  -- Function to limit an integer bit width to a value for which functions can use VHDL's integer type for calculations
  function width_limit(w : integer) return integer is
    constant MAX_WIDTH : integer := float_width-2;  -- maximum width: allows 2**width to be stored in an integer variable
  begin
    if w < MAX_WIDTH then
      return w;
    else
      return MAX_WIDTH;
    end if;
  end function width_limit;

  function real_to_flt(x : real;                       -- real number to convert
                       s : floating_point_special_t;   -- indicates special values to convert (overrides x)
                       w, fw : integer)                -- total and fractional bit width of result
    return std_logic_vector is
    constant EW       : integer := w - fw;
    constant FW_LIM   : integer := width_limit(fw);    -- limited internal value of fractional bit width
    constant E_BIAS   : integer := 2 ** (EW - 1) - 1;
    constant MANT_MAX : real    := 2.0 - 1.0 / real(2 ** (FW_LIM - 1));
    variable result   : std_logic_vector(w-1 downto 0) := (others => '0');
    variable sign     : std_logic := '0';
    variable exp      : integer   := 0;
    variable mant     : real;
    variable mant_int : integer;
  begin

    -- Handle special cases
    case s is
      when zero_pos =>  -- plus zero
        result(w-1) := '0';                          -- sign bit clear
        result(w-2 downto 0) := (others => '0');     -- exponent and mantissa clear

      when zero_neg =>  -- minus zero
        result(w-1) := '1';                          -- sign bit set
        result(w-2 downto 0) := (others => '0');     -- exponent and mantissa clear

      when inf_pos =>   -- plus infinity
        result(w-1) := '0';                          -- sign bit clear
        result(w-2 downto fw-1) := (others => '1');  -- exponent
        result(fw-2 downto 0)   := (others => '0');  -- mantissa

      when inf_neg =>   -- minus infinity
        result(w-1) := '1';                          -- sign bit set
        result(w-2 downto fw-1) := (others => '1');  -- exponent
        result(fw-2 downto 0)   := (others => '0');  -- mantissa

      when nan =>       -- Not a Number
        result(w-1) := '0';                          -- sign bit
        result(w-2 downto fw-1) := (others => '1');  -- exponent
        result(fw-2)            := '1';              -- MSB of mantissa set
        result(fw-3 downto 0)   := (others => '0');  -- rest of mantissa clear

      -- NOTE: out_of_range might be possible under some circumstances, but it can be represented
      when normal | out_of_range =>
        -- Handle normal numbers

        -- Zero must be requested using s = zero_pos or s = zero_neg, not s = normal and x = 0.0
        assert x /= 0.0
          report "ERROR: real_to_flt: zero must be requested using s = zero_pos or s = zero_neg, not s = normal and x = 0.0"
          severity failure;

        -- Get sign bit
        if x < 0.0 then
          sign := '1';
          mant := -x;
        else
          sign := '0';
          mant := x;
        end if;

        -- Normalize input to calculate exponent
        while mant < 1.0 loop
          exp  := exp - 1;
          mant := mant * 2.0;
        end loop;
        while mant > MANT_MAX loop
          exp  := exp + 1;
          mant := mant / 2.0;
        end loop;

        -- Remove hidden bit and convert to std_logic_vector
        -- To avoid VHDL's integer type overflowing, use at most 30 bits of the mantissa
        mant := mant - 1.0;
        mant_int := integer(mant * real(2 ** (FW_LIM - 1)));  -- implicit round-to-nearest
        result(fw - 2 downto fw - FW_LIM) := std_logic_vector(to_unsigned(mant_int, FW_LIM - 1));

        -- Add exponent bias and convert to std_logic_vector
        exp := exp + E_BIAS;
        result(w - 2 downto fw - 1) := std_logic_vector(to_unsigned(exp, EW));

        -- Add sign bit
        result(w - 1) := sign;

    end case;

    return result;
  end function real_to_flt;

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

  type shr_struct is array (natural range<>) of std_logic_vector(float_width - 1 downto 0);

  constant SOFT : std_logic_vector(float_width - 1 downto 0) := real_to_flt(1.0E-9, normal, float_width, fractional);
  signal SOFTENING : std_logic_vector(float_width - 1 downto 0) := SOFT;
  signal INT_VALID : std_logic := '0';
  signal DZ : std_logic_vector(float_width - 1 downto 0);
  
  signal SHR_DZ : shr_struct(0 to fma_latency);

begin

    Z_DIFF : diff
    port map(aclk, valid_in, z_target, valid_in, z_this, INT_VALID, DZ);

    SHR_DZ(0) <= DZ;
    dz_out <= SHR_DZ(SHR_DZ'right);

    GZSOFT: if fma_latency > 0 generate
    process(aclk)
    begin
      if rising_edge(aclk) then
        SHR_DZ(1 to SHR_DZ'right) <= SHR_DZ(0 to SHR_DZ'right - 1);
      end if;
    end process;
    end generate GZSOFT;

    Z_DIFF_SQ_P_SOFT : fma
    port map(aclk, INT_VALID, DZ, INT_VALID, DZ, '1', SOFTENING, valid_out, sum);

end Behavioral;