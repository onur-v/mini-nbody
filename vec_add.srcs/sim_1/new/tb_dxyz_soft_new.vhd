----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 05/27/20fractional 01:53:39 PM
-- Design Name: 
-- Module Name: tb_dxyz_soft - Behavioral
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

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

entity tb_dxyz_soft is
end tb_dxyz_soft;

architecture tb of tb_dxyz_soft is

  -----------------------------------------------------------------------
  -- Timing constants
  -----------------------------------------------------------------------

  constant float_width : integer := 32;
  constant fractional : integer := 24;

  constant add_latency : integer := 11;
  constant mult_latency : integer := 8;
  constant fma_latency : integer := 16;
  constant total_latency : integer := 2*add_latency + maximum(mult_latency + add_latency, fma_latency);

  constant CLOCK_PERIOD : time := 100 ns;
  constant T_HOLD       : time := 10 ns;
  constant T_STROBE     : time := CLOCK_PERIOD - (1 ns);
  constant DUT_DELAY    : time := CLOCK_PERIOD * total_latency;

  -----------------------------------------------------------------------
  -- Testbench types and signals
  -----------------------------------------------------------------------

  -- Overall simulation phase control enumerated type and signal
  type sim_phase_t is (phase_init,           -- test initialization, run no operations
                       phase_single,         -- run a single operation, and wait for the result
                       phase_consecutive,    -- run a consecutive series of 100 operations with incrementing data
                       phase_axi_handshake,  -- demonstrate the use and effect of AXI handshaking signals
                       phase_special         -- demonstrate the use of special floating-point values
                       );
  signal sim_phase : sim_phase_t := phase_init;

  -- Enumerated type to indicate special floating point values.
  -- Note: denormalized numbers are treated as zero, and signalling NaNs are treated as quiet NaNs.
  -- Out Of Range just means the value is larger than VHDL can support in a Real type
  type floating_point_special_t is (normal, zero_pos, zero_neg, inf_pos, inf_neg, nan, out_of_range);

  -----------------------------------------------------------------------
  -- Functions
  -----------------------------------------------------------------------

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

  -- Function to convert a real number to a std_logic_vector floating point representation
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
        -- To avoid VHDL's integer type overflowing, use at most float_width-2 bits of the mantissa
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


  -- Function to identify special numbers in a std_logic_vector floating point representation
  function flt_to_special(f : std_logic_vector;  -- floating point number to convert
                          w, fw : integer)       -- total and fractional bit width of result
    return floating_point_special_t is
    constant EW     : integer := w - fw;
    constant ZEROS  : std_logic_vector(w-1 downto 0)    := (others => '0');
    constant ONES   : std_logic_vector(w-1 downto 0)    := (others => '1');
    variable f_int  : std_logic_vector(f'high downto 0) := f;  -- fixed width version of f
    variable f_sign : std_logic                         := f_int(w-1);              -- sign bit of f
    variable f_exp  : std_logic_vector(EW-1 downto 0)   := f_int(w-2 downto fw-1);  -- exponent of f
    variable f_mant : std_logic_vector(fw-2 downto 0)   := f_int(fw-2 downto 0);    -- mantissa of f
    variable result : floating_point_special_t;
    constant E_BIAS   : integer := 2 ** (EW - 1) - 1;
  begin

    -- Check bit widths match
    assert f'high = w - 1
      report "ERROR: flt_to_special: input std_logic_vector f must have bit width = input integer w"
      severity failure;

    -- Check for special cases
    if f_exp = ZEROS(w-2 downto fw-1) then
      -- +/- zero (treat denormalized numbers as zero)
      if f_sign = '0' then
        result := zero_pos;
      else
        result := zero_neg;
      end if;

    elsif f_exp = ONES(w-2 downto fw-1) then
      if f_mant = ZEROS(fw-2 downto 0) then
        -- +/- infinity
        if f_sign = '0' then
          result := inf_pos;
        else
          result := inf_neg;
        end if;

      else
        -- NaN (not a number)
        result := nan;
      end if;
    elsif to_integer(unsigned(f_exp)) - E_BIAS > 1022 then
      -- If the exponent is too large then we end up with a number that VHDL can't represent in a REAL.
      -- This gives fatal runtime errors.  Note that 10fractional-2 is used instead of 10fractional-1 (the actual max) because
      -- that allows us to ignore the value of the mantissa

      result := out_of_range;
    elsif to_integer(unsigned(f_exp)) - E_BIAS < -1021 then
      result := out_of_range;

    else
      -- not a special case
      result := normal;
    end if;

    return result;
  end function flt_to_special;

  -- Function to convert a std_logic_vector floating point representation into a real number
  function flt_to_real(f : std_logic_vector;  -- floating point number to convert
                       w, fw : integer)       -- total and fractional bit width of result
    return real is
    constant EW     : integer := w - fw;
    constant FW_LIM : integer := width_limit(fw);    -- limited internal value of fractional bit width
    constant E_BIAS : integer := 2 ** (EW - 1) - 1;
    constant ZEROS  : std_logic_vector(w-1 downto 0)    := (others => '0');
    variable f_int  : std_logic_vector(f'high downto 0) := f;  -- fixed width version of f
    variable f_sign : std_logic                         := f_int(w-1);              -- sign bit of f
    variable f_exp  : std_logic_vector(EW-1 downto 0)   := f_int(w-2 downto fw-1);  -- exponent of f
    variable f_mant : std_logic_vector(fw-2 downto 0)   := f_int(fw-2 downto 0);    -- mantissa of f
    variable exp    : integer;
    variable result : real;
  begin

    -- Check bit widths match
    assert f'high = w - 1
      report "ERROR: flt_to_real: input std_logic_vector f must have bit width = input integer w"
      severity failure;

    -- Check for special cases: return zero if any are found
    if flt_to_special(f, w, fw) /= normal then
      result := 0.0;
    else

      -- Convert mantissa to real
      -- To avoid VHDL's integer type overflowing, consider at most float_width-2 bits of the mantissa
      result := real(to_integer(unsigned(f_mant(fw - 2 downto fw - FW_LIM)))) / real(2 ** (FW_LIM - 1)) + 1.0;

      -- Apply exponent
      exp := to_integer(unsigned(f_exp)) - E_BIAS;
      if exp > 0 then
        result := result * (2.0 ** real(exp));
      elsif exp < 0 then
        result := result / (2.0 ** real(-exp));
      end if;

      -- Apply sign bit
      if f_sign = '1' then
        result := -result;
      end if;

    end if;

    return result;
  end function flt_to_real;


  -----------------------------------------------------------------------
  -- DUT signals
  -----------------------------------------------------------------------

  -- Global signals
  signal aclk                    : std_logic := '0';  -- the master clock

  -- input valid signal for all four signals x_*, y_*
  signal valid_in                : std_logic := '0';  -- payload is valid

  -- x_this  and x_target operand slave channel signals
  signal x_this                  : std_logic_vector(float_width-1 downto 0) := (others => '0');  -- data payload
  signal x_target                : std_logic_vector(float_width-1 downto 0) := (others => '0');  -- data payload

  -- y_this  and y_target operand slave channel signals
  signal y_this                  : std_logic_vector(float_width-1 downto 0) := (others => '0');  -- data payload
  signal y_target                : std_logic_vector(float_width-1 downto 0) := (others => '0');  -- data payload

  -- z_this  and z_target operand slave channel signals
  signal z_this                  : std_logic_vector(float_width-1 downto 0) := (others => '0');  -- data payload
  signal z_target                : std_logic_vector(float_width-1 downto 0) := (others => '0');  -- data payload

  -- Result master channel signals
  signal valid_out               : std_logic := '0'; --output is valid
  signal dist_sqr                : std_logic_vector(float_width-1 downto 0) := (others => '0');  -- data payload

  signal dxdy_in                 : std_logic_vector(2*float_width-1 downto 0) := (others => '0');
  signal dz_in                   : std_logic_vector(float_width-1 downto 0) := (others => '0');
  signal dxdydz_out              : std_logic_vector(3*float_width-1 downto 0) := (others => '0');

  -----------------------------------------------------------------------
  -- Aliases for AXI channel TDATA and TUSER fields
  -- These are a convenience for viewing data in a simulator waveform viewer.
  -- If using ModelSim or Questa, add "-voptargs=+acc=n" to the vsim command
  -- to prevent the simulator optimizing away these signals.
  -----------------------------------------------------------------------

  -- x_this operand slave channel alias signals
  signal x_this_real    : real := 0.0;  -- floating-point value using VHDL 'real' data type
  signal x_this_special : floating_point_special_t := normal;  -- indicate special values
  signal x_this_sign    : std_logic := '0';  -- sign bit
  signal x_this_exp     : std_logic_vector(float_width-fractional-1 downto 0) := (others => '0');  -- exponent (biased)
  signal x_this_mant    : std_logic_vector(fractional-2 downto 0) := (others => '0');  -- mantissa (without hidden bit)

  -- x_target operand slave channel alias signals
  signal x_target_real    : real := 0.0;  -- floating-point value using VHDL 'real' data type
  signal x_target_special : floating_point_special_t := normal;  -- indicate special values
  signal x_target_sign    : std_logic := '0';  -- sign bit
  signal x_target_exp     : std_logic_vector(float_width-fractional-1 downto 0) := (others => '0');  -- exponent (biased)
  signal x_target_mant    : std_logic_vector(fractional-2 downto 0) := (others => '0');  -- mantissa (without hidden bit)

  -- y_this operand slave channel alias signals
  signal y_this_real    : real := 0.0;  -- floating-point value using VHDL 'real' data type
  signal y_this_special : floating_point_special_t := normal;  -- indicate special values
  signal y_this_sign    : std_logic := '0';  -- sign bit
  signal y_this_exp     : std_logic_vector(float_width-fractional-1 downto 0) := (others => '0');  -- exponent (biased)
  signal y_this_mant    : std_logic_vector(fractional-2 downto 0) := (others => '0');  -- mantissa (without hidden bit)

  -- y_target operand slave channel alias signals
  signal y_target_real    : real := 0.0;  -- floating-point value using VHDL 'real' data type
  signal y_target_special : floating_point_special_t := normal;  -- indicate special values
  signal y_target_sign    : std_logic := '0';  -- sign bit
  signal y_target_exp     : std_logic_vector(float_width-fractional-1 downto 0) := (others => '0');  -- exponent (biased)
  signal y_target_mant    : std_logic_vector(fractional-2 downto 0) := (others => '0');  -- mantissa (without hidden bit)
  
  -- z_this operand slave channel alias signals
  signal z_this_real    : real := 0.0;  -- floating-point value using VHDL 'real' data type
  signal z_this_special : floating_point_special_t := normal;  -- indicate special values
  signal z_this_sign    : std_logic := '0';  -- sign bit
  signal z_this_exp     : std_logic_vector(float_width-fractional-1 downto 0) := (others => '0');  -- exponent (biased)
  signal z_this_mant    : std_logic_vector(fractional-2 downto 0) := (others => '0');  -- mantissa (without hidden bit)

  -- z_target operand slave channel alias signals
  signal z_target_real    : real := 0.0;  -- floating-point value using VHDL 'real' data type
  signal z_target_special : floating_point_special_t := normal;  -- indicate special values
  signal z_target_sign    : std_logic := '0';  -- sign bit
  signal z_target_exp     : std_logic_vector(float_width-fractional-1 downto 0) := (others => '0');  -- exponent (biased)
  signal z_target_mant    : std_logic_vector(fractional-2 downto 0) := (others => '0');  -- mantissa (without hidden bit)

  -- dist_sqr operand slave channel alias signals
  signal dist_sqr_real    : real := 0.0;  -- floating-point value using VHDL 'real' data type
  signal dist_sqr_special : floating_point_special_t := normal;  -- indicate special values
  signal dist_sqr_sign    : std_logic := '0';  -- sign bit
  signal dist_sqr_exp     : std_logic_vector(float_width-fractional-1 downto 0) := (others => '0');  -- exponent (biased)
  signal dist_sqr_mant    : std_logic_vector(fractional-2 downto 0) := (others => '0');  -- mantissa (without hidden bit)

begin

  -----------------------------------------------------------------------
  -- Instantiate the DUT
  -----------------------------------------------------------------------

  dut : entity work.dxyz_soft
    generic map(
      float_width             => float_width,
      fractional              => fractional,
      add_latency             => 11,
      mult_latency            => 8,
      fma_latency             => 16)
    port map (
      aclk                    => aclk,
      valid_in                => valid_in,
      x_this                  => x_this,
      x_target                => x_target,
      y_this                  => y_this,
      y_target                => y_target,
      z_this                  => z_this,
      z_target                => z_target,
      dxdy_in                 => dxdy_in,
      dz_in                   => dz_in,
      valid_out               => valid_out,
      dist_sqr                => dist_sqr,
      dxdydz_out                 => dxdydz_out);


  -----------------------------------------------------------------------
  -- Generate clock
  -----------------------------------------------------------------------

  clock_gen : process
  begin
    aclk <= '0';
    wait for CLOCK_PERIOD;
    loop
      aclk <= '0';
      wait for CLOCK_PERIOD/2;
      aclk <= '1';
      wait for CLOCK_PERIOD/2;
    end loop;
  end process clock_gen;


  -----------------------------------------------------------------------
  -- Simulation control
  -- Run a series of demonstrations, each in a separate test phase
  -- This process controls all other stimuli processes
  -----------------------------------------------------------------------

  sim_control : process
  begin

    -- Drive simulation control synchronous to the rising edge of the clock
    wait until rising_edge(aclk);

    -- Run a single operation, and wait for the result
    sim_phase <= phase_single;

    wait for 5 * CLOCK_PERIOD + DUT_DELAY;
    -- Run a consecutive series of 100 operations with incrementing data and wait for all results
    sim_phase <= phase_consecutive;
    wait for 105 * CLOCK_PERIOD + DUT_DELAY;


    -- Run the same consecutive series of 100 operations, while demonstrating use and effect of AXI handshaking signals
    sim_phase <= phase_axi_handshake;
    wait for 127 * CLOCK_PERIOD;


    -- Run operations that demonstrate the use of special floating-point values (+/- zero, +/- infinity, Not a Number)
    sim_phase <= phase_special;
    wait for 14 * CLOCK_PERIOD;
    -- Allow operations in progress to complete and the results to be produced
    wait for DUT_DELAY;

    -- End of simulation
    report "Not a real failure. Simulation finished successfully. Test completed successfully" severity failure;
    wait;

  end process sim_control;


  stimuli_all : process

  procedure drive_all_single(tdata1, tdata2, tdata3, tdata4, tdata5, tdata6 : std_logic_vector(float_width-1 downto 0);
                             variable abort : out boolean) is
  begin
      valid_in <= '1';
      x_this <= tdata1;
      x_target <= tdata2;
      y_this <= tdata3;
      y_target <= tdata4;
      y_this <= tdata5;
      z_target <= tdata6;
      abort := false;
      wait until rising_edge(aclk);
      wait for T_HOLD;
      valid_in <= '0'
  end procedure drive_all_single;

  procedure drive_all(data1, data2, data3, data4, data5, data6 : real;
                      special : floating_point_special_t;
                      count   : positive := 1;
                      step    : real := 0.0) is
  
    variable value1    : real := data1;
    variable value2    : real := data2;
    variable value3    : real := data3;
    variable value4    : real := data4;
    variable value5    : real := data5;
    variable value6    : real := data6;
    variable value_slv1, value_slv2, value_slv3, value_slv4, value_slv5, value_slv6: std_logic_vector(float_width-1 downto 0);
    variable tdata1, tdata2, tdata3, tdata4, tdata5, tdata6 : std_logic_vector(float_width-1 downto 0);
    variable ip_count  : natural := 0;
    variable abort     : boolean;
  begin
      count_loop : loop
        -- Convert data from real to std_logic_vector
        value_slv1 := real_to_flt(value1, special, float_width, fractional);
        tdata1 := value_slv;
        value_slv2 := real_to_flt(value2, special, float_width, fractional);
        tdata2 := value_slv;
        value_slv3 := real_to_flt(value3, special, float_width, fractional);
        tdata3 := value_slv;
        value_slv4 := real_to_flt(value4, special, float_width, fractional);
        tdata4 := value_slv;
        value_slv5 := real_to_flt(value5, special, float_width, fractional);
        tdata5 := value_slv;
        value_slv6 := real_to_flt(value6, special, float_width, fractional);
        tdata6 := value_slv;
        -- Drive AXI transaction
        drive_all_single(tdata => tdata,
                       abort => abort);

        ip_count := ip_count + 1;
        exit count_loop when ip_count >= count;
        -- Increment data for next iteration
        value1 := value1 + step;
        value3 := value3 + step;
        value5 := value5 + step;
      end loop count_loop;
  end procedure drive_all;

  variable tdata : std_logic_vector(float_width-1 downto 0) := (others => '0');
  variable abort : boolean;

  begin

    -- Wait for simulation control to signal the first phase
    wait until sim_phase = phase_single;
    wait for T_HOLD;  -- drive inputs T_HOLD after the rising edge of the clock

    -- Run a single operation, and wait for the result
    drive_all(1.0, 0.0, 2.0, 0.0, 3.0, 0.0, normal);

    -- Wait for simulation control to signal the next phase
    wait until sim_phase = phase_consecutive;
    wait for T_HOLD;  -- drive inputs T_HOLD after the rising edge of the clock

    -- Run a consecutive series of 100 operations with incrementing data
    drive_all(1.0, 0.0, 2.0, 0.0, 3.0, 0.0, normal, 100, 2.0);

    -- Wait for simulation control to signal the next phase
    wait until sim_phase = phase_axi_handshake;
    wait for T_HOLD;  -- drive inputs T_HOLD after the rising edge of the clock

  end process stimuli_all;
  -----------------------------------------------------------------------
  -- Generate inputs on the x_this operand slave channel
  -----------------------------------------------------------------------

  -- A operand slave channel alias signals
  x_this_real    <= flt_to_real(x_this(float_width-1 downto 0), float_width, fractional);
  x_this_special <= flt_to_special(x_this(float_width-1 downto 0), float_width, fractional);
  x_this_sign    <= x_this(float_width-1);
  x_this_exp     <= x_this(float_width-2 downto fractional-1);
  x_this_mant    <= x_this(fractional-2 downto 0);
  
  -- A operand slave channel alias signals
  x_target_real    <= flt_to_real(x_target(float_width-1 downto 0), float_width, fractional);
  x_target_special <= flt_to_special(x_target(float_width-1 downto 0), float_width, fractional);
  x_target_sign    <= x_target(float_width-1);
  x_target_exp     <= x_target(float_width-2 downto fractional-1);
  x_target_mant    <= x_target(fractional-2 downto 0);

  -- B operand slave channel alias signals
  y_this_real    <= flt_to_real(y_this(float_width-1 downto 0), float_width, fractional);
  y_this_special <= flt_to_special(y_this(float_width-1 downto 0), float_width, fractional);
  y_this_sign    <= y_this(float_width-1);
  y_this_exp     <= y_this(float_width-2 downto fractional-1);
  y_this_mant    <= y_this(fractional-2 downto 0);
  
  -- B operand slave channel alias signals
  y_target_real    <= flt_to_real(y_target(float_width-1 downto 0), float_width, fractional);
  y_target_special <= flt_to_special(y_target(float_width-1 downto 0), float_width, fractional);
  y_target_sign    <= y_target(float_width-1);
  y_target_exp     <= y_target(float_width-2 downto fractional-1);
  y_target_mant    <= y_target(fractional-2 downto 0);

  -- C operand slave channel alias signals
  z_this_real    <= flt_to_real(z_this(float_width-1 downto 0), float_width, fractional);
  z_this_special <= flt_to_special(z_this(float_width-1 downto 0), float_width, fractional);
  z_this_sign    <= z_this(float_width-1);
  z_this_exp     <= z_this(float_width-2 downto fractional-1);
  z_this_mant    <= z_this(fractional-2 downto 0);
  
  -- C operand slave channel alias signals
  z_target_real    <= flt_to_real(z_target(float_width-1 downto 0), float_width, fractional);
  z_target_special <= flt_to_special(z_target(float_width-1 downto 0), float_width, fractional);
  z_target_sign    <= z_target(float_width-1);
  z_target_exp     <= z_target(float_width-2 downto fractional-1);
  z_target_mant    <= z_target(fractional-2 downto 0);

  -- Result master channel alias signals
  dist_sqr_real     <= flt_to_real(dist_sqr(float_width-1 downto 0), float_width, fractional) when valid_out = '1';
  dist_sqr_special  <= flt_to_special(dist_sqr(float_width-1 downto 0), float_width, fractional) when valid_out = '1';
  dist_sqr_sign     <= dist_sqr(float_width-1) when valid_out = '1';
  dist_sqr_exp      <= dist_sqr(float_width-2 downto fractional-1) when valid_out = '1';
  dist_sqr_mant     <= dist_sqr(fractional-2 downto 0) when valid_out = '1';

end tb;

