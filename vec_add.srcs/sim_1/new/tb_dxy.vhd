
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

entity tb_dxy is
end tb_dxy;

architecture tb of tb_dxy is

  -----------------------------------------------------------------------
  -- Timing constants
  -----------------------------------------------------------------------
  constant CLOCK_PERIOD : time := 100 ns;
  constant T_HOLD       : time := 10 ns;
  constant T_STROBE     : time := CLOCK_PERIOD - (1 ns);
  constant DUT_DELAY    : time := CLOCK_PERIOD * 30;

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
    constant MAX_WIDTH : integer := 30;  -- maximum width: allows 2**width to be stored in an integer variable
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
      -- This gives fatal runtime errors.  Note that 1022 is used instead of 1023 (the actual max) because
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
      -- To avoid VHDL's integer type overflowing, consider at most 30 bits of the mantissa
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
  signal x_this                  : std_logic_vector(31 downto 0) := (others => '0');  -- data payload
  signal x_target                : std_logic_vector(31 downto 0) := (others => '0');  -- data payload

  -- y_this  and y_target operand slave channel signals
  signal y_this                  : std_logic_vector(31 downto 0) := (others => '0');  -- data payload
  signal y_target                : std_logic_vector(31 downto 0) := (others => '0');  -- data payload

  -- Result master channel signals
  signal valid_out               : std_logic := '0'; --output is valid
  signal sum                     : std_logic_vector(31 downto 0) := (others => '0');  -- data payload

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
  signal x_this_exp     : std_logic_vector(7 downto 0) := (others => '0');  -- exponent (biased)
  signal x_this_mant    : std_logic_vector(22 downto 0) := (others => '0');  -- mantissa (without hidden bit)

  -- x_target operand slave channel alias signals
  signal x_target_real    : real := 0.0;  -- floating-point value using VHDL 'real' data type
  signal x_target_special : floating_point_special_t := normal;  -- indicate special values
  signal x_target_sign    : std_logic := '0';  -- sign bit
  signal x_target_exp     : std_logic_vector(7 downto 0) := (others => '0');  -- exponent (biased)
  signal x_target_mant    : std_logic_vector(22 downto 0) := (others => '0');  -- mantissa (without hidden bit)

  -- y_this operand slave channel alias signals
  signal y_this_real    : real := 0.0;  -- floating-point value using VHDL 'real' data type
  signal y_this_special : floating_point_special_t := normal;  -- indicate special values
  signal y_this_sign    : std_logic := '0';  -- sign bit
  signal y_this_exp     : std_logic_vector(7 downto 0) := (others => '0');  -- exponent (biased)
  signal y_this_mant    : std_logic_vector(22 downto 0) := (others => '0');  -- mantissa (without hidden bit)

  -- y_target operand slave channel alias signals
  signal y_target_real    : real := 0.0;  -- floating-point value using VHDL 'real' data type
  signal y_target_special : floating_point_special_t := normal;  -- indicate special values
  signal y_target_sign    : std_logic := '0';  -- sign bit
  signal y_target_exp     : std_logic_vector(7 downto 0) := (others => '0');  -- exponent (biased)
  signal y_target_mant    : std_logic_vector(22 downto 0) := (others => '0');  -- mantissa (without hidden bit)

  -- sum operand slave channel alias signals
  signal sum_real    : real := 0.0;  -- floating-point value using VHDL 'real' data type
  signal sum_special : floating_point_special_t := normal;  -- indicate special values
  signal sum_sign    : std_logic := '0';  -- sign bit
  signal sum_exp     : std_logic_vector(7 downto 0) := (others => '0');  -- exponent (biased)
  signal sum_mant    : std_logic_vector(22 downto 0) := (others => '0');  -- mantissa (without hidden bit)

begin

  -----------------------------------------------------------------------
  -- Instantiate the DUT
  -----------------------------------------------------------------------

  dut : entity work.dxy
    port map (
      -- Global signals
      aclk                    => aclk,
      valid_in                => valid_in,
      x_this                  => x_this,
      y_this                  => y_this,
      x_target                => x_target,
      y_target                => y_target,
      valid_out               => valid_out,
      sum                     => sum
      );


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

  -----------------------------------------------------------------------
  -- Generate inputs on the x_this operand slave channel
  -----------------------------------------------------------------------

  stimuli_x_this : process

    -- Procedure to drive a single transaction on the A channel
    procedure drive_x_this_single(tdata : std_logic_vector(31 downto 0);
                             variable abort : out boolean) is
    begin
      -- Drive AXI signals
      valid_in <= '1';
      x_this  <= tdata;
      abort := false;
      wait until rising_edge(aclk);
      wait for T_HOLD;
      valid_in <= '0';
    end procedure drive_x_this_single;

    -- Procedure to drive a series of transactions with incrementing data values on the A channel.
    -- data is the data value for the first transaction
    -- special indicates special floating-point values, and overrides data
    -- count is the number of transactions to drive
    -- step is the increment to add to the data value on each successive transaction
    procedure drive_x_this(data    : real;
                      special : floating_point_special_t;
                      count   : positive := 1;
                      step    : real     := 0.0) is
      variable value     : real := data;
      variable value_slv : std_logic_vector(31 downto 0);
      variable tdata     : std_logic_vector(31 downto 0);
      variable ip_count  : natural := 0;
      variable abort     : boolean;
    begin
      count_loop : loop
        -- Convert data from real to std_logic_vector
        value_slv := real_to_flt(value, special, 32, 24);
        -- Set up AXI signals
        tdata := value_slv;
        -- Drive AXI transaction
        drive_x_this_single(tdata => tdata,
                       abort => abort);

        ip_count := ip_count + 1;
        exit count_loop when ip_count >= count;
        -- Increment data for next iteration
        value := value + step;
      end loop count_loop;
    end procedure drive_x_this;

    variable tdata : std_logic_vector(31 downto 0) := (others => '0');
    variable abort : boolean;

  begin

    -- Wait for simulation control to signal the first phase
    wait until sim_phase = phase_single;
    wait for T_HOLD;  -- drive inputs T_HOLD after the rising edge of the clock

    -- Run a single operation, and wait for the result
    drive_x_this(2.0, normal);

    -- Wait for simulation control to signal the next phase
    wait until sim_phase = phase_consecutive;
    wait for T_HOLD;  -- drive inputs T_HOLD after the rising edge of the clock


    -- Run a consecutive series of 100 operations with incrementing data
    drive_x_this(1.0, normal, 100, 2.0);

    -- Wait for simulation control to signal the next phase
    wait until sim_phase = phase_axi_handshake;
    wait for T_HOLD;  -- drive inputs T_HOLD after the rising edge of the clock

    /*
    -- Run the same consecutive series of 100 operations, while demonstrating use and effect of AXI handshaking signals
    -- 5 normal consecutive transactions
    drive_x_this(1.0, normal, 5, 2.0);
    -- No transactions for 5 clock cycles
    wait for 5 * CLOCK_PERIOD;
    -- 25 normal consecutive transactions
    drive_x_this(11.0, normal, 25, 2.0);
    -- No transactions for 15 clock cycles
    wait for 15 * CLOCK_PERIOD;
    -- 20 normal consecutive transactions
    drive_x_this(41.0, normal, 20, 1.0);
    -- 50 normal consecutive transactions
    drive_x_this(91.0, normal, 50, 1.0);

    -- Wait for simulation control to signal the next phase
    wait until sim_phase = phase_special;
    wait for T_HOLD;  -- drive inputs T_HOLD after the rising edge of the clock

    -- Run operations that demonstrate the use of special floating-point values (+/- zero, +/- infinity, Not a Number)
    -- plus zero * 2 : result = plus zero
    drive_x_this(0.0, zero_pos);
    -- minus zero * 2 : result = minus zero
    drive_x_this(0.0, zero_neg);
    -- plus zero * minus zero : result = minus zero
    drive_x_this(0.0, zero_pos);
    -- very small number * -(very small number) : underflow, result = minus zero
    tdata(31) := '0';  -- sign bit
    tdata(30 downto 23) := std_logic_vector(to_unsigned(1, 8));  -- biased exponent = smallest
    tdata(22 downto 0) := (others => '0');  -- mantissa without hidden bit = [1].0
    drive_x_this_single(tdata, abort);
    -- plus infinity * 2 : result = plus infinity
    drive_x_this(0.0, inf_pos);
    -- minus infinity * 2 : result = minus infinity
    drive_x_this(0.0, inf_neg);
    -- plus infinity * minus infinity : result = minus infinity
    drive_x_this(0.0, inf_pos);
    -- very large number * very large number : overflow, result = plus infinity
    tdata(31) := '0';  -- sign bit
    tdata(30 downto 23) := std_logic_vector(to_unsigned(254, 8));  -- biased exponent = largest
    tdata(22 downto 0) := (others => '1');  -- mantissa without hidden bit = largest
    drive_x_this_single(tdata, abort);
    -- plus zero * plus infinity : invalid operation, result = Not a Number
    drive_x_this(0.0, zero_pos);
    -- Not a Number * 2 : result = Not a Number
    drive_x_this(0.0, nan);
    */

    -- End of test
    --wait;

  end process stimuli_x_this;

  -----------------------------------------------------------------------
  -- Generate inputs on the x_target operand slave channel
  -----------------------------------------------------------------------

  stimuli_x_target : process

    -- Procedure to drive a single transaction on the A channel
    procedure drive_x_target_single(tdata : std_logic_vector(31 downto 0);
                             variable abort : out boolean) is
    begin
      -- Drive AXI signals
      x_target  <= tdata;
      abort := false;
      wait until rising_edge(aclk);
      wait for T_HOLD;
    end procedure drive_x_target_single;

    -- Procedure to drive a series of transactions with incrementing data values on the A channel.
    -- data is the data value for the first transaction
    -- special indicates special floating-point values, and overrides data
    -- count is the number of transactions to drive
    -- step is the increment to add to the data value on each successive transaction
    procedure drive_x_target(data    : real;
                      special : floating_point_special_t;
                      count   : positive := 1;
                      step    : real     := 0.0) is
      variable value     : real := data;
      variable value_slv : std_logic_vector(31 downto 0);
      variable tdata     : std_logic_vector(31 downto 0);
      variable ip_count  : natural := 0;
      variable abort     : boolean;
    begin
      count_loop : loop
        -- Convert data from real to std_logic_vector
        value_slv := real_to_flt(value, special, 32, 24);
        -- Set up AXI signals
        tdata := value_slv;
        -- Drive AXI transaction
        drive_x_target_single(tdata => tdata,
                       abort => abort);

        ip_count := ip_count + 1;
        exit count_loop when ip_count >= count;
        -- Increment data for next iteration
        value := value + step;
      end loop count_loop;
    end procedure drive_x_target;

    variable tdata : std_logic_vector(31 downto 0) := (others => '0');
    variable abort : boolean;

  begin

    -- Wait for simulation control to signal the first phase
    wait until sim_phase = phase_single;
    wait for T_HOLD;  -- drive inputs T_HOLD after the rising edge of the clock

    -- Run a single operation, and wait for the result
    drive_x_target(1.0, normal);


    -- Wait for simulation control to signal the next phase
    wait until sim_phase = phase_consecutive;
    wait for T_HOLD;  -- drive inputs T_HOLD after the rising edge of the clock


    -- Run a consecutive series of 100 operations with incrementing data
    drive_x_target(0.0, normal, 100, 1.0);

    -- Wait for simulation control to signal the next phase
    wait until sim_phase = phase_axi_handshake;
    wait for T_HOLD;  -- drive inputs T_HOLD after the rising edge of the clock

    /*
    -- Run the same consecutive series of 100 operations, while demonstrating use and effect of AXI handshaking signals
    -- 5 normal consecutive transactions
    drive_x_target(0.0, normal, 5, 1.0);
    -- No transactions for 5 clock cycles
    wait for 5 * CLOCK_PERIOD;
    -- 25 normal consecutive transactions
    drive_x_target(5.0, normal, 25, 1.0);
    -- No transactions for 15 clock cycles
    wait for 15 * CLOCK_PERIOD;
    -- 20 normal consecutive transactions
    drive_x_target(20.0, normal, 20, 1.0);
    -- 50 normal consecutive transactions
    drive_x_target(70.0, normal, 50, 1.0);

    -- Wait for simulation control to signal the next phase
    wait until sim_phase = phase_special;
    wait for T_HOLD;  -- drive inputs T_HOLD after the rising edge of the clock

    -- Run operations that demonstrate the use of special floating-point values (+/- zero, +/- infinity, Not a Number)
    -- plus zero * 2 : result = plus zero
    drive_x_target(0.0, zero_pos);
    -- minus zero * 2 : result = minus zero
    drive_x_target(0.0, zero_neg);
    -- plus zero * minus zero : result = minus zero
    drive_x_target(0.0, zero_pos);
    -- very small number * -(very small number) : underflow, result = minus zero
    tdata(31) := '0';  -- sign bit
    tdata(30 downto 23) := std_logic_vector(to_unsigned(1, 8));  -- biased exponent = smallest
    tdata(22 downto 0) := (others => '0');  -- mantissa without hidden bit = [1].0
    drive_x_target_single(tdata, abort);
    -- plus infinity * 2 : result = plus infinity
    drive_x_target(0.0, inf_pos);
    -- minus infinity * 2 : result = minus infinity
    drive_x_target(0.0, inf_neg);
    -- plus infinity * minus infinity : result = minus infinity
    drive_x_target(0.0, inf_pos);
    -- very large number * very large number : overflow, result = plus infinity
    tdata(31) := '0';  -- sign bit
    tdata(30 downto 23) := std_logic_vector(to_unsigned(254, 8));  -- biased exponent = largest
    tdata(22 downto 0) := (others => '1');  -- mantissa without hidden bit = largest
    drive_x_target_single(tdata, abort);
    -- plus zero * plus infinity : invalid operation, result = Not a Number
    drive_x_target(0.0, zero_pos);
    -- Not a Number * 2 : result = Not a Number
    drive_x_target(0.0, nan);
    */

    -- End of test
    --wait;

  end process stimuli_x_target;

  -----------------------------------------------------------------------
  -- Generate inputs on the y_this operand slave channel
  -----------------------------------------------------------------------

  stimuli_y_this : process

    -- Procedure to drive a single transaction on the A channel
    procedure drive_y_this_single(tdata : std_logic_vector(31 downto 0);
                             variable abort : out boolean) is
    begin
      -- Drive AXI signals
      y_this  <= tdata;
      abort := false;
      wait until rising_edge(aclk);
      wait for T_HOLD;
    end procedure drive_y_this_single;

    -- Procedure to drive a series of transactions with incrementing data values on the A channel.
    -- data is the data value for the first transaction
    -- special indicates special floating-point values, and overrides data
    -- count is the number of transactions to drive
    -- step is the increment to add to the data value on each successive transaction
    procedure drive_y_this(data    : real;
                      special : floating_point_special_t;
                      count   : positive := 1;
                      step    : real     := 0.0) is
      variable value     : real := data;
      variable value_slv : std_logic_vector(31 downto 0);
      variable tdata     : std_logic_vector(31 downto 0);
      variable ip_count  : natural := 0;
      variable abort     : boolean;
    begin
      count_loop : loop
        -- Convert data from real to std_logic_vector
        value_slv := real_to_flt(value, special, 32, 24);
        -- Set up AXI signals
        tdata := value_slv;
        -- Drive AXI transaction
        drive_y_this_single(tdata => tdata,
                       abort => abort);

        ip_count := ip_count + 1;
        exit count_loop when ip_count >= count;
        -- Increment data for next iteration
        value := value + step;
      end loop count_loop;
    end procedure drive_y_this;

    variable tdata : std_logic_vector(31 downto 0) := (others => '0');
    variable abort : boolean;

  begin

    -- Wait for simulation control to signal the first phase
    wait until sim_phase = phase_single;
    wait for T_HOLD;  -- drive inputs T_HOLD after the rising edge of the clock

    -- Run a single operation, and wait for the result
    drive_y_this(1.0, normal);


    -- Wait for simulation control to signal the next phase
    wait until sim_phase = phase_consecutive;
    wait for T_HOLD;  -- drive inputs T_HOLD after the rising edge of the clock


    -- Run a consecutive series of 100 operations with incrementing data
    drive_y_this(1.0, normal, 100, 0.0);

    -- Wait for simulation control to signal the next phase
    wait until sim_phase = phase_axi_handshake;
    wait for T_HOLD;  -- drive inputs T_HOLD after the rising edge of the clock

    /*
    -- Run the same consecutive series of 100 operations, while demonstrating use and effect of AXI handshaking signals
    -- 5 normal consecutive transactions
    drive_y_this(0.0, normal, 5, 0.0);
    -- No transactions for 5 clock cycles
    wait for 5 * CLOCK_PERIOD;
    -- 25 normal consecutive transactions
    drive_y_this(0.0, normal, 25, 0.0);
    -- No transactions for 15 clock cycles
    wait for 15 * CLOCK_PERIOD;
    -- 20 normal consecutive transactions
    drive_y_this(0.0, normal, 20, 0.0);
    -- 50 normal consecutive transactions
    drive_y_this(0.0, normal, 50, 1.0);

    -- Wait for simulation control to signal the next phase
    wait until sim_phase = phase_special;
    wait for T_HOLD;  -- drive inputs T_HOLD after the rising edge of the clock

    -- Run operations that demonstrate the use of special floating-point values (+/- zero, +/- infinity, Not a Number)
    -- plus zero * 2 : result = plus zero
    drive_y_this(0.0, zero_pos);
    -- minus zero * 2 : result = minus zero
    drive_y_this(0.0, zero_neg);
    -- plus zero * minus zero : result = minus zero
    drive_y_this(0.0, zero_pos);
    -- very small number * -(very small number) : underflow, result = minus zero
    tdata(31) := '0';  -- sign bit
    tdata(30 downto 23) := std_logic_vector(to_unsigned(1, 8));  -- biased exponent = smallest
    tdata(22 downto 0) := (others => '0');  -- mantissa without hidden bit = [1].0
    drive_y_this_single(tdata, abort);
    -- plus infinity * 2 : result = plus infinity
    drive_y_this(0.0, inf_pos);
    -- minus infinity * 2 : result = minus infinity
    drive_y_this(0.0, inf_neg);
    -- plus infinity * minus infinity : result = minus infinity
    drive_y_this(0.0, inf_pos);
    -- very large number * very large number : overflow, result = plus infinity
    tdata(31) := '0';  -- sign bit
    tdata(30 downto 23) := std_logic_vector(to_unsigned(254, 8));  -- biased exponent = largest
    tdata(22 downto 0) := (others => '1');  -- mantissa without hidden bit = largest
    drive_y_this_single(tdata, abort);
    -- plus zero * plus infinity : invalid operation, result = Not a Number
    drive_y_this(0.0, zero_pos);
    -- Not a Number * 2 : result = Not a Number
    drive_y_this(0.0, nan);
    */

    -- End of test
    --wait;

  end process stimuli_y_this;

  -----------------------------------------------------------------------
  -- Generate inputs on the x_target operand slave channel
  -----------------------------------------------------------------------

  stimuli_y_target : process

    -- Procedure to drive a single transaction on the A channel
    procedure drive_y_target_single(tdata : std_logic_vector(31 downto 0);
                             variable abort : out boolean) is
    begin
      -- Drive AXI signals
      y_target  <= tdata;
      abort := false;
      wait until rising_edge(aclk);
      wait for T_HOLD;
    end procedure drive_y_target_single;

    -- Procedure to drive a series of transactions with incrementing data values on the A channel.
    -- data is the data value for the first transaction
    -- special indicates special floating-point values, and overrides data
    -- count is the number of transactions to drive
    -- step is the increment to add to the data value on each successive transaction
    procedure drive_y_target(data    : real;
                      special : floating_point_special_t;
                      count   : positive := 1;
                      step    : real     := 0.0) is
      variable value     : real := data;
      variable value_slv : std_logic_vector(31 downto 0);
      variable tdata     : std_logic_vector(31 downto 0);
      variable ip_count  : natural := 0;
      variable abort     : boolean;
    begin
      count_loop : loop
        -- Convert data from real to std_logic_vector
        value_slv := real_to_flt(value, special, 32, 24);
        -- Set up AXI signals
        tdata := value_slv;
        -- Drive AXI transaction
        drive_y_target_single(tdata => tdata,
                       abort => abort);

        ip_count := ip_count + 1;
        exit count_loop when ip_count >= count;
        -- Increment data for next iteration
        value := value + step;
      end loop count_loop;
    end procedure drive_y_target;

    variable tdata : std_logic_vector(31 downto 0) := (others => '0');
    variable abort : boolean;

  begin

    -- Wait for simulation control to signal the first phase
    wait until sim_phase = phase_single;
    wait for T_HOLD;  -- drive inputs T_HOLD after the rising edge of the clock

    -- Run a single operation, and wait for the result
    drive_y_target(1.0, normal);


    -- Wait for simulation control to signal the next phase
    wait until sim_phase = phase_consecutive;
    wait for T_HOLD;  -- drive inputs T_HOLD after the rising edge of the clock


    -- Run a consecutive series of 100 operations with incrementing data
    drive_y_target(1.0, normal, 100, 0.0);

    -- Wait for simulation control to signal the next phase
    wait until sim_phase = phase_axi_handshake;
    wait for T_HOLD;  -- drive inputs T_HOLD after the rising edge of the clock

    /*
    -- Run the same consecutive series of 100 operations, while demonstrating use and effect of AXI handshaking signals
    -- 5 normal consecutive transactions
    drive_y_target(0.1, normal, 5, 0.1);
    -- No transactions for 5 clock cycles
    wait for 5 * CLOCK_PERIOD;
    -- 25 normal consecutive transactions
    drive_y_target(0.6, normal, 25, 0.1);
    -- No transactions for 15 clock cycles
    wait for 15 * CLOCK_PERIOD;
    -- 20 normal consecutive transactions
    drive_y_target(3.1, normal, 20, 0.1);
    -- 50 normal consecutive transactions
    drive_y_target(5.1, normal, 50, 0.1);

    -- Wait for simulation control to signal the next phase
    wait until sim_phase = phase_special;
    wait for T_HOLD;  -- drive inputs T_HOLD after the rising edge of the clock

    -- Run operations that demonstrate the use of special floating-point values (+/- zero, +/- infinity, Not a Number)
    -- plus zero * 2 : result = plus zero
    drive_y_target(0.0, zero_pos);
    -- minus zero * 2 : result = minus zero
    drive_y_target(0.0, zero_neg);
    -- plus zero * minus zero : result = minus zero
    drive_y_target(0.0, zero_pos);
    -- very small number * -(very small number) : underflow, result = minus zero
    tdata(31) := '0';  -- sign bit
    tdata(30 downto 23) := std_logic_vector(to_unsigned(1, 8));  -- biased exponent = smallest
    tdata(22 downto 0) := (others => '0');  -- mantissa without hidden bit = [1].0
    drive_y_target_single(tdata, abort);
    -- plus infinity * 2 : result = plus infinity
    drive_y_target(0.0, inf_pos);
    -- minus infinity * 2 : result = minus infinity
    drive_y_target(0.0, inf_neg);
    -- plus infinity * minus infinity : result = minus infinity
    drive_y_target(0.0, inf_pos);
    -- very large number * very large number : overflow, result = plus infinity
    tdata(31) := '0';  -- sign bit
    tdata(30 downto 23) := std_logic_vector(to_unsigned(254, 8));  -- biased exponent = largest
    tdata(22 downto 0) := (others => '1');  -- mantissa without hidden bit = largest
    drive_y_target_single(tdata, abort);
    -- plus zero * plus infinity : invalid operation, result = Not a Number
    drive_y_target(0.0, zero_pos);
    -- Not a Number * 2 : result = Not a Number
    drive_y_target(0.0, nan);
    */
    
    -- End of test
    --wait;

  end process stimuli_y_target;

  -----------------------------------------------------------------------
  -- Check outputs
  -----------------------------------------------------------------------

  check_outputs : process
    variable check_ok : boolean := true;
  begin

    -- Check outputs T_STROBE time after rising edge of clock
    wait until rising_edge(aclk);
    wait for T_STROBE;

    -- Do not check the output payload values, as this requires the behavioral model
    -- which would make this demonstration testbench unwieldy.
    -- Instead, check the protocol of the RESULT master channel:
    -- check that the payload is valid (not X) when TVALID is high

    if valid_out = '1' then
      if is_x(sum) then
        report "ERROR: sum is invalid when valid_out is high" severity error;
        check_ok := false;
      end if;

    end if;

    assert check_ok
      report "ERROR: terminating test with failures." severity failure;

  end process check_outputs;


  -----------------------------------------------------------------------
  -- Assign TDATA / TUSER fields to aliases, for easy simulator waveform viewing
  -----------------------------------------------------------------------

  -- A operand slave channel alias signals
  x_this_real    <= flt_to_real(x_this(31 downto 0), 32, 24);
  x_this_special <= flt_to_special(x_this(31 downto 0), 32, 24);
  x_this_sign    <= x_this(31);
  x_this_exp     <= x_this(30 downto 23);
  x_this_mant    <= x_this(22 downto 0);
  
  -- A operand slave channel alias signals
  x_target_real    <= flt_to_real(x_target(31 downto 0), 32, 24);
  x_target_special <= flt_to_special(x_target(31 downto 0), 32, 24);
  x_target_sign    <= x_target(31);
  x_target_exp     <= x_target(30 downto 23);
  x_target_mant    <= x_target(22 downto 0);

  -- B operand slave channel alias signals
  y_this_real    <= flt_to_real(y_this(31 downto 0), 32, 24);
  y_this_special <= flt_to_special(y_this(31 downto 0), 32, 24);
  y_this_sign    <= y_this(31);
  y_this_exp     <= y_this(30 downto 23);
  y_this_mant    <= y_this(22 downto 0);
  
  -- B operand slave channel alias signals
  y_target_real    <= flt_to_real(y_target(31 downto 0), 32, 24);
  y_target_special <= flt_to_special(y_target(31 downto 0), 32, 24);
  y_target_sign    <= y_target(31);
  y_target_exp     <= y_target(30 downto 23);
  y_target_mant    <= y_target(22 downto 0);

  -- Result master channel alias signals
  sum_real     <= flt_to_real(sum(31 downto 0), 32, 24) when valid_out = '1';
  sum_special  <= flt_to_special(sum(31 downto 0), 32, 24) when valid_out = '1';
  sum_sign     <= sum(31) when valid_out = '1';
  sum_exp      <= sum(30 downto 23) when valid_out = '1';
  sum_mant     <= sum(22 downto 0) when valid_out = '1';

end tb;

