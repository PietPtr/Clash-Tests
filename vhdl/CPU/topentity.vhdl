-- Automatically generated VHDL-93
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use IEEE.MATH_REAL.ALL;
use std.textio.all;
use work.all;
use work.cpu_types.all;

entity topentity is
  port(-- clock
       clk     : in cpu_types.clk_system;
       -- reset
       rst     : in cpu_types.rst_system;
       en      : in boolean;
       \c$arg\ : in signed(11 downto 0);
       s1_0    : out signed(11 downto 0);
       s1_1_0  : out signed(11 downto 0);
       s1_1_1  : out signed(11 downto 0);
       s1_2_0  : out signed(11 downto 0);
       s1_2_1  : out signed(11 downto 0);
       s1_2_2  : out signed(11 downto 0);
       s1_2_3  : out signed(11 downto 0);
       s1_2_4  : out signed(11 downto 0);
       s1_2_5  : out signed(11 downto 0);
       s1_2_6  : out signed(11 downto 0);
       s1_2_7  : out signed(11 downto 0);
       s1_2_8  : out signed(11 downto 0);
       s1_2_9  : out signed(11 downto 0);
       s1_2_10 : out signed(11 downto 0);
       s1_2_11 : out signed(11 downto 0);
       s1_2_12 : out signed(11 downto 0);
       s1_2_13 : out signed(11 downto 0);
       s1_2_14 : out signed(11 downto 0);
       s1_2_15 : out signed(11 downto 0));
end;

architecture structural of topentity is
  -- CPU.hs:77:1-42
  signal \c$s1_rec\       : cpu_types.tup3;
  -- CPU.hs:34:1-93
  signal pc               : signed(11 downto 0);
  -- CPU.hs:34:1-93
  signal ds1              : cpu_types.array_of_signed_12(0 to 1);
  -- CPU.hs:34:1-93
  signal mem              : cpu_types.array_of_signed_12(0 to 15);
  -- CPU.hs:34:1-93
  signal word             : signed(11 downto 0);
  -- CPU.hs:34:1-93
  signal val              : signed(11 downto 0);
  -- CPU.hs:34:1-93
  signal r0               : signed(11 downto 0);
  -- CPU.hs:34:1-93
  signal regs             : signed(11 downto 0);
  -- CPU.hs:34:1-93
  signal regs1            : cpu_types.array_of_signed_12(0 to 0);
  -- CPU.hs:34:1-93
  signal regs2            : cpu_types.array_of_signed_12(0 to 1);
  signal result           : cpu_types.tup3;
  signal \c$case_alt\     : boolean;
  signal \c$case_alt_0\   : boolean;
  signal \c$app_arg\      : signed(11 downto 0);
  signal \c$case_scrut\   : boolean;
  signal \c$app_arg_0\    : signed(11 downto 0);
  signal result_0         : cpu_types.array_of_signed_12(0 to 1);
  signal \c$app_arg_1\    : signed(11 downto 0);
  signal \c$app_arg_2\    : signed(11 downto 0);
  signal \c$app_arg_3\    : signed(11 downto 0);
  signal \c$case_scrut_0\ : boolean;
  signal \c$app_arg_4\    : signed(11 downto 0);
  signal result_1         : cpu_types.array_of_signed_12(0 to 1);
  -- CPU.hs:30:1-62
  signal scrut            : cpu_types.array_of_signed_12(0 to 0);
  signal \c$app_arg_5\    : cpu_types.array_of_signed_12(0 to 15);
  signal wild             : signed(63 downto 0);
  signal \c$app_arg_6\    : signed(11 downto 0);
  signal \c$app_arg_7\    : signed(11 downto 0);
  signal \c$app_arg_8\    : signed(11 downto 0);
  signal wild_0           : signed(63 downto 0);
  signal result_2         : signed(11 downto 0);
  -- CPU.hs:34:1-93
  signal \c$r_app_arg\    : signed(11 downto 0);
  signal result_3         : signed(11 downto 0);
  -- CPU.hs:34:1-93
  signal \c$ins1_app_arg\ : signed(11 downto 0);
  signal wild_1           : signed(63 downto 0);
  signal \c$vec\          : cpu_types.array_of_signed_12(0 to 0);
  signal s1               : cpu_types.tup3;
  signal s1_1             : cpu_types.array_of_signed_12(0 to 1);
  signal s1_2             : cpu_types.array_of_signed_12(0 to 15);

begin
  -- register begin
  topentity_register : block
    signal cs1_rec_reg : cpu_types.tup3  := ( tup3_sel0_signed => to_signed(0,12), tup3_sel1_array_of_signed_12_0 => cpu_types.array_of_signed_12'( to_signed(0,12), to_signed(0,12) ), tup3_sel2_array_of_signed_12_1 => cpu_types.array_of_signed_12'( to_signed(769,12), to_signed(-1777,12), to_signed(1024,12), to_signed(0,12), to_signed(0,12), to_signed(0,12), to_signed(0,12), to_signed(0,12), to_signed(0,12), to_signed(0,12), to_signed(0,12), to_signed(0,12), to_signed(0,12), to_signed(0,12), to_signed(0,12), to_signed(0,12) ) ) ;
  begin
    \c$s1_rec\ <= cs1_rec_reg; 
    cs1_rec_r : process(clk,rst)
    begin
      if rst =  '1'  then
        cs1_rec_reg <= ( tup3_sel0_signed => to_signed(0,12), tup3_sel1_array_of_signed_12_0 => cpu_types.array_of_signed_12'( to_signed(0,12), to_signed(0,12) ), tup3_sel2_array_of_signed_12_1 => cpu_types.array_of_signed_12'( to_signed(769,12), to_signed(-1777,12), to_signed(1024,12), to_signed(0,12), to_signed(0,12), to_signed(0,12), to_signed(0,12), to_signed(0,12), to_signed(0,12), to_signed(0,12), to_signed(0,12), to_signed(0,12), to_signed(0,12), to_signed(0,12), to_signed(0,12), to_signed(0,12) ) );
      elsif rising_edge(clk) then
        if en then
          cs1_rec_reg <= result
          -- pragma translate_off
          after 1 ps
          -- pragma translate_on
          ;
        end if;
      end if;
    end process;
  end block;
  -- register end

  pc <= \c$s1_rec\.tup3_sel0_signed;

  ds1 <= \c$s1_rec\.tup3_sel1_array_of_signed_12_0;

  mem <= \c$s1_rec\.tup3_sel2_array_of_signed_12_1;

  -- index begin
  indexvec : block
    signal vec_index : integer range 0 to 16-1;
  begin
    vec_index <= to_integer((wild_1))
    -- pragma translate_off
                 mod 16
    -- pragma translate_on
                 ;
    word <= mem(vec_index);
  end block;
  -- index end

  val <= word and to_signed(255,12);

  r0 <=  ds1(0) ;

  \c$vec\ <= (ds1(1 to ds1'high));

  regs <=  \c$vec\(0) ;

  regs1 <= cpu_types.array_of_signed_12'(0 => regs);

  regs2 <= cpu_types.array_of_signed_12'(signed'(r0) & regs1);

  with (result_3) select
    result <= ( tup3_sel0_signed => \c$app_arg_6\
              , tup3_sel1_array_of_signed_12_0 => result_1
              , tup3_sel2_array_of_signed_12_1 => mem ) when x"000",
              ( tup3_sel0_signed => \c$app_arg_6\
              , tup3_sel1_array_of_signed_12_0 => result_1
              , tup3_sel2_array_of_signed_12_1 => mem ) when x"001",
              ( tup3_sel0_signed => \c$app_arg_8\
              , tup3_sel1_array_of_signed_12_0 => regs2
              , tup3_sel2_array_of_signed_12_1 => mem ) when x"002",
              ( tup3_sel0_signed => \c$app_arg_7\
              , tup3_sel1_array_of_signed_12_0 => regs2
              , tup3_sel2_array_of_signed_12_1 => mem ) when x"003",
              ( tup3_sel0_signed => \c$app_arg_6\
              , tup3_sel1_array_of_signed_12_0 => regs2
              , tup3_sel2_array_of_signed_12_1 => \c$app_arg_5\ ) when x"004",
              ( tup3_sel0_signed => pc
              , tup3_sel1_array_of_signed_12_0 => regs2
              , tup3_sel2_array_of_signed_12_1 => mem ) when others;

  \c$case_alt\ <= result_2 = \c$app_arg_4\;

  \c$case_alt_0\ <= result_2 = \c$app_arg_0\;

  with (result_3) select
    \c$app_arg\ <= val when x"000",
                   val + \c$app_arg_3\ when others;

  with (result_3) select
    \c$case_scrut\ <= \c$case_alt_0\ when x"000",
                      \c$case_alt_0\ when others;

  with (result_3) select
    \c$app_arg_0\ <= to_signed(1,12) when x"000",
                     to_signed(1,12) when others;

  result_0 <= cpu_types.array_of_signed_12'( \c$app_arg_1\
                                           , \c$app_arg\ ) when \c$case_scrut\ else
              cpu_types.array_of_signed_12'(0 to 1 => signed'(0 to 11 => '-'));

  \c$app_arg_1\ <=  regs2(0) ;

  with (result_3) select
    \c$app_arg_2\ <= val when x"000",
                     val + \c$app_arg_1\ when others;

  \c$app_arg_3\ <=  scrut(0) ;

  with (result_3) select
    \c$case_scrut_0\ <= \c$case_alt\ when x"000",
                        \c$case_alt\ when others;

  with (result_3) select
    \c$app_arg_4\ <= to_signed(0,12) when x"000",
                     to_signed(0,12) when others;

  result_1 <= cpu_types.array_of_signed_12'( \c$app_arg_2\
                                           , \c$app_arg_3\ ) when \c$case_scrut_0\ else
              result_0;

  scrut <= regs2(1 to regs2'high);

  -- replace begin
  replacevec : block
    signal vec_index_0 : integer range 0 to 16-1;
  begin
    vec_index_0 <= to_integer((wild))
    -- pragma translate_off
                 mod 16
    -- pragma translate_on
                 ;

    process(vec_index_0,mem,\c$app_arg_8\)
      variable ivec : cpu_types.array_of_signed_12(0 to 15);
    begin
      ivec := mem;
      ivec(vec_index_0) := \c$app_arg_8\;
      \c$app_arg_5\ <= ivec;
    end process;
  end block;
  -- replace end

  wild <= (resize(val,64));

  \c$app_arg_6\ <= pc + to_signed(1,12);

  with (r0) select
    \c$app_arg_7\ <= val when x"000",
                     \c$app_arg_6\ when others;

  -- index begin
  indexvec_0 : block
    signal vec_index_1 : integer range 0 to 2-1;
  begin
    vec_index_1 <= to_integer((wild_0))
    -- pragma translate_off
                 mod 2
    -- pragma translate_on
                 ;
    \c$app_arg_8\ <= regs2(vec_index_1);
  end block;
  -- index end

  wild_0 <= (resize(result_2,64));

  result_2 <= \c$r_app_arg\ and to_signed(1,12);

  \c$r_app_arg\ <= shift_right(word,to_integer(to_signed(8,64)));

  result_3 <= \c$ins1_app_arg\ and to_signed(7,12);

  \c$ins1_app_arg\ <= shift_right(word,to_integer(to_signed(9,64)));

  wild_1 <= (resize(pc,64));

  s1 <= \c$s1_rec\;

  s1_0 <= s1.tup3_sel0_signed;

  s1_1 <= s1.tup3_sel1_array_of_signed_12_0;

  s1_2 <= s1.tup3_sel2_array_of_signed_12_1;

  s1_1_0 <= s1_1(0);

  s1_1_1 <= s1_1(1);

  s1_2_0 <= s1_2(0);

  s1_2_1 <= s1_2(1);

  s1_2_2 <= s1_2(2);

  s1_2_3 <= s1_2(3);

  s1_2_4 <= s1_2(4);

  s1_2_5 <= s1_2(5);

  s1_2_6 <= s1_2(6);

  s1_2_7 <= s1_2(7);

  s1_2_8 <= s1_2(8);

  s1_2_9 <= s1_2(9);

  s1_2_10 <= s1_2(10);

  s1_2_11 <= s1_2(11);

  s1_2_12 <= s1_2(12);

  s1_2_13 <= s1_2(13);

  s1_2_14 <= s1_2(14);

  s1_2_15 <= s1_2(15);


end;

