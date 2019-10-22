-- Automatically generated VHDL-93
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use IEEE.MATH_REAL.ALL;
use std.textio.all;
use work.all;
use work.tutorial_types.all;

entity topentity is
  port(-- clock
       clk       : in tutorial_types.clk_system;
       -- reset
       rst       : in tutorial_types.rst_system;
       en        : in boolean;
       \c$arg_0\ : in signed(8 downto 0);
       \c$arg_1\ : in signed(8 downto 0);
       \c$arg_2\ : in signed(8 downto 0);
       s1_0      : out signed(8 downto 0);
       s1_1      : out signed(8 downto 0);
       s1_2      : out signed(8 downto 0));
end;

architecture structural of topentity is
  -- Tutorial.hs:68:1-42
  signal \c$s1_rec\                    : tutorial_types.tup3;
  -- Tutorial.hs:57:1-59
  signal r0                            : signed(8 downto 0);
  -- Tutorial.hs:57:1-59
  signal r1                            : signed(8 downto 0);
  -- Tutorial.hs:57:1-59
  signal r2                            : signed(8 downto 0);
  -- Tutorial.hs:57:1-59
  signal ins                           : signed(8 downto 0);
  -- Tutorial.hs:57:1-59
  signal r                             : signed(8 downto 0);
  -- Tutorial.hs:57:1-59
  signal val                           : signed(8 downto 0);
  signal result                        : tutorial_types.tup3;
  -- Tutorial.hs:52:1-66
  signal r0_0                          : signed(8 downto 0);
  -- Tutorial.hs:52:1-66
  signal r1_0                          : signed(8 downto 0);
  -- Tutorial.hs:52:1-66
  signal r2_0                          : signed(8 downto 0);
  signal \c$app_arg\                   : signed(8 downto 0);
  signal result_0                      : tutorial_types.tup3;
  signal \c$applyr_f1Out\              : signed(8 downto 0);
  signal result_1                      : tutorial_types.tup3;
  signal \c$app_arg_0\                 : signed(8 downto 0);
  signal result_2                      : tutorial_types.tup3;
  signal \c$app_arg_1\                 : signed(8 downto 0);
  signal \c$case_scrut\                : boolean;
  signal \c$case_alt\                  : boolean;
  signal \c$case_alt_0\                : boolean;
  signal \c$case_alt_1\                : boolean;
  signal \c$applyr_f1Out_app_arg\      : signed(8 downto 0);
  signal \c$applyr_f1Out_case_scrut\   : boolean;
  signal result_3                      : signed(8 downto 0);
  signal \c$applyr_f1Out_case_scrut_0\ : boolean;
  signal \c$case_alt_2\                : signed(8 downto 0);
  -- Tutorial.hs:47:1-44
  signal r_0                           : signed(8 downto 0);
  -- Tutorial.hs:47:1-44
  signal ds2                           : signed(8 downto 0);
  -- Tutorial.hs:47:1-44
  signal ds3                           : signed(8 downto 0);
  signal \c$arg\                       : tutorial_types.tup3;
  signal s1                            : tutorial_types.tup3;

begin
  \c$arg\ <= ( tup3_sel0_signed_0 => \c$arg_0\
             , tup3_sel1_signed_1 => \c$arg_1\
             , tup3_sel2_signed_2 => \c$arg_2\ );

  -- register begin
  topentity_register : block
    signal cs1_rec_reg : tutorial_types.tup3  := ( tup3_sel0_signed_0 => to_signed(0,9), tup3_sel1_signed_1 => to_signed(0,9), tup3_sel2_signed_2 => to_signed(0,9) ) ;
  begin
    \c$s1_rec\ <= cs1_rec_reg; 
    cs1_rec_r : process(clk,rst)
    begin
      if rst =  '1'  then
        cs1_rec_reg <= ( tup3_sel0_signed_0 => to_signed(0,9), tup3_sel1_signed_1 => to_signed(0,9), tup3_sel2_signed_2 => to_signed(0,9) );
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

  r0 <= \c$s1_rec\.tup3_sel0_signed_0;

  r1 <= \c$s1_rec\.tup3_sel1_signed_1;

  r2 <= \c$s1_rec\.tup3_sel2_signed_2;

  ins <= \c$arg\.tup3_sel0_signed_0;

  r <= \c$arg\.tup3_sel1_signed_1;

  val <= \c$arg\.tup3_sel2_signed_2;

  with (ins) select
    result <= result_0 when "000000000",
              result_0 when "000000001",
              result_0 when "000000010",
              ( tup3_sel0_signed_0 => r0 + \c$case_alt_2\
              , tup3_sel1_signed_1 => r1
              , tup3_sel2_signed_2 => r2 ) when "000000011",
              tutorial_types.tup3'( signed'(0 to 8 => '-'), signed'(0 to 8 => '-'), signed'(0 to 8 => '-') ) when others;

  r0_0 <= \c$s1_rec\.tup3_sel0_signed_0;

  r1_0 <= \c$s1_rec\.tup3_sel1_signed_1;

  r2_0 <= \c$s1_rec\.tup3_sel2_signed_2;

  with (ins) select
    \c$app_arg\ <= to_signed(0,9) when "000000000",
                   to_signed(0,9) when "000000001",
                   to_signed(0,9) when others;

  result_0 <= ( tup3_sel0_signed_0 => \c$applyr_f1Out\
              , tup3_sel1_signed_1 => r1_0
              , tup3_sel2_signed_2 => r2_0 ) when \c$applyr_f1Out_case_scrut\ else
              result_1;

  with (ins) select
    \c$applyr_f1Out\ <= val when "000000000",
                        \c$applyr_f1Out_app_arg\ + val when "000000001",
                        \c$applyr_f1Out_app_arg\ - val when others;

  result_1 <= ( tup3_sel0_signed_0 => r0_0
              , tup3_sel1_signed_1 => \c$applyr_f1Out\
              , tup3_sel2_signed_2 => r2_0 ) when \c$applyr_f1Out_case_scrut_0\ else
              result_2;

  with (ins) select
    \c$app_arg_0\ <= to_signed(1,9) when "000000000",
                     to_signed(1,9) when "000000001",
                     to_signed(1,9) when others;

  result_2 <= ( tup3_sel0_signed_0 => r0_0
              , tup3_sel1_signed_1 => r1_0
              , tup3_sel2_signed_2 => \c$applyr_f1Out\ ) when \c$case_scrut\ else
              tutorial_types.tup3'( signed'(0 to 8 => '-'), signed'(0 to 8 => '-'), signed'(0 to 8 => '-') );

  with (ins) select
    \c$app_arg_1\ <= to_signed(2,9) when "000000000",
                     to_signed(2,9) when "000000001",
                     to_signed(2,9) when others;

  with (ins) select
    \c$case_scrut\ <= \c$case_alt\ when "000000000",
                      \c$case_alt\ when "000000001",
                      \c$case_alt\ when others;

  \c$case_alt\ <= r = \c$app_arg_1\;

  \c$case_alt_0\ <= r = \c$app_arg_0\;

  \c$case_alt_1\ <= r = \c$app_arg\;

  \c$applyr_f1Out_app_arg\ <= r0_0 when \c$applyr_f1Out_case_scrut\ else
                              result_3;

  with (ins) select
    \c$applyr_f1Out_case_scrut\ <= \c$case_alt_1\ when "000000000",
                                   \c$case_alt_1\ when "000000001",
                                   \c$case_alt_1\ when others;

  result_3 <= r1_0 when \c$applyr_f1Out_case_scrut_0\ else
              r2_0;

  with (ins) select
    \c$applyr_f1Out_case_scrut_0\ <= \c$case_alt_0\ when "000000000",
                                     \c$case_alt_0\ when "000000001",
                                     \c$case_alt_0\ when others;

  with (r) select
    \c$case_alt_2\ <= r_0 when "000000000",
                      ds2 when "000000001",
                      ds3 when "000000010",
                      signed'(0 to 8 => '-') when others;

  r_0 <= \c$s1_rec\.tup3_sel0_signed_0;

  ds2 <= \c$s1_rec\.tup3_sel1_signed_1;

  ds3 <= \c$s1_rec\.tup3_sel2_signed_2;

  s1 <= \c$s1_rec\;

  s1_0 <= s1.tup3_sel0_signed_0;

  s1_1 <= s1.tup3_sel1_signed_1;

  s1_2 <= s1.tup3_sel2_signed_2;


end;

