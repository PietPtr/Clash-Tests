library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

package tutorial_types is


  subtype rst_system is std_logic;
  subtype clk_system is std_logic;
  type tup3 is record
    tup3_sel0_signed_0 : signed(8 downto 0);
    tup3_sel1_signed_1 : signed(8 downto 0);
    tup3_sel2_signed_2 : signed(8 downto 0);
  end record;
  function toSLV (b : in boolean) return std_logic_vector;
  function fromSLV (sl : in std_logic_vector) return boolean;
  function tagToEnum (s : in signed) return boolean;
  function dataToTag (b : in boolean) return signed;
  function toSLV (s : in signed) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return signed;
  function toSLV (sl : in std_logic) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return std_logic;
  function toSLV (p : tutorial_types.tup3) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return tutorial_types.tup3;
end;

package body tutorial_types is
  function toSLV (b : in boolean) return std_logic_vector is
  begin
    if b then
      return "1";
    else
      return "0";
    end if;
  end;
  function fromSLV (sl : in std_logic_vector) return boolean is
  begin
    if sl = "1" then
      return true;
    else
      return false;
    end if;
  end;
  function tagToEnum (s : in signed) return boolean is
  begin
    if s = to_signed(0,64) then
      return false;
    else
      return true;
    end if;
  end;
  function dataToTag (b : in boolean) return signed is
  begin
    if b then
      return to_signed(1,64);
    else
      return to_signed(0,64);
    end if;
  end;
  function toSLV (s : in signed) return std_logic_vector is
  begin
    return std_logic_vector(s);
  end;
  function fromSLV (slv : in std_logic_vector) return signed is
  begin
    return signed(slv);
  end;
  function toSLV (sl : in std_logic) return std_logic_vector is
  begin
    return std_logic_vector'(0 => sl);
  end;
  function fromSLV (slv : in std_logic_vector) return std_logic is
    alias islv : std_logic_vector (0 to slv'length - 1) is slv;
  begin
    return islv(0);
  end;
  function toSLV (p : tutorial_types.tup3) return std_logic_vector is
  begin
    return (toSLV(p.tup3_sel0_signed_0) & toSLV(p.tup3_sel1_signed_1) & toSLV(p.tup3_sel2_signed_2));
  end;
  function fromSLV (slv : in std_logic_vector) return tutorial_types.tup3 is
  alias islv : std_logic_vector(0 to slv'length - 1) is slv;
  begin
    return (fromSLV(islv(0 to 8)),fromSLV(islv(9 to 17)),fromSLV(islv(18 to 26)));
  end;
end;

