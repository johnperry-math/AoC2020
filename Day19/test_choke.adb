with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Test_Choke is

   S: String
      -- := "29: 116 82 | 119 24";
      -- := "29# 116 82 | 119 24";
      -- := "29:";  -- OK
      -- := "29#";
      -- := "29: "; -- NOT OK
      := "29# ";
   A: Natural;
   Last: Positive;

begin

   Get(S, A, Last);
   Put(A, 0); New_Line;

end test_choke;
