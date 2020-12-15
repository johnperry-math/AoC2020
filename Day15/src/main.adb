-- Advent of Code 2020, Day 15
--
-- John Perry
--
-- apologies for any bad Ada style
--
-- Rambunctious Recitation
--
-- parts 1 and 2 both ask you to determine the nth value of a sequence defined
-- as follows:
--
-- - you are given the first few numbers
--   repeat
--      if previous number appeared the first time, next number is 0
--      else next number is difference between the last time the number appeared
--           and the previous time it appeared

pragma Ada_2020;

with Ada.Text_IO;
use Ada.Text_IO;

with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;

with Ada.Containers.Hashed_Maps;

procedure Main is

   Puzzle_Input: array ( 1 .. 7 ) of Natural := ( 0,20,7,16,1,18,15 );
   -- Puzzle_Input: array( 1 .. 3 ) of Natural := ( 0, 3, 6 );

   -- to keep track of the numbers we need to know
   -- I thought about this a while & wondered if we couldn't get away with only
   -- remembering two turns, but there doesn't seem to be an escape:
   --    + you MUST remember the first time spoken, to know which branch to take
   --    + you MUST remember the previous two times spoken, to know what result
   --      to give

   type Positive_Triple is record
      First, Last, Previous: Positive;
   end record;

   function Natural_Hash(N: Natural) return Ada.Containers.Hash_Type is
   (
    Ada.Containers.Hash_Type(N)
   );

   package Turn_Map is new Ada.Containers.Hashed_Maps
   (
    Key_Type        => Natural,         -- the number recited
    Element_Type    => Positive_Triple, -- when spoken
    Hash            => Natural_Hash,
    Equivalent_Keys => "="
   );

   Appearance: Turn_Map.Map;

   -- the game rules

   function Take_A_Turn(Value: Natural; Current_Turn: Positive) return Natural
   -- apply the rules of the game on Current_Turn to value,
   -- which was spoken on previous turn

   is

      Result: Natural := 0;

   begin

      if Appearance(Value).First = Current_Turn - 1 then

         -- first time spoken was previous turn; next number is 0
         -- update 0's appearances

         Appearance(0).Previous := Appearance(0).Last;
         Appearance(0).Last := Current_Turn;

      else

         -- not first time spoken;
         -- next number is number of turns since last time spoken

         Result := Appearance(Value).Last - Appearance(Value).Previous;

         -- update map

         if Appearance.Contains(Result) then

            -- already has the number, so update its appearances

            Appearance(Result).Previous := Appearance(Result).Last;
            Appearance(Result).Last := Current_Turn;

         else

            -- new number! add it

            declare
               New_Record: Positive_Triple := ( others => Current_Turn );
            begin
               Appearance.Insert(Result, New_Record);
            end;

         end if;

      end if;

      return Result;

   end Take_A_Turn;

begin

   -- initialize with input

   for I in Puzzle_Input'First .. Puzzle_Input'Last loop

      declare New_Record: Positive_Triple := ( others => I );
      begin
         Appearance.Insert(Puzzle_Input(I), New_Record);
      end;

   end loop;

   -- parts one and two in one block

   declare Last_Value: Natural := Puzzle_Input(Puzzle_Input'Last);
   begin

      -- part 1: figure out the 2020th number in the sequence
      for I in Puzzle_Input'Length + 1 .. 2020 loop
         Last_Value := Take_A_Turn(Last_Value, I);
      end loop;
      Put("on turn 2020, the number recited is "); Put(Last_Value, 0);
      New_Line(1);

      -- part 2: figure out the 30000000th number in the sequence
      -- this is relatively long (half a minute?) and seemingly pointless
      -- maybe there's a hidden periodicity at work, but I didn't check
      for I in 2021 .. 30000000 loop
         Last_Value := Take_A_Turn(Last_Value, I);
      end loop;
      Put("on turn 30000000, the number recited is "); Put(Last_Value, 0);
      New_Line(1);

   end;

end Main;
