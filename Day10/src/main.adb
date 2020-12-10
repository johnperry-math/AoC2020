-- Advent of Code 2020, Day 10
--
-- John Perry
--
-- apologies for any bad Ada style
--
-- arranging adapters
--
-- first exercise: in an array of adapters whose differences are 1, 2, or 3
-- jolts, determine the number of 1-jolt differences and the number of 3-jolt
-- differences, then return the product
--
-- second exercise: find the product of all valid arrangements of these adapters
-- where you might remove one or more; an arrangement is valid if and only if
-- no gap between adapters is larger than 3
--
-- the second exercise can be solved in a non-brute force manner by identifying
-- subsequences ( a1 , ... , ai ) where
--
-- (a) a1 is at least 3 larger than the element before it;
-- (b) ai is at least 3 smaller than the element after it; and
-- (c) the difference between any two elements of the subsequence is at most 3.
--
-- if we can determine the number of arrangements of these subsequences, then
-- we can multiply them together to obtain the number of arrangements of the
-- entire sequence
--
-- so, how do we determine the number of arrangements of these subsequences?
--
-- one manner is by brute force, which I did not implement
--
-- for a few small contiguous subsequences (2, 3, ... 7) it's not too hard to
-- determine the number of valid arrangments
--
-- I worried the input would have subsequences with a distance of less than 3
-- between elements, such as ( 1, 2, 4, 6 ), where 3 and 5 do not appear
-- in the original input. I even defined an exception just in case! but
-- either the puzzle author deliberately excluded this possibility, or he made
-- the same error I did, or there's a theorem to be proved somewhere in there,
-- but I don't feel like proving it.
-- (especially since my intuition says the theorem is wrong)
--
-- given, then, that all subsequences appear to be contiguous, we can exclude
-- a brute force approach to the subsequences, giving a very fast solution
--
-- that said, I have to wonder if I wasted more time trying to find the elegant
-- solution than if I had just implemented a brute force approach to the
-- subsequences... ;-)
--

with Ada.Text_IO;
use Ada.Text_IO;

with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;

-- the adapter sizes aren't large, but the number of arrangements is
with Ada.Long_Long_Integer_Text_IO;
use Ada.Long_Long_Integer_Text_IO;

with Ada.Containers.Vectors;

procedure Main is

   -- I/O

   F: File_Type;

   Current_Number: Natural;

   -- storing the adapter sizes

   package Natural_Vector is new Ada.Containers.Vectors
   (
    Element_Type => Natural,
    Index_Type => Positive
   );

   Joltage_Adapters: Natural_Vector.Vector;

   -- sorting the adapter sizes

   package Natural_Sorting is new Natural_Vector.Generic_Sorting;

   procedure Natural_Sort(V: in out Natural_Vector.Vector)
         renames Natural_Sorting.Sort;

   -- in case there is reason to believe the solution is erroneous

   Dont_Know: exception;

   function Contiguous_Subsequence_Arrangements(N: Natural) return Natural is
   -- returns the number of arrangements of a contiguous subsequence
   -- I didn't quite find a pattern, and after kicking it around far too long
   -- I decided to try what I had and raise exceptions for what I dind't;
   -- it turns out I had more than I needed (none of my sequences had a length
   -- longer than 5)
   (
         case N is
         when 0 | 1 | 2 => 1,
         when 3 => 2,
         when 4 => 4,
         when 5 => 7,
         when 6 => 13,
         when 7 => 24,
         when others => raise Dont_Know with "contiguous subsequence too large"
   );

begin

   -- build the list of adapters:
   --
   -- start with the output, which has joltage 0
   -- read in all the adapters, sort them, then add the device's adapter,
   -- which is 3 larger than the largest supplied adapter

   Joltage_Adapters.Append(0);

   Open(F, In_File, "/Users/user/common/Ada/AoC2020/Day10/input.txt");

   while not End_Of_File(F) loop
      Get(F, Current_Number);
      Joltage_Adapters.Append( Current_Number );
   end loop;

   Close(F);

   Natural_Sort(Joltage_Adapters);

   Joltage_Adapters.Append( Joltage_Adapters.Last_Element + 3 );

   -- exercise 1: count joltage differences between outlet, adapters,
   -- and device, multiplying number of 1-jolt differences by 3-jolt differences

   declare

      J1_Diffs, J3_Diffs: Natural := 0;

   begin

      -- check the adapters sequentially

      for I in 2 .. Positive(Joltage_Adapters.Length) loop

         if Joltage_Adapters(I) - Joltage_Adapters(I-1) = 1 then
            J1_Diffs := J1_Diffs + 1;
         elsif Joltage_Adapters(I) - Joltage_Adapters(I-1) = 3 then
            J3_Diffs := J3_Diffs + 1;
         end if;

      end loop;

      Put("there are "); Put(J1_Diffs, 0); Put(" differences of 1 jolt and ");
      Put(J3_Diffs, 0); Put(" differences of 3 jolts");
      New_Line(1);

      Put("the product of these values is "); Put(J1_Diffs * J3_Diffs, 0);
      New_Line(1);

   end;

   -- exercise 2: find the total number of distinct arrangements of the adapters
   --
   -- basically loops through the list linearly, finding contiguous
   -- subsequences, counting the numbers of their arrangements, and multiplying
   -- them to a running count

   declare

      I, J: Natural; -- locations in the list of adapters

      Arrangements: Long_Long_Integer := 1; -- running count of arrangements

   begin

      I := 1;

      while I < Positive(Joltage_Adapters.Length) - 1 loop

         -- identify a contiguous subsequence

         J := 1;
         while I + J < Positive(Joltage_Adapters.Length)
               and Joltage_Adapters(I+J) = Joltage_Adapters(I) + J
         loop
            J := J + 1;
         end loop;

         Arrangements := Arrangements
               * Long_Long_Integer(Contiguous_Subsequence_Arrangements(J));

--           Put("contiguous subsequence of "); Put(J, 0);
--           Put(" elements starting at "); Put(I, 0);
--           Put(" gives "); Put(Arrangements, 0);
--           New_Line(1);

         I := I + J;

         -- I've only verified this approach for contiguous subsequences where
         -- the distance between the largest and smallest adapter to the next
         -- adapters in the main sequence is at least 3, so we need to raise
         -- an error (probably with reason) if that assumption is not satisfied

         if I < Positive(Joltage_Adapters.Length) - 1 and
               Joltage_Adapters(I) < Joltage_Adapters(I-1) + 3
         then
            raise Dont_Know with "output is likely erroneous";
         end if;

      end loop;

      Put(Arrangements, 0); Put(" arrangements"); New_Line(1);

   end;

end Main;
