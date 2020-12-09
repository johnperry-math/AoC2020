-- Advent of Code 2020, Day 9
--
-- John Perry
--
-- apologies for any bad Ada style
--
-- hacking (?) a network protocol
--
-- after a 25-number preamble, each subsequent number must be the sum
-- of two of the previous 25
--
-- first exercise: report the first number that is not the sum of two of the
-- previous 25
--
-- second exercise: find a contiguous sequence of numbers that add to this
-- number, then report the sum of the first and last in that sequence
--
-- I kept screwing up my logic on the first exercise, and the example didn't
-- reveal my error, so this took a lot longer than it should have
--

with Ada.Text_IO;
use Ada.Text_IO;

with Ada.Long_Long_Integer_Text_IO;
use Ada.Long_Long_Integer_Text_IO;

with Ada.Containers.Vectors;

procedure Main is

   -- the numbers in the input

   package Natural_Vector is new Ada.Containers.Vectors
         (
          Element_Type => Long_Long_Integer,
          Index_Type   => Positive
         );

   Previous_List: Natural_Vector.Vector;

   -- Found will tell us when we've found one of the values we're looking for
   -- Current_Number is the first number that is not the sum of the previous 25
   -- A, B are numbers that add up to each exercise's solution

   A, B, Current_Number: Long_Long_Integer;

   -- input file

   F: File_Type;

begin

   -- read numbers
   -- originally I stored only 25 numbers at a time, and solved the first
   -- exercise while reading the values, but on account of exercise 2
   -- I might as well read the whole thing

   Open(F, In_File, "/Users/user/common/Ada/AoC2020/Day9/input.txt");

   while not End_Of_File(F) loop
      Get(F, Current_Number);
      Previous_List.Append(Current_Number);
   end loop;

   Close(F);

   -- first exercise:
   -- find first instance where a number
   -- does not add to the previous two numbers

   for I in 26 .. Positive(Previous_List.Length) loop

      Current_Number := Previous_List(I);

      for J in I - 25 .. I - 2 loop

         if Current_Number > Previous_List(J) then

            A := Previous_List(J);

            for K in J + 1 .. I - 1 loop

               B := Previous_List(K);

               if A /= B and A + B = Current_Number then
                  goto Found_Pair;
               end if;

            end loop;

         end if;

      end loop;

      -- if we reach this point, then we've not found a pair of numbers
      -- in the previous 25 that sum to Current_Number
      goto Found_First;

   << Found_Pair >>
      -- if we reach this point, then we found a pair in the previous 25
      -- that sum to Current_Number, so we need to continue the loop
      null;

   end loop;

   << Found_First >>
   Put("bad: "); Put(Current_Number, 0); New_Line(1);

   -- second exercise:
   -- find a contiguous sequence of numbers that add up to our bad number
   -- then report sum of first, last numbers in sequence

   for I in 1 .. Positive( Previous_List.Length ) loop

      declare

         J: Positive := I + 1;
         Current_Sum: Long_Long_Integer := Previous_List(I);

      begin

         while Current_Sum < Current_Number loop
            Current_Sum := Current_Sum + Previous_List(J);
            J := J + 1;
         end loop;

         if Current_Sum = Current_Number then
            A := Previous_List(I);
            B := Previous_List(J - 1);
            exit;
         end if;

      end;

   end loop;

   Put("weakness: "); Put(A + B, 0); New_Line(1);

end Main;
