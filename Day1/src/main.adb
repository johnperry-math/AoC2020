-- Advent of Code 2020, Day 1
--
-- John Perry
-- reworked based on advice from Emmanuel Briot and Maxim Reznick
--
-- later realized / learned that Hashed_Sets may be a bad idea due to
-- repeated values, so switching to Vectors
--
-- apologies for any bad Ada style
--
-- read expense entries from a file (they look to be integers)
-- first exercise: find two that add to 2020, report their product
-- second exercise: find three that add to 2020, report their product

with Ada.Text_IO;
use Ada.Text_IO;

with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;

with Ada.Containers.Vectors;

procedure Main is

   F : File_Type;

   package Positive_Vectors is new Ada.Containers.Vectors
   (
    Element_Type => Natural,
    Index_Type   => Positive
   );

   Expenses: Positive_Vectors.Vector;

   A, B, C : Natural;

begin

   -- read the entries

   Open( F, In_File, "/Users/user/common/Ada/AoC2020/Day1/input.txt" );
   while not End_Of_File(F) loop
      Get( F, A );
      Expenses.Append(A);
   end loop;
   Close(F);

   -- identify two numbers that sum to 2020 and report their product

   Outer_2:
   for I in Expenses.First_Index .. Expenses.Last_Index - 1 loop

      A := Expenses(I);
      if A <= 2020 then
         B := 2020 - A;

         Inner_2:
         for J in I + 1 .. Expenses.Last_Index loop
            if Expenses(J) = B then
               Put(A); Put(" + "); Put(B); Put(" = "); Put(A + B); New_Line(1);
               Put(A); Put(" * "); Put(B); Put(" = "); Put(A * B); New_Line(1);
               exit Outer_2;
            end if;
         end loop Inner_2;

      end if;

   end loop Outer_2;

   -- now identify three numbers that sum to 2020 and report their product

   Outer_3:
   for I in Expenses.First_Index .. Expenses.Last_Index - 2 loop

      A := Expenses(I);

      Middle_3:
      for J in I + 1 .. Expenses.Last_Index - 1 loop

         B := Expenses(J);

         if ( A + B <= 2020 ) then
            C := 2020 - ( A + B );

            Inner_3:
            for K in J + 1 .. Expenses.Last_Index loop
               if Expenses(K) = C then
                  Put(A); Put(" + "); Put(B); Put(" + "); Put(C); Put(" = ");
                  Put(A + B + C); New_Line(1);
                  Put(A); Put(" + "); Put(B); Put(" + "); Put(C); Put(" = ");
                  Put(A * B * C); New_Line(1);
                  exit Outer_3;
               end if;
            end loop Inner_3;
         end if;

      end loop Middle_3;

   end loop Outer_3;

end Main;
