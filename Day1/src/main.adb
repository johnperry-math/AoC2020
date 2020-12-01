-- Advent of Code 2020, Day 1
--
-- John Perry
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

procedure Main is

   F : File_Type;
   Array_Size : Natural := 0;

begin

   -- count the entries

   declare

      A : Natural;

   begin

      Open( F, In_File, "/Users/user/common/Ada/AoC2020/Day1/input.txt" );
      while not End_Of_File(F) loop
         Get(F, A);
         Array_Size := Array_Size + 1;
      end loop;
      Close(F);

   end; -- declare

   Put("number of expenses: "); Put(Array_Size); New_Line(1);

   -- create an array and perform the exercises

   declare

      Expenses : array ( 1 .. Array_Size ) of Positive;
      C, D, E : Positive;
      I : Positive := 1;

   begin

      -- read the entries

      Open( F, In_File, "/Users/user/common/Ada/AoC2020/Day1/input.txt" );
      while not End_Of_File(F) loop
         Get( F, Expenses(I) );
         I := I + 1;
      end loop;
      Close(F);

      -- identify two numbers that sum to 2020 and report their product

      for I in Positive range 1 .. Array_Size - 1 loop
         C := Expenses(I);
         for J in Positive range I + 1 .. Array_Size loop
            D := Expenses(J);
            if C + D = 2020 then
               Put(C); Put(" + "); Put(D); Put(" = "); Put(C + D); New_Line(1);
               Put(C); Put(" * "); Put(D); Put(" = "); Put(C * D); New_Line(1);
            end if;
         end loop; -- j
      end loop; -- i

      -- now identify three numbers that sum to 2020 and report their product

      for I in Positive range 1 .. Array_Size - 2 loop
         C := Expenses(I);
         for J in Positive range I + 1 .. Array_Size - 1 loop
            D := Expenses(J);
            if C + D < 2020 then
               for K in Positive range J + 1 .. Array_Size loop
                  E := Expenses(K);
                  if C + D + E = 2020 then
                     Put(C); Put(" + "); Put(D); Put(" + "); Put(E); Put(" = ");
                     Put(C + D + E); New_Line(1);
                     Put(C); Put(" * "); Put(D); Put(" * "); Put(E); Put(" = ");
                     Put(C * D * E); New_Line(1);
                  end if;
               end loop; -- k
            end if;
         end loop; -- j
      end loop; -- i

   end; -- declare

end Main;
