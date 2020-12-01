-- Advent of Code 2020, Day 1
--
-- John Perry
-- reworked based on advice from Emmanuel Briot and Maxim Reznick
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

with Ada.Containers;
use Ada.Containers;

with Ada.Containers.Hashed_Sets;

procedure Main is

   F : File_Type;

   function Hash ( Value: Positive ) return Hash_Type is
      ( Hash_Type'Mod( Value ) );

   package Positive_Sets is new Ada.Containers.Hashed_Sets
   (
    Element_Type        => Positive,
    Hash                => Hash,
    Equivalent_Elements => "="
   );

   Expenses: Positive_Sets.Set;

   A, C, D, E : Positive;

begin

   -- read the entries

   Open( F, In_File, "/Users/user/common/Ada/AoC2020/Day1/input.txt" );
   while not End_Of_File(F) loop
      Get( F, A );
      Expenses.Insert(A);
   end loop;
   Close(F);

   -- identify two numbers that sum to 2020 and report their product

   Outer_2:
   for A in Expenses.Iterate loop

      C := Positive_Sets.Element(A);
      if C < 2020 then
         D := 2020 - C;
         if Expenses.Contains(D) then
            Put(C); Put(" + "); Put(D); Put(" = "); Put(C + D); New_Line(1);
            Put(C); Put(" * "); Put(D); Put(" = "); Put(C * D); New_Line(1);
            exit Outer_2;
         end if;
      end if;

   end loop Outer_2;

   -- now identify three numbers that sum to 2020 and report their product

   Outer_3:
   for A in Expenses.Iterate loop

      C := Positive_Sets.Element(A);

      Inner:
      for B in Expenses.Iterate loop

         D := Positive_Sets.Element(B);

         if ( C + D < 2020 ) then
            E := 2020 - ( C + D );
            if Expenses.Contains(E) then
               Put(C); Put(" + "); Put(D); Put(" + "); Put(E); Put(" = ");
               Put(C + D + E); New_Line(1);
               Put(C); Put(" * "); Put(D); Put(" * "); Put(E); Put(" = ");
               Put(C * D * E); New_Line(1);
               exit Outer_3;
            end if;
         end if;

      end loop Inner;

   end loop Outer_3;

end Main;
