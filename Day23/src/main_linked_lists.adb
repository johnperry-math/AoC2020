-- Advent of Code 2020, Day 23
--
-- John Perry
--
-- apologies for any bad Ada style
--
-- Crab Cups
--
-- rotate some cups in a circle according to peculiar rules
--
-- part 1: rotate the cups 100 times
--
-- part 2: rotate 1000000 cups 10000000 times
--

pragma Ada_2020;

with Ada.Text_IO;
use Ada.Text_IO;

with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Hashed_Sets;

procedure Main is

   package Positive_DLLs is new Ada.Containers.Doubly_Linked_Lists
   (
    Element_Type => Positive
   );

   function Positive_Hash(P: Positive) return Ada.Containers.Hash_Type is
   ( Ada.Containers.Hash_Type(P) );

   package Positive_Sets is new Ada.Containers.Hashed_Sets
   (
    Element_Type => Positive,
    Hash => Positive_Hash,
    Equivalent_Elements => "="
   );

   type Cup_Array is array( Positive range <> ) of Positive;

   generic
      Start_Config: Cup_Array;
   package Crab_Cup_Configuration is
      Is_Initialized: Boolean := False;
      Circle: Positive_DLLs.List;
      Last_Element: Positive := Start_Config'Length;
      procedure Initialize;
      procedure Put_Current_Configuration with Pre => Is_Initialized;
      procedure Crab_Moves with Pre => Is_Initialized;
   end Crab_Cup_Configuration;

   package body Crab_Cup_Configuration is

      procedure Initialize is
      begin
         for Each of Start_Config loop
            Circle.Append(Each);
         end loop;
         Is_Initialized := True;
      end Initialize;

      procedure Put_Current_Configuration is
      begin
         Put("( ");
         for Label of Circle loop
            Put(Positive(Label), 0); Put(' ');
         end loop;
         Put(')');
      end Put_Current_Configuration;

      procedure Crab_Moves is
         Picked_Up: Cup_Array( 1 .. 3 );
         Pos: Positive_DLLs.Cursor := Circle.First;
         Destination: Natural;
      begin
         Destination := Circle(Pos) - 1;
         if Destination = 0 then Destination := Positive(Circle.Length); end if;
         Positive_DLLs.Next(Pos);
         -- pick up 3, leave 6
         Picked_Up(1) := Circle(Pos);
         Positive_DLLs.Next(Pos);
         Picked_Up(2) := Circle(Pos);
         Positive_DLLs.Next(Pos);
         Picked_Up(3) := Circle(Pos);
         Positive_DLLs.Previous(Pos);
         Positive_DLLs.Previous(Pos);
         Circle.Delete(Pos, 3);
         -- select destination
         while Destination = Picked_Up(1) or Destination = Picked_Up(2)
               or Destination = Picked_Up(3)
         loop
            Destination := Destination - 1;
            if Destination = 0 then Destination := Last_Element; end if;
         end loop;
         -- place cups
         -- move first element to end of list
         declare Old_First_Value: Positive := Circle.First_Element;
         begin
            Circle.Delete_First;
            Circle.Append(Old_First_Value);
         end;
         -- find new position
         declare
            Pos: Positive_DLLs.Cursor := Circle.First;
         begin
            while Circle(Pos) /= Destination loop
               Positive_DLLs.Next(Pos);
            end loop;
            Positive_DLLs.Next(Pos);
            for I in 1 .. 3 loop
               Circle.Insert(Pos, Picked_Up(I));
            end loop;
         end;
      end Crab_Moves;

   end Crab_Cup_Configuration;

   -- types and initial configuration for part 1

   -- Start_Config: Cup_Array := ( 3, 2, 6, 5, 1, 9, 4, 7, 8 ); -- input
   Start_Config: Cup_Array := ( 3, 8, 9, 1, 2, 5, 4, 6, 7 ); -- example

   package Crab_Cups_10 is new Crab_Cup_Configuration
   (
    Start_Config => Start_Config
   );

   -- types and initial configuration for part 2

   Larger_C_Last: constant := 1_000_000;

   Larger_Start_Config: Cup_Array
         := ( (
               for I in 1 .. Larger_C_Last
               => ( if I in 1 .. 9 then Start_Config(I) else I )
            ) );

   package Crab_Cups_1000000 is new Crab_Cup_Configuration
   (
    Start_Config => Larger_Start_Config
   );

begin
   -- part 1
   Crab_Cups_10.Initialize;
   Put("start: ");
   Crab_Cups_10.Put_Current_Configuration; New_Line;
   for I in 1 .. 100 loop
      Crab_Cups_10.Crab_Moves;
   end loop;
   Crab_Cups_10.Put_Current_Configuration; New_Line;

   -- part 2
   Crab_Cups_1000000.Initialize;
   -- Put("start: ");
   -- Crab_Cups_1000000.Put_Current_Configuration; New_Line;
   for I in 1 .. 10_000_000 loop
      Crab_Cups_1000000.Crab_Moves;
      if I mod 100 = 0 then Put(I, 0); New_Line; end if;
   end loop;
   Crab_Cups_1000000.Put_Current_Configuration;

end Main;
