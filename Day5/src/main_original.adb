-- Advent of Code 2020, Day 5
--
-- John Perry
--
-- apologies for any bad Ada style
--
-- binary boarding passes

-- the boarding passes have 10 characters which represent a binary number
--
-- the first 7 consist of either 'F' or 'B', which gives a binary search
-- mechanism to locate a seat in the front or back of the plane:
-- if the first one is 'F', then you're in the first 64 seats;
-- if the second one is also 'F', then you're in the first 32 seats;
-- if the third one is then 'B', then your in the 2nd group of 16 seats; etc.
--
-- the last 3 consist of either 'L' or 'R', and give a binary search
-- mechanism to locate a seat in a row:
-- if the first one is 'L', then you're in the first 4 seats;
-- if the second one is then 'R', then you're in the 2nd group of 2 seats; etc.
--
-- there is then a "seat number" which is determined by multiplying the row
-- number by 8 and adding the column number
--
-- first exercise: determine the highest seat ID on a plane where seating
-- assignments are essentially binary numbers (it is not 1024)
--
-- second exercise: determine your seat number, which will be the only
-- one missing from the list

with Ada.Text_IO;
use Ada.Text_IO;

with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;

with Ada.Containers.Vectors;

procedure Main is

   F: File_Type;

   type Boarding_Pass is record
      Row, Column: Natural := 0;
   end record;

   function Seat_Number(P: in out Boarding_Pass) return Positive is
   ( P.Row * 8 + P.Column );

   package Seat_Vector is new Ada.Containers.Vectors
   (
    Element_Type => Boarding_Pass,
    Index_Type => Positive
   );

   package Occupied_Seat_Vector is new Ada.Containers.Vectors
   (
    Element_Type => Boolean,
    Index_Type => Positive
   );

   All_Seats: Seat_Vector.Vector;
   Seat_Is_Occupied: Occupied_Seat_Vector.Vector;

   procedure Read_Boarding_Pass is
   begin
      declare
         Line: String := Get_Line(F);
         Pass: Boarding_Pass;
      begin
         for I in 1 .. 7 loop
            Pass.Row := Pass.Row * 2;
            if Line(I) = 'B' then Pass.Row := Pass.Row + 1; end if;
         end loop;
         for I in 8 .. 10 loop
            Pass.Column := Pass.Column * 2;
            if Line(I) = 'R' then Pass.Column := Pass.Column + 1; end if;
         end loop;
         All_Seats.Append(Pass);
      end;
   end Read_Boarding_Pass;

begin

   Open(F, In_File, "/Users/user/common/Ada/AoC2020/Day5/input.txt");
   while not End_Of_File(F) loop
      Read_Boarding_Pass;
   end loop;
   Close(F);

   Put("there are "); Put(All_Seats.Length'Image); Put(" seats total");
   New_Line(1);

   -- we can do parts 1 and 2 pretty quickly in one block

   declare
      Seat: Positive;
      Maximum_Seat_Number: Positive := 1;
   begin

      -- part 1: identify the maximum seat number

      for Pass of All_Seats loop
         Seat := Seat_Number(Pass);
         Maximum_Seat_Number :=
         (
          if Maximum_Seat_Number >= Seat then Maximum_Seat_Number else Seat
         );
      end loop;
      Put("the maximum seat number is "); Put(Maximum_Seat_Number); New_Line(1);

      -- part 2: determine the missing seat, which is +1 or -1 from any seat
      -- between those listed

      -- initialize record of occupied seats
      Seat_Is_Occupied :=
            Occupied_Seat_Vector.To_Vector(
               False, Ada.Containers.Count_Type(Maximum_Seat_Number)
            );
      for Pass of All_Seats loop
         Seat_Is_Occupied(Seat_Number(Pass)) := True;
      end loop;

      -- start by determine where the seats actually begin
      -- (first & last few seat numbers don't exist)
      Seat := 1;
      while not Seat_Is_Occupied(Seat) loop Seat := Seat + 1; end loop;
      -- now find a missing seat
      while Seat <= Positive(Seat_Is_Occupied.Length) and Seat_Is_Occupied(Seat)
      loop
         Seat := Seat + 1;
      end loop;
      Put("your seat number is "); Put(Seat); New_Line(1);


   end;

end Main;
