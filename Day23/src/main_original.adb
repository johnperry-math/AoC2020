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
-- part 1: rotate 9 cups 100 times
--
-- part 2: rotate 1000000 cups 10000000 times
--

pragma Ada_2020;

with Ada.Text_IO;
use Ada.Text_IO;

with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;

procedure Main is

   -- this generic package works fine for part 1
   -- it probably works fine for part 2, but copying a one-million-element array
   -- is exceedingly time-consuming, so I am switching to linked lists
   --
   -- also note that one must increase the default stack size; on MacOS I used
   --    ulimit -s 13000

   generic
      C_Last: Positive;
      type C_Range is mod <>;
      type L_Range is mod <>;
      type C_Array is array ( C_Range range <> ) of L_Range;
      Start_Config: C_Array;
   package Crab_Cup_Configuration is
      Circle: C_Array := Start_Config;
      procedure Put_Current_Configuration;
      function Contains(Cups: C_Array; Value: L_Range) return Boolean;
      procedure Crab_Moves;
   end Crab_Cup_Configuration;

   package body Crab_Cup_Configuration is

      procedure Put_Current_Configuration is
      begin
         Put("( ");
         for Label of Circle loop
            Put(Positive(Label), 0); Put(' ');
         end loop;
         Put(')');
      end Put_Current_Configuration;

      function Contains(Cups: C_Array; Value: L_Range) return Boolean is
      ( ( for some Cup of Cups => Cup = Value ) );

      procedure Crab_Moves is
         Picked_Up: C_Array( 0 .. 2 );
         Left_Down: array ( 0 .. C_Last - 4 ) of L_Range;
         Destination: L_Range;
      begin
         -- pick up 3, leave 6
         Picked_Up(0) := Circle(1);
         Picked_Up(1) := Circle(2);
         Picked_Up(2) := Circle(3);
         Left_Down(0) := Circle(0);
         for I in 1 .. Left_Down'Last loop
            Left_Down(I) := Circle(C_Range(I) + 3);
         end loop;
         -- select destination
         Destination := Circle(0) - 1;
         if Destination = 0 then Destination := L_Range'Last; end if;
         while Contains(Picked_Up, Destination) loop
            Destination := Destination - 1;
            if Destination = 0 then Destination := L_Range'Last; end if;
         end loop;
         -- place cups
         declare
            I: C_Range := 0;
            J: Positive range 1 .. C_Last - 4 := 1;
         begin
            loop
               Circle(I) := Left_Down(J);
               exit when Left_Down(J) = Destination;
               I := I + 1;
               J := J + 1;
            end loop;
            I := I + 1; J := J + 1;
            Circle(I) := Picked_Up(0);
            Circle(I+1) := Picked_Up(1);
            Circle(I+2) := Picked_Up(2);
            for K in I + 3 .. Circle'Last loop
               Circle(K) := Left_Down(J);
               if J < C_Last - 4 then J := J + 1; end if;
            end loop;
         end;
      end Crab_Moves;

   end Crab_Cup_Configuration;

   -- types and initial configuration for part 1

   C_Last: constant := 10;

   type Cup_Range is mod C_Last - 1;

   type Label_Type is mod C_Last;

   type Cup_Array is array ( Cup_Range range <> ) of Label_Type;

   -- Start_Config: Cup_Array := ( 3, 2, 6, 5, 1, 9, 4, 7, 8 ); -- input
   Start_Config: Cup_Array := ( 3, 8, 9, 1, 2, 5, 4, 6, 7 ); -- example

   package Crab_Cups_10 is new Crab_Cup_Configuration
   (
    C_Last => C_Last,
    C_Range => Cup_Range, L_Range => Label_Type, C_Array => Cup_Array,
    Start_Config => Start_Config
   );

   -- types and initial configuration for part 2

   Larger_C_Last: constant := 1_000_001;

   type Larger_Cup_Range is mod Larger_C_Last - 1;

   type Larger_Label_Type is mod Larger_C_Last;

   type Larger_Cup_Array
         is array ( Larger_Cup_Range range <> ) of Larger_Label_Type with Pack;

   Larger_Start_Config: Larger_Cup_Array
         := ( (
               for I in Larger_Cup_Range(0) .. Larger_Cup_Range(Larger_C_Last-2)
               => (
                   if I in Larger_Cup_Range(0) .. Larger_Cup_Range(8)
                   then Larger_Label_Type(Start_Config(Cup_Range(I)))
                   else Larger_Label_Type(I) + 1
                  )
            ) );

   package Crab_Cups_1000000 is new Crab_Cup_Configuration
   (
    C_Last => Larger_C_Last,
    C_Range => Larger_Cup_Range, L_Range => Larger_Label_Type,
    C_Array => Larger_Cup_Array,
    Start_Config => Larger_Start_Config
   );

begin
   -- part 1
   Put("start: ");
   Crab_Cups_10.Put_Current_Configuration; New_Line;
   for I in 1 .. 100 loop
      Crab_Cups_10.Crab_Moves;
   end loop;
   Crab_Cups_10.Put_Current_Configuration; New_Line;

   -- part 2
   -- Put("start: ");
   -- Crab_Cups_1000000.Put_Current_Configuration; New_Line;
   for I in 1 .. 10_000_000 loop
      Crab_Cups_1000000.Crab_Moves;
      if I mod 1000 = 0 then Put(I, 0); New_Line; end if;
   end loop;
   Crab_Cups_1000000.Put_Current_Configuration;

end Main;
pragma main(stack_size=>300_000_000);
