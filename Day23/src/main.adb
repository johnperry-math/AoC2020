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
-- due to the use of a large array, you MUST increase the default stack size
-- to use this program; on MacOS I used
--    ulimit -s 13000
--
-- I used generics here. My original approach (main_original.adb) failed on
-- part 2 because I copied the array to move things over. That takes too long.
-- My second approach (main_linked_lists.adb) failed on part 2 because I have to
-- search through the list for the destination. That takes too long.
--
-- The approach that worked was to use arrays, but instead of listing the
-- numbers in order, list them according to an abbreviated permutation, where
--
--    element 0 -> first # in list, call it a
--    element a -> second # in list, call it b
--    element b -> third # in list, call it c
--    ...
--    last element -> last # in list
--
-- So the 0th element directs you the first number, the last element directs you
-- to the last number, and the ith element directs you to the number in the list
-- that comes after i.
--
-- EXAMPLE
--
-- The example input is ( 3 8 9 1 2 5 4 6 7 ). We store this as
--
--    0  1  2  3  4  5  6  7  8  9  10  <- index
--    3  2  5  8  6  4  7 10  9  1   7  <- array value
--
-- It is now easy to do the following:
--   + identify who follows index i (value at i)
--   + change the head (change value at 0)
--   + change the tail (change last value)
--   + rearrange any elements (change corresponding values)
-- It is inconvenient however to expand the list, but we don't need that
-- facility.

pragma Ada_2020;

-- we use almost no external packages!

with Ada.Text_IO;
use Ada.Text_IO;

with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;

procedure Main is

   -- an array of cups
   type Cup_Array is array ( Natural range <> ) of Positive;

   -- I wrote a generic package to deal with arrays of different lengths
   -- in retrospect that may have been overkill, but it worked and I obtained
   -- some desired experience of generics

   generic

      -- Start_Config should indicate the numbers in the desired sequence
      -- the package will automatically convert them to the structure we need
      Start_Config: Cup_Array;

   package Crab_Cup_Configuration is

      procedure Put_Current_Configuration(Verbose: Boolean := False);
      -- shows the current configuration of the numbers (in order)
      -- if Verbose = True, prefaces the configuration with the internal array

      procedure Crab_Moves;
      -- the crab moves once according to his rules

      function Element_After(I: Natural) return Natural;
      -- returns the element that appears after I in the current sequence
      -- use 0 for the head
      -- if I is larger than Start_Config'Length it raises a Data_Error

   private

      Circle: Cup_Array( 0 .. Start_Config'Length + 1 );
      -- holds the numbers according to the structure described above

      Picked_Up: array( 1 .. 3 ) of Positive;
      -- the three cups that the crab picks up each time

      procedure Initialize;
      -- converts the given start configuration to the data structure we need

   end Crab_Cup_Configuration;

   package body Crab_Cup_Configuration is

      procedure Initialize is
      begin

         -- point to first
         Circle(0) := Start_Config(Start_Config'First);

         -- point to last
         Circle(Circle'Last) := Start_Config(Start_Config'Last);
         Circle(Circle(Circle'Last)) := Circle'Last;

         -- point the rest
         for I in Start_Config'First .. Start_Config'Last - 1 loop
            Circle(Start_Config(I)) := Start_Config(I+1);
         end loop;

      end Initialize;

      function Element_After(I: Natural) return Natural is
      (
            if I < Circle'Last then Circle(I)
            else raise Data_Error with "circle has fewer than "
                  & Circle'Last'Image & " cups"
      );

      procedure Put_Current_Configuration(Verbose: Boolean := False) is
         I: Positive := Circle(0);
      begin

         if Verbose then
            -- show the underlying array
            Put("[ ");
            for I in 0 .. Circle'Last loop Put(Circle(I), 0); Put(' '); end loop;
            Put(']');
         end if;

         Put("( ");
         I := Circle(0);
         while I /= Circle'Last loop
            Put(I, 0); Put(' ');
            I := Circle(I);
         end loop;
         Put(')');

      end Put_Current_Configuration;

      function Contains(Val: Positive) return Boolean is
      ( Val /= Picked_Up(1) and Val /= Picked_Up(2) and Val /= Picked_Up(3) );

      procedure Crab_Moves is

         Head: Positive := Circle(0); -- "current" cup
         Destination: Natural; -- "destination" cup
         Last_Value: Positive := Start_Config'Last - Start_Config'First + 1;
         -- the last possible value we can insert

      begin

         -- pick up 3 after current
         Picked_Up(1) := Circle(Circle(0));
         Picked_Up(2) := Circle(Picked_Up(1));
         Picked_Up(3) := Circle(Picked_Up(2));

         -- select destination
         Destination := Head - 1;
         if Destination = 0 then Destination := Last_Value; end if;
         while not Contains(Destination) loop
            Destination := Destination - 1;
            if Destination = 0 then Destination := Last_Value; end if;
         end loop;

         -- place cups: move current head to end
         Circle(Circle(Circle'Last)) := Head;
         Circle(Head) := Circle'Last;
         Circle(Circle'Last) := Head;

         -- place cups: set new head
         Circle(0) := Circle(Picked_Up(3));

         -- place cups: insert cups picked up after destination
         declare After_Destination: Positive := Circle(Destination);
         begin
            Circle(Destination) := Picked_Up(1);
            Circle(Picked_Up(3)) := After_Destination;
         end;

      end Crab_Moves;

   begin
      Initialize;
   end Crab_Cup_Configuration;

   -- types and initial configuration for part 1

   Start_Config: Cup_Array := ( 3, 2, 6, 5, 1, 9, 4, 7, 8 ); -- input
   -- Start_Config: Cup_Array := ( 3, 8, 9, 1, 2, 5, 4, 6, 7 ); -- example

   package Crab_Cups_10 is new Crab_Cup_Configuration
   (
    Start_Config => Start_Config
   );

   -- types and initial configuration for part 2

   Larger_C_Last: constant := 1_000_000;

   Larger_Start_Config: Cup_Array
         := ( (
               for I in 0 .. Larger_C_Last - 1
               => ( if I in 0 ..8 then Start_Config(I) else I + 1 )
            ) );

   package Crab_Cups_1000000 is new Crab_Cup_Configuration
   (
    Start_Config => Larger_Start_Config
   );

begin

   -- part 1

   Put("start: ");
   Crab_Cups_10.Put_Current_Configuration; New_Line;

   -- play
   for I in 1 .. 100 loop
      Crab_Cups_10.Crab_Moves;
   end loop;

   -- report
   Crab_Cups_10.Put_Current_Configuration; New_Line;

   -- part 2

   -- you can pring this if you like,
   -- but it's not interesting except for debugging
   -- Put("start: ");
   -- Crab_Cups_1000000.Put_Current_Configuration(True);
   -- New_Line;

   -- play
   for I in 1 .. 10_000_000 loop
      Crab_Cups_1000000.Crab_Moves;
   end loop;

   -- as in remark above...
   -- Crab_Cups_1000000.Put_Current_Configuration(True); New_Line;

   -- report
   declare
      First_Cup: Positive := Crab_Cups_1000000.Element_After(1);
      Second_Cup: Positive := Crab_Cups_1000000.Element_After(First_Cup);
      Product: Long_Long_Integer
            := Long_Long_Integer(First_Cup) * Long_Long_Integer(Second_Cup);
   begin
      Put(First_Cup'Image); Put(" * "); Put(Second_Cup'Image); Put(" = ");
      Put_Line(Product'Image);
   end;

end Main;
