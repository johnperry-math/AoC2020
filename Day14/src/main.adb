-- Advent of Code 2020, Day 14
--
-- John Perry
--
-- apologies for any bad Ada style
--
-- Shuttle Search
--
-- it seemed more appropriate to put description of problems below
-- than to put them here

pragma Ada_2020;

with Ada.Text_IO;
use Ada.Text_IO;

with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;

with Ada.Long_Integer_Text_IO;
use Ada.Long_Integer_Text_IO;

with Ada.Containers.Vectors;

with Ada.Containers.Hashed_Maps;

procedure Main is

   F: File_Type;

   -- 36 bit integers
   -- need to define this type to work with bit masking

   type Word is mod 68719476736;

   -- memory map for part 1

   function Long_Hash(L: Integer) return Ada.Containers.Hash_Type is
   ( Ada.Containers.Hash_Type(L) );

   package Memory_Map is new Ada.Containers.Hashed_Maps
   (
    Key_Type => Integer,
    Element_Type => Word,
    Hash => Long_Hash,
    Equivalent_Keys => "="
   );

   Mem: Memory_Map.Map;

   -- masking
   -- there are three symbols: 0, 1, X
   --
   -- in part 1, 0 means "turn off", 1 means "turn on", and X means "leave be"
   --
   -- in part 2, 0 means "leave be", 1 means "turn on", and X means "both";
   -- that is, both 0 and 1

   type Masking is ( On, Off, Dont_Care );

   type Mask is array ( 1 .. 36 ) of Masking;

   function Read_Mask(S: String) return Mask is

   -- returns the mask defined by S according to the specification above
   -- note however that I reverse the order for the sake of convenience later on

      Result: Mask;

   begin

      for I in 1 .. S'Length loop

         Result(37 - I) := (
               case S(S'First - 1 + I) is
               when '1' => On,
               when '0' => Off,
               when 'X' => Dont_Care,
               when others => raise Data_Error
         );

      end loop;

      return Result;

   end Read_Mask;

   procedure Apply_Mask(Value: in out Word; M: Mask) is

   -- this applies the mask according to the rules in part 1
   -- we use two Word's, Turn_On and Turn_Off, to change one bit at a time
   -- Turn_On starts at 1 and rotates left with each iteration
   -- Turn Off starts at 2#111...10# and likewise rotates left each iteration

      Turn_On: Word := 1;
      Turn_Off: Word := Word'Last;

   begin

      for I in 1 .. 36 loop

         Turn_Off := Turn_Off - Turn_On;

         Value :=
         (
          case M(I) is
             when On        => Value or Turn_On,
             when Off       => Value and Turn_Off,
             when Dont_Care => Value
         );

         -- shift the mask one over
         Turn_Off := Turn_Off + Turn_On;
         Turn_On := Turn_On * 2;

      end loop;

   end;

   procedure Store_To_Mem(S: String; M: Mask) is

   -- this stores to memory according to the rules in part 1:
   -- apply the mask to the value before we store it

      Location: Integer; -- where to store the value
      Read_Value: Long_Integer; -- the value as read
      Value: Word; -- the value as a Word
      Last: Positive := 5; -- last position read in S

   begin

      Get(S(S'First - 1 + Last..S'Last), Location, Last);
      Get(S(Last+5..S'Last), Read_Value, Last);

      Value := Word(Read_Value);
      Apply_Mask(Value, M);

      if Mem.Contains(Location) then Mem(Location) := Value;
      else Mem.Insert(Location, Value);
      end if;

   end Store_To_Mem;

   -- memory map for part 2

   function Key_Hash(K: Word) return Ada.Containers.Hash_Type is
   -- there aren't that many locations, so this isn't as dangerous as it looks
   (
         Ada.Containers.Hash_Type( Long_Long_Integer(K)
                                       mod Long_Long_Integer( Integer'Last ) )
   );

   package Second_Memory_Map is new Ada.Containers.Hashed_Maps
   (
    Key_Type => Word,
    Element_Type => Long_Integer,
    Hash => Key_Hash,
    Equivalent_Keys => "="
   );

   Mem2: Second_Memory_Map.Map;

   -- in part 2, a mask of X means "floating", so we need to generate
   -- 2 memory locations for each X
   -- we store them in a Location_Array

   -- for clarity I hope
   Float_Bit renames Dont_Care;

   type Location_Array is array( Positive range <> ) of Word;

   function Count_Xs(M: Mask) return Natural is
   -- counts the number of X's in M
      Power: Natural := 1;
   begin
      for X of M loop
         if X = Float_Bit then Power := Power * 2; end if;
      end loop;
      return Power;
   end Count_Xs;

   function Find_All_Locations(Key: Long_Integer; M: Mask) return Location_Array
   -- determines all locations of Key when we apply M, according to Part 2
   -- for each X in mask it takes all previously known values and sets
   -- their value at the index of X with both 1 and 0
   --
   -- to do this we use two lists: locations computed on previous pass,
   -- and locations computing on current pass
   is

      -- index of last location computed in previous pass
      I: Positive := 1;

      -- used the same way as in Apply_Mask; see the documentation there
      Turn_On: Word := 1;
      Turn_Off: Word := Word'Last;

      -- the value we are asked to store
      Base_Location: Word := Word(Key);

      -- we use Mod2 and Which to switch between two arrays
      type Mod2 is mod 2;
      Which: Mod2 := 1;
      Result: array ( Mod2 ) of Location_Array( 1 .. Count_Xs(M) );

   begin

      -- first turn on everything that needs turning on
      for X of M loop
         if X = On then Base_Location := Base_Location or Turn_On; end if;
         Turn_On := Turn_On * 2;
      end loop;

      Result(0)(1) := Base_Location;

      -- now apply the floating mask

      Turn_On := 1;

      for X of M loop

         Turn_Off := Turn_Off - Turn_On;

         if X = Float_Bit then

            for J in 1 .. I loop
               Result(Which)(2 * J - 1) := Result(Which + 1)(J) or Turn_On;
               Result(Which)(2 * J) := Result(Which + 1)(J) and Turn_Off;
            end loop;

            I := I * 2;
            Which := Which + 1;

         end if;

         -- rotate
         Turn_Off := Turn_Off + Turn_On;
         Turn_On := Turn_On * 2;

      end loop;

      return Result(Which + 1);

   end Find_All_Locations;

   procedure Store_To_Masked_Mem(S: String; M: Mask) is
   -- store to memory according to the rules in part 2

      Last: Positive := 5; -- used for reading S
      Value: Long_Integer; -- value to store
      Read_Loc: Long_Integer; -- location as read

   begin

      -- read location and value
      Get(S(S'First - 1 + Last..S'Last), Read_Loc, Last);
      Get(S(Last+5..S'Last), Value, Last);

      -- determine all locations we need to write,
      -- then write to them

      declare

         All_Locations: Location_Array := Find_All_Locations(Read_Loc, M);

      begin

         for Loc of All_Locations loop

            if Mem2.Contains(Loc) then
               Mem2(Loc) := Value;
            else
               Mem2.Insert(Loc, Value);
            end if;

         end loop;

      end;

   end Store_To_Masked_Mem;

   Current_Mask: Mask := ( others => Dont_Care );

begin

   -- part 1

   -- read the input, write masked values to memory

   Open(F, In_File, "/Users/user/common/Ada/AoC2020/Day14/input.txt");

   while not End_Of_File(F) loop
      declare
         S: String := Get_Line(F);
      begin
         if S(1..3) = "mem" then Store_To_Mem(S, Current_Mask);
         else
            Current_Mask := Read_Mask( S(8..S'Length) );
         end if;
      end;
   end loop;

   Close(F);

   -- report the sum

   declare Sum: Long_Integer := 0;
   begin
      for Value of Mem loop
         Sum := Sum + Long_Integer(Value);
      end loop;
      Put("the sum of values in memory is "); Put(Sum, 0); New_Line(1);
   end;

   -- part 2

   -- read the input, write values to masked memory

   Open(F, In_File, "/Users/user/common/Ada/AoC2020/Day14/input.txt");

   while not End_Of_File(F) loop
      declare
         S: String := Get_Line(F);
      begin
         if S(1..3) = "mem" then Store_To_Masked_Mem(S, Current_Mask);
         else
            Current_Mask := Read_Mask( S(8..S'Length) );
         end if;
      end;
   end loop;

   Close(F);

   -- report the sum

   declare Sum: Long_Integer := 0;
   begin
      for Value of Mem2 loop
         Sum := Sum + Long_Integer(Value);
      end loop;
      Put("the sum of values in memory is "); Put(Sum, 0); New_Line(1);
   end;

end Main;
