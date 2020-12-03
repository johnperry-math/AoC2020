-- Advent of Code 2020, Day 3
--
-- John Perry
--
-- apologies for any bad Ada style
--
-- a toboggan is sledding down an hill with open space ('.') and trees ('#')
-- the toboggan slides according to a certain slope (right r, down d)
--
-- first exercise: count the number of trees when r=3, d=1
--
-- second exercise: count the number of trees for each (r,d) in
--    { (3,1), (1,1), (5,1), (7,1), (1,2) }
-- then report the product

with Ada.Text_IO;
use Ada.Text_IO;

with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;

procedure Main is

   F: File_Type;
   Filename: String := "/Users/user/common/Ada/AoC2020/Day3/input.txt";

   -- to count the number of strings
   Trees_1_1, Trees_3_1, Trees_5_1, Trees_7_1, Trees_1_2: Natural;

   -- count the number of trees encountered when sliding down the mountain
   -- at a slope of "Right" units right and "Down" units down
   function Count_Trees(Right, Down: Positive) return Natural is

      Number_Of_Trees : Natural := 0;
      I, J: Natural := 1; -- toboggan's current position
      Line_Number: Natural := 0;

   begin

      Put("checking "); Put(Right, 0); Put("x"); Put(Down, 0); New_Line(1);

      while not End_Of_File(F) loop

         declare
            S: constant String := Get_Line(F);
         begin
            Line_Number := Line_Number + 1;
            if J = 1 and S(I) = '#' then
               Number_Of_Trees := Number_Of_Trees + 1;
               Put("tree at line "); Put(Line_Number, 0); Put(" column "); Put(I, 0);
               New_Line(1);
            end if;
            J := J + 1;
            if J > Down then
               J := 1;
               I := I + Right;
               if I > S'Length then I := I - S'Length; end if;
            end if;
         end;

      end loop;

      return Number_Of_Trees ;

   end Count_Trees;

begin

   -- part 1: count trees when toboggan has slope (3,1)

   Open( F, In_File, Filename );
   Trees_3_1 := Count_Trees(3,1);
   Close(F);
   Put("encountered "); Put(Trees_3_1, 0); Put(" trees with slope 3,1");
   New_Line(1);

   -- part 2: count trees for several slopes, and compute product

   Open( F, In_File, Filename );
   Trees_1_1 := Count_Trees(1, 1);
   Close(F);
   Put("encountered "); Put(Trees_1_1, 0); Put(" trees with slope 1,1");
   New_Line(1);

   Open( F, In_File, Filename );
   Trees_1_2 := Count_Trees(1, 2);
   Close(F);
   Put("encountered "); Put(Trees_1_2, 0); Put(" trees with slope 1,2");
   New_Line(1);

   Open( F, In_File, Filename );
   Trees_5_1 := Count_Trees(5, 1);
   Close(F);
   Put("encountered "); Put(Trees_5_1, 0); Put(" trees with slope 5,1");
   New_Line(1);

   Open( F, In_File, Filename );
   Trees_7_1 := Count_Trees(7, 1);
   Close(F);
   Put("encountered "); Put(Trees_7_1, 0); Put(" trees with slope 7,1");
   New_Line(1);

   Put("product of trees when using different slopes: ");
   Put( Trees_3_1 * Trees_1_1 * Trees_1_2 * Trees_5_1 * Trees_7_1, 0 );
   New_Line(1);

end Main;
