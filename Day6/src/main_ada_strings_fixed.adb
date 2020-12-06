-- Advent of Code 2020, Day 6
--
-- John Perry
--
-- apologies for any bad Ada style
--
-- helping groups fill out customs forms
--
-- each group is split into lines, one per group member
-- each line records which questions the group member answered affirmatively
-- blank lines separate groups
--
-- first exercise: count the number of questions g_i that someone in group i
-- answered affirmatively, then add the g_i for each group i
--
-- second exercise: count the number of questions g_i that everyone in group i
-- answered affirmatively, then add the g_i for each group i
--
-- inspired by Maxim Reznick's solution to Day 4,
-- for this day I decided to rework my original soluton using Ada.Strings.Maps
-- I *really* like the result, though I wonder about efficiency
--
-- the strategy will be this: convert each line to the set S of characters
-- that appear on that line. take the union of S with the set A of characters
-- to which someone has responded (initially A is the null set for each group)
-- and the intersection of S with the set E of characters to which everyone
-- has responded (initially E is the set { 'a', ..., 'z' } for each group).
-- after each group finishes, add the sizes of A and E to their respective
-- totals.

with Ada.Text_IO;
use Ada.Text_IO;

with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;

with Ada.Strings.Maps;
use Ada.Strings.Maps;

procedure Main is

   F: File_Type;

   Number_Of_Respondents: Natural := 0;
   Questions_Someone_Responded: Natural := 0;
   Questions_Everyone_Responded: Natural := 0;

begin

   Open(F, In_File, "/Users/user/common/Ada/AoC2020/Day6/input.txt");

   Through_Groups :
   while not End_Of_File(F) loop

      Process_Group:
      declare

         subtype Char_Set is Character_Set;
         -- questions responded affirmatively by anyone in group
         -- start empty, add characters when someone responds
         Anyone: Char_Set := Null_Set;
         -- questions responded affirmatively by everyone in group
         -- start full, remove characters when no one responds
         Lower_Case_Range: Character_Range :=  ( Low => 'a', High => 'z' );
         Everyone: Char_Set := To_Set( Lower_Case_Range );

      begin

         Through_Members:
         loop -- each line of the group

            Process_Line:
            declare

               S: String := Get_Line(F);
               This_One: Char_Set;

            begin

               exit when S = "";
               -- Put(S); New_Line(1);

               Number_Of_Respondents := Number_Of_Respondents + 1;

               -- these next three lines are really, really cool,
               -- especially when compared to my original solution
               -- in main_original.adb

               This_One := To_Set(S);
               Anyone := Anyone or This_One;
               Everyone := Everyone and This_One;

            end Process_Line;

            if End_Of_File(F) then exit; end if;

         end loop through_members;

         -- I wish it were possible to count the number of characters in a set
         -- without converting it to a sequence first
         Questions_Someone_Responded
               := Questions_Someone_Responded + To_Sequence(Anyone)'Length;
         Questions_Everyone_Responded
               := Questions_Everyone_Responded + To_Sequence(Everyone)'Length;

      end Process_Group;

   end loop Through_Groups;

   Close(F);

   Put(Number_Of_Respondents, 0); Put(" total respondents"); New_Line(1);

   Put(Questions_Someone_Responded, 0);
   Put(" total questions answered 'yes' by at least one passenger in a group");
   New_Line(1);

   Put(Questions_Everyone_Responded, 0);
   Put(" total questions answered 'yes' by every passenger in a group");
   New_Line(1);

end Main;
