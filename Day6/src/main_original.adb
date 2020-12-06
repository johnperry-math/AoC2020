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

with Ada.Text_IO;
use Ada.Text_IO;

with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;

procedure Main is

   F: File_Type;

   Number_Of_Respondents: Natural := 0;
   Questions_Someone_Responded: Natural := 0;
   Questions_Everyone_Responded: Natural := 0;

begin

   Open(F, In_File, "/Users/user/common/Ada/AoC2020/Day6/input.txt");

   Through_Groups :
   while not End_Of_File(F) loop

      declare

         type Questions_Responded
               is array( Character range 'a' .. 'z' ) of Boolean;

         -- questions responded affirmatively by anyone in group
         -- start false, set true when someone responds
         Anyone: Questions_Responded := ( others => False );
         -- questions responded affirmatively by everyone in group
         -- start true, set false when someone doesn't respond
         Everyone: Questions_Responded := ( others => True );

      begin

         Through_Members:
         loop -- each line of the group

            declare

               S: String := Get_Line(F);
               This_One: Questions_Responded := ( others => False );

            begin

               exit when S = "";
               -- Put(S); New_Line(1);
               Number_Of_Respondents := Number_Of_Respondents + 1;

               -- record this person's responses in This_One and update Anyone
               Through_Person:
               for C of S loop

                  This_One(C) := True;

                  if not Anyone(C) then
                     Anyone(C) := True;
                     Questions_Someone_Responded
                           := Questions_Someone_Responded + 1;
                  end if;

               end loop Through_Person;

               -- now update everyone
               for C in This_One'First .. This_One'Last loop
                  Everyone(C) := Everyone(C) and This_One(C);
               end loop;

            end;

            if End_Of_File(F) then exit; end if;

         end loop through_members;

         -- add number of questions everyone answered
         for C in Questions_Responded'First .. Questions_Responded'Last loop

            if Everyone(C) then
               Questions_Everyone_Responded := Questions_Everyone_Responded + 1;
            end if;

         end loop;

      end;

   end loop Through_Groups;

   Close(F);

   Put(Questions_Someone_Responded, 0);
   Put(" total questions answered 'yes' by at least one passenger in a group");
   New_Line(1);
   Put(Questions_Everyone_Responded, 0);
   Put(" total questions answered 'yes' by every passenger in a group");

end Main;
