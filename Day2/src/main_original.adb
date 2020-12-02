-- Advent of Code 2020, Day 2
--
-- John Perry
--
-- apologies for any bad Ada style
--
-- get password policy and password entries from a file, then
-- validate password against policy
--
-- policy specified as <rule1>-<rule2> <char>: <password>
-- where <rule1> and <rule2> are positive integers, <char> is a character,
-- and <password> is a sequence of characters (length varies)
--
-- exercise: determine how many passwords are valid
--
-- there were two variant exercises; see Check_Password_* functions
--
-- I would prefer to use vectors, but I'm pretty weak on reading strings
-- in Ada, so this first solution read the file twice, once per
-- policy

with Ada.Text_IO;
use Ada.Text_IO;

with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;

procedure Main is

   -- for Geting the file
   F : File_Type;
   Number_Of_Passwords: Natural := 0;

   -- variables related to policy
   Rule_1 , Rule_2 : Positive;
   Letter: Character;

   Valid_Passwords_Count: Natural := 0;

   -- reads the policy and returns true if successful
   -- otherwise, returns False if end of file occurred at beginning,
   -- and will probably raise an exception if something else goes wrong
   --
   -- assumes that file's position is at beginning of policy
   function Read_Policy( F: File_Type ) return Boolean is
      Inchar: Character;
      Result: Boolean := False;
   begin
      if not End_Of_File(F) then
         Number_Of_Passwords := Number_Of_Passwords + 1;
         Get(F, Rule_1);
         Get(F, Inchar); -- dash
         Get(F, Rule_2);
         Get(F, Inchar); -- space
         Get(F, Letter);
         Get(F, Inchar); -- colon
         Get(F, Inchar); -- space;
         Result := True;
      end if;
      return Result;
   end Read_Policy;

   -- reads the file and checks the key letter's count satisfies policy
   --
   -- assumes that file's position is at beginning of password
   function Check_Password_Count(F: File_Type) return Boolean is
      Result: Boolean := False;
      Instances: Natural := 0;
      Inchar: Character;
   begin
      if not End_Of_File(F) then
         loop
            Get(F, Inchar);
            if Inchar = Letter then Instances := Instances + 1; end if;
            exit when End_Of_Line(F);
         end loop;
         Result := ( Instances in Rule_1 .. Rule_2 );
      end if;
      return Result;
   end Check_Password_Count;

   -- reads the file and checks that the key letter's positions satisfy policy
   --
   -- assumes that F's position is at beginning of password
   function Check_Password_Position(F: File_Type) return Boolean is
      Result: Boolean := False;
      Current_Position: Natural;
      Inchar: Character;
   begin
      if not End_Of_File(F) then
         Current_Position := 0;
         loop
            Get(F, Inchar);
            Current_Position := Current_Position + 1;
            if Inchar = Letter
                and ( Current_Position = Rule_1 or Current_Position = Rule_2 )
            then
               Result := not Result;
            end if;
            exit when End_Of_Line(F);
         end loop;
      end if;
      return Result;
   end Check_Password_Position;

begin

   -- first policy

   Open( F, In_File, "/Users/user/common/Ada/AoC2020/Day2/input.txt" );
   while not End_Of_File(F) loop
      Put( Number_Of_Passwords, 0 ); Put(": ");
      if Read_Policy(F) and then Check_Password_Count(F) then
         Valid_Passwords_Count := Valid_Passwords_Count + 1;
         Put("valid"); New_Line(1);
      else
         Put("invalid"); New_Line(1);
      end if;
   end loop;
   Close(F);

   Put(Number_Of_Passwords, 0); Put(" passwords total"); New_Line(1);
   Put(Valid_Passwords_Count, 0); Put(" valid passwords"); New_Line(1);

   -- reset data
   Number_Of_Passwords := 0;
   Valid_Passwords_Count := 0;

   -- second policy

   Open( F, In_File, "/Users/user/common/Ada/AoC2020/Day2/input.txt" );
   while not End_Of_File(F) loop
      Put( Number_Of_Passwords, 0 ); Put(": ");
      if Read_Policy(F) and then Check_Password_Position(F) then
         Valid_Passwords_Count := Valid_Passwords_Count + 1;
         Put("valid"); New_Line(1);
      else
         Put("invalid"); New_Line(1);
      end if;
   end loop;
   Close(F);

   Put(Number_Of_Passwords, 0); Put(" passwords total"); New_Line(1);
   Put(Valid_Passwords_Count, 0); Put(" valid passwords"); New_Line(1);

end Main;
