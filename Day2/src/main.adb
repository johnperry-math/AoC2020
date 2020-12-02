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
-- probably not my favorite way to do this;
-- maybe better to read all passwords into a vector of vectors,
-- but this uses less memory and I've put enough work into this for now

with Ada.Text_IO;
use Ada.Text_IO;

with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;

with Ada.Containers.Vectors;

procedure Main is

   -- for reading the file
   F : File_Type;
   package Positive_Vectors is new Ada.Containers.Vectors
   (
    Element_Type => Character,
    Index_Type   => Positive
   );

   -- variables related to policy
   Rule_1 , Rule_2 : Positive;
   Letter: Character;

   -- password data and statistics
   Number_Of_Passwords: Natural := 0;
   Password: Positive_Vectors.Vector;
   Valid_Passwords_Count: Natural := 0;
   Valid_Passwords_Position: Natural := 0;

   -- reads the policy and returns True if successful
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

   -- reads the file and stores the password into Password
   -- returns True iff successful
   function Read_Password(F: File_Type) return Boolean is
      Inchar: Character;
      Result: Boolean := False;
   begin
      if not End_Of_File(F) then
         Password.Clear;
         while not End_Of_Line(F) loop
            Get(F, Inchar);
            Password.Append(Inchar);
         end loop;
         Result := True;
      end if;
      return Result;
   end Read_Password;

   -- reads the file and checks the key letter's count satisfies policy
   --
   -- assumes that file's position is at beginning of password
   function Check_Password_Count return Boolean is
      Instances_Of_Key: Natural := 0;
   begin
      for C of Password loop
         if C = Letter then Instances_Of_Key := Instances_Of_Key + 1; end if;
      end loop;
      return Instances_Of_Key in Rule_1 .. Rule_2;
   end Check_Password_Count;

   -- reads the file and checks that the key letter's positions satisfy policy:
   -- Letter must appear in position Rule1 or position Rule2, but not both
   --
   -- assumes that F's position is at beginning of password
   function Check_Password_Position return Boolean is
      Result: Boolean := False;
      Current_Position: Natural := 0;
   begin
      for C of Password loop
         Current_Position := Current_Position + 1;
         if C = Letter
             and then ( Current_Position = Rule_1 or Current_Position = Rule_2 )
         then
            Result := not Result;
         end if;
      end loop;
      return Result;
   end Check_Password_Position;

begin

   -- read policy and passwords, checking each password against policy
   -- once it is read, then forget it

   Open( F, In_File, "/Users/user/common/Ada/AoC2020/Day2/input.txt" );
   while not End_Of_File(F) loop
      if Read_Policy(F) and then Read_Password(F) then
         if Check_Password_Count then
            Valid_Passwords_Count := Valid_Passwords_Count + 1;
         end if;
         if Check_Password_Position then
            Valid_Passwords_Position := Valid_Passwords_Position + 1;
         end if;
      end if;
   end loop;
   Close(F);

   Put(Number_Of_Passwords, 0); Put(" passwords total");
   New_Line(1);
   Put(Valid_Passwords_Count, 0); Put(" valid passwords by counting policy");
   New_Line(1);
   Put(Valid_Passwords_Position, 0); Put(" valid passwords by position policy");
   New_Line(1);

end Main;
