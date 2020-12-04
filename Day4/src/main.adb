-- Advent of Code 2020, Day 4
--
-- John Perry
--
-- apologies for any bad Ada style
--
-- hacking the passport checker
--
-- first exercise: count number of passports having all fields except cid
--
-- second exercise: count number of passports satisfying much stricter rules
--
-- this one was frustrating because no information was given on the file format,
-- and surprises arise here and there

with Ada.Text_IO;
use Ada.Text_IO;

with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;

with Ada.Containers.Vectors;

procedure Main is

   F: File_Type;

   -- used to store values of different sizes
   -- 12 seemed like a safe bet, and apparently it was
   subtype Uncertain_String is String(1..12);

   -- a person's height is recorded in inches, centimeters,
   -- ... and sometimes nothing!
   type Length_Units is ( Inches, Centimeters, Unknown );

   -- eye color stored two different ways
   type Color_Format is ( Name, Hex );

   -- we use this to store hair and eye color
   type Color_Specification is record
      Format: Color_Format;
      Value: Uncertain_String;
   end record;

   -- the following two types are used to track whether a passport's field
   -- has been initialized
   type Passport_Fields is ( Byrv, Cidv, Eclv, Eyrv, Hclv, Hgtv, Iyrv, Pidv );
   type Initialization_Array is array ( Passport_Fields ) of Boolean;

   -- the passport type; details inline
   type Passport is record

      Pid: Uncertain_String; -- passport ID #

      Byr,           -- birth year
      Cid,           -- country ID #
      Eyr,           -- expire year
      Hgt,           -- height
      Iyr: Natural;  -- issue year

      Hgt_Units: Length_Units; -- units for the height

      Eye_Color,
      Hair_Color: Color_Specification;

      Was_Initialized: Initialization_Array := ( others => False );

   end record;

   Invalid_Input : exception; -- just in case; proved useful for debugging

   -- the next two declarations are used to store passport data
   package Passport_List is new Ada.Containers.Vectors
   (
    Element_Type => Passport,
    Index_Type   => Positive
   );

   All_Passports : Passport_List.Vector;

   -- utility functions and procedures

   procedure Read_Uncertain_String(Destination: out Uncertain_String) is
   -- read a string of no more than 12 characters

      Inchar: Character;
      Eol: Boolean;
      I: Natural := 1;

   begin

      loop

         Look_Ahead(F, Inchar, Eol);

         if Inchar = ' ' or Eol then exit;
         else
            Get(F, Inchar);
            Destination(I) := Inchar;
            I := I + 1;
         end if;

      end loop;

      while I <= Destination'Last loop
         Destination(I) := ' ';
         I := I + 1;
      end loop;

   end Read_Uncertain_String;

   procedure Read_Color(Color: out Color_Specification) is
   -- read either an eye color or a hair color (or potentially a different one)
   -- if the first character read is '#' then we assume hexadecimal format;
   -- otherwise we assume name format

      Eol: Boolean;
      Inchar: Character;
      I: Natural := 1;

   begin

      -- determine the format, and remove '#' if present

      Look_Ahead(F, Inchar, Eol);

      if Inchar = '#' then
         Get(F, Inchar); -- '#'
         Color.Format := Hex;
      else
         Color.Format := Name;
      end if;

      Read_Uncertain_String(Color.Value);


   end Read_Color;

   procedure Put_Color(Color: Color_Specification) is
   -- print a color to default output; useful for debugging if nothing else

   begin

      case Color.Format is

         when Hex => Put('#'); Put(Color.Value);

         when Name =>
            Put(Color.Value(1)); Put(Color.Value(2)); Put(Color.Value(3));

      end case;

   end Put_Color;

   procedure Read_Passport is
   -- reads a passport record, stores it at the end of All_Passports,
   -- and marks which fields were initialized

      Data: Passport; -- temporary, copied to All_Passports
      Unit: String(1..2); -- used to read 'in' or 'cm'
      Eol, Finished : Boolean;
      Inchar1, Inchar2, Inchar3, Colon, Next_Char : Character;

   begin

      while not End_Of_Line(F) loop

         Get(F, Inchar1); Get(F, Inchar2); Get(F, Inchar3); Get(F, Colon);

         -- this parsing is a little dangerous but if it worked for VMS...

         case Inchar1 is

            when 'b' => -- byr

               Get(F, Data.Byr);
               Data.Was_Initialized(Byrv) := True;

            when 'c' => -- cid

               Get(F, Data.Cid);
               Data.Was_Initialized(Cidv) := True;

            when 'e' => -- eyr, ecl

               case Inchar2 is

                  when 'y' =>

                     Get(F, Data.Eyr);
                     Data.Was_Initialized(Eyrv) := True;

                  when 'c' =>

                     Read_Color(Data.Eye_Color);
                     Data.Was_Initialized(Eclv) := True;

                  when others =>

                     raise Invalid_Input with All_Passports.Length'Image;

               end case;

            when 'i' => -- iyr

               Get(F, Data.Iyr);
               Data.Was_Initialized(Iyrv) := True;

            when 'h' => -- hgt, hcl

               case Inchar2 is

                  when 'g' =>

                     Get(F, Data.Hgt);
                     Look_Ahead(F, Next_Char, Eol);

                     if Eol or Next_Char = ' ' then Data.Hgt_Units := Unknown;

                     else

                        Get(F, Unit);

                        if Unit(Unit'First) = 'i' then
                           Data.Hgt_Units := Inches;
                        elsif Unit(Unit'First) = 'c' then
                           Data.Hgt_Units := Centimeters;
                        else Data.Hgt_Units := Unknown;
                        end if;

                     end if;

                     Data.Was_Initialized(Hgtv) := True;

                  when 'c' =>

                     Read_Color(Data.Hair_Color);
                     Data.Was_Initialized(Hclv) := True;

                  when others =>

                     raise Invalid_Input with All_Passports.Length'Image;

               end case;

            when 'p' => -- pid

               Read_Uncertain_String(Data.Pid);
               Data.Was_Initialized(Pidv) := True;

            when others =>

               raise Invalid_Input with All_Passports.Length'Image;

         end case;

         if not End_Of_File(F) then

            -- Text_IO's Get typically gobbles white space, which made this hard
            -- to parse (I really ought to look into a more abstract method) so
            -- I resorted to Get_Immediate and Look_Ahead with End-of-line to
            -- determine when we have a double-skip

            loop

               Look_Ahead(F, Next_Char, Eol);

               if Next_Char = ' ' then
                  Get(F, Next_Char);
               elsif Eol then
                  Get_Immediate(F, Next_Char);
                  Look_Ahead(F, Next_Char, Eol);
                  if Eol then Finished := True; end if;
               else exit;
               end if;

            end loop;

            if Finished then exit; end if;

         end if;

      end loop;

      All_Passports.Append(Data);

   end Read_Passport;

   function All_Initialized_But_Cid(P: Passport) return Boolean is
   -- validity check for exercise 1: all field initialized except maybe Cid
   (
      P.Was_Initialized(Byrv) and P.Was_Initialized(Eclv) and
      P.Was_Initialized(Eyrv) and P.Was_Initialized(Hclv) and
      P.Was_Initialized(Hgtv) and P.Was_Initialized(Iyrv) and
      P.Was_Initialized(Pidv)
   );

   function Valid_Hair_Color(P: Passport) return Boolean is
   -- test whether P lists a valid hair color, which means
   -- it is hexadecimal with six hexadigits

      Result: Boolean := False;
      Color: constant Color_Specification := P.Hair_Color;

   begin

      if Color.Format = Hex then

         Result := True;

         for I in 1 .. 12 loop

            if I <= 6 and not (
               Color.Value(I) in '0'..'9' or
               Color.Value(I) in 'a'..'f'
            ) then
               Result := False;
               exit;
            elsif I > 6 and Color.Value(I) /= ' ' then
               Result := False;
               exit;
            end if;

         end loop;

      end if;

      return Result;

   end Valid_Hair_Color;

   function Valid_Eye_Color(P: Passport) return Boolean is
   -- test whether P lists a valid eye color, which means
   -- it is one of a very small set of names

      Result: Boolean := False;
      Color: Color_Specification := P.Eye_Color;

   begin

      if Color.Format = Name then

         if Color.Value(1..3) = "amb" or
               Color.Value(1..3) = "blu" or
               Color.Value(1..3) = "brn" or
               Color.Value(1..3) = "gry" or
               Color.Value(1..3) = "grn" or
               Color.Value(1..3) = "hzl" or
               Color.Value(1..3) = "oth"
         then
            Result := True;
         end if;

      end if;

      return Result;

   end Valid_Eye_Color;

   function Valid_Passport_Id(P: Passport) return Boolean is
   -- check whether a Passport's ID is valid, which means that
   -- it's a 9-digit number, including leading 0's

      Result: Boolean := True;

   begin

      for I in 1 .. 9 loop
         Result := Result and P.Pid(I) in '0'..'9';
      end loop;

      for I in 10..12 loop
         Result := Result and P.Pid(I) in ' ';
      end loop;

      return Result;

   end Valid_Passport_Id;

   function Strict_Rules(P: Passport) return Boolean is
   -- check the passport according to much stricter rules
   (
      P.Byr in 1920 .. 2002 and
      P.Iyr in 2010 .. 2020 and
      P.Eyr in 2020 .. 2030 and
      (
       if P.Hgt_Units = Inches then P.Hgt in 59 .. 76
       elsif P.Hgt_Units = Centimeters then P.Hgt in 150 .. 193
       else False
      ) and
      Valid_Hair_Color(P) and
      Valid_Eye_Color(P) and
      Valid_Passport_Id(P)
   );

   -- type for passing a validity function
   type Validity_Function is access function(P: Passport) return Boolean;

   function Number_Of_Valid_Passports(
         Valid: Validity_Function; Verbose: Boolean := False
   )
   return Natural
   is
   -- determine the number of passports that pass the test imposed by Valid
   -- this also reports which passports fail the test

      Result: Natural := 0;
      Passport_Number: Natural := 0;

   begin

      for Passport of All_Passports loop

         Passport_Number := Passport_Number + 1;

         if Valid(Passport) then
            Result := Result + 1;
         elsif Verbose then
            Put(Passport_Number, 0); Put(" invalid ");
            New_Line(1);
         end if;

      end loop;

      return Result;

   end Number_Of_Valid_Passports;

-- main program!
begin

   Open(F, In_File, "/Users/user/common/Ada/AoC2020/Day4/input.txt");
   while not End_Of_File(F) loop
      Read_Passport;
   end loop;
   Close(F);

   Put(All_Passports.Length'Image); Put(" passports read"); New_Line(1);

   Put(Number_Of_Valid_Passports(All_Initialized_But_Cid'Access));
   Put(" passports are valid");
   New_Line(1);

   Put(Number_Of_Valid_Passports(Strict_Rules'Access));
   Put(" passports are valid by strict rules");
   New_Line(1);

end Main;
