-- Advent of Code 2020, Day 7
--
-- John Perry
--
-- apologies for any bad Ada style
--
-- sorting out rules for luggage
--
-- each rule is one line, indicating what bags can contain other bags,
-- and how many of each
--
-- first exercise: determine how many bag colors can eventually contain
-- one shiny gold bag
--
-- second exercise: determine how many total bags
-- the shiny gold bag must contain

with Ada.Text_IO;
use Ada.Text_IO;

with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;

with Ada.Containers.Hashed_Sets;
with Ada.Containers.Ordered_Maps;

with Ada.Strings.Hash;
with Ada.Strings.Fixed;

with Ada.Containers.Vectors;

procedure Main is

   F: File_Type;

   -- each rule contains a quantity and description of bag

   subtype Bag_Description is String(1..20);

   type Bag_Entry is record
      Description: Bag_Description := ( others => ' ' );
      Quantity: Positive;
   end record;

   -- we keep a record of all possible bags described in the rules

   function Bag_Hash(Bag: Bag_Description) return Ada.Containers.Hash_Type is
   ( Ada.Strings.Hash(Bag) );

   package Bag_Set is new Ada.Containers.Hashed_Sets
   (
    Element_Type => Bag_Description,
    Hash => Bag_Hash,
    Equivalent_Elements => "="
   );

   All_Bags: Bag_Set.Set;

   -- each rule is indexed by its Bag_Description (above)
   -- and a set (below) of Bag_Entry's (above)

   function Bag_Entry_Hash(Key: Bag_Entry) return Ada.Containers.Hash_Type is
   ( Ada.Strings.Hash(Key.Description) );

   package Bag_Rules is new Ada.Containers.Hashed_Sets
   (
    Element_Type => Bag_Entry,
    Hash => Bag_Entry_Hash,
    Equivalent_Elements => "="
   );

   function "="(Left, Right: Bag_Rules.Set) return Boolean is
   -- True iff two sets of bag entries contain the same bag descriptions
   (
         ( for all Bag of Left => Right.Contains(Bag) )
         and
         ( for all Bag of Right => Left.Contains(Bag) )
   ); -- hey lookit, I remembered "for all" expressions
      -- kept forgetting "all" before

   package Map_Bags_To_Sets is new Ada.Containers.Ordered_Maps
   (
    Key_Type => Bag_Description,
    Element_Type => Bag_Rules.Set,
    "<" => "<",
    "=" => "="
   );

   All_Rules: Map_Bags_To_Sets.Map;

   -- matters related to the exercises

   Bags_That_Can_Contain_Shiny_Gold: Natural := 0;

   Shiny_Gold: constant Bag_Description := "shiny gold          ";

   function Found_Shiny_Gold(Rule: Bag_Rules.Set) return Boolean is
   -- True iff a bag of rule,
   -- or a bag that some such bag must eventually contain,
   -- is shiny gold
   (
         ( for some Clause of Rule => Clause.Description = Shiny_Gold )
         or else
         (
               for some Clause of Rule
                     => Found_Shiny_Gold( All_Rules(Clause.Description) )
         )
   );

   function Count_Contained_Bags(Kind: Bag_Description) return Natural is
   -- returns the number of bags that Kind contains recursively
      Result: Natural := 0;
   begin
      for Bag_Rule of All_Rules(Kind) loop
         Result := Result + Bag_Rule.Quantity
               + Bag_Rule.Quantity * Count_Contained_Bags(Bag_Rule.Description);
      end loop;
      return Result;
   end Count_Contained_Bags;

begin

   -- read in the rules before doing anything else

   Open(F, In_File, "/Users/user/common/Ada/AoC2020/Day7/input.txt");

   while not End_Of_File(F) loop

      -- first we find the position of "bag"
      -- this gives us the description of the bag
      -- whose rules are being described

      declare

         S: String := Get_Line(F);
         Pos: Positive := Ada.Strings.Fixed.Index(S, "bag", 1);
         Key: Bag_Description;
         Quantity: Natural;

      begin

         -- Put(S); New_Line(1);

         -- copy the bag description into Key

         for I in 1 .. Pos - 2 loop Key(I) := S(I); end loop;
         for I in Pos - 1 .. 20 loop Key(I) := ' '; end loop;

         -- if it doesn't exist, then add a rule for it
         if not All_Rules.Contains(Key) then
            All_Rules.Insert(Key, Bag_Rules.Empty_Set);
            All_Bags.Insert( Key );
         end if;

         -- find what it contains

         Pos := Ada.Strings.Fixed.Index(S, "contain", Pos);
         Pos := Pos + 8;

         -- loop through the bag quantities & descriptions

         while Pos <= S'Length loop

            -- originally I ignored the count of numbers
            -- I commented that I would regret this, and I indeed I did

            Quantity := 0; -- need to reset this each time!

            while S(Pos) = ' ' loop Pos := Pos + 1; end loop;
            while S(Pos) in '0' .. '9' loop
               Quantity := Quantity * 10
                     + Character'Pos(S(Pos)) - Character'Pos('0');
               Pos := Pos + 1;
            end loop;

            -- a quantity of 0 can exist when the bag contains bags"
            -- in this case, advance to the end of the line;
            -- otherwise, obtain the description

            if Quantity = 0 then

               Pos := S'Length + 1;

            else

               while S(Pos) = ' ' loop Pos := Pos + 1; end loop;

               declare

                  Second_Pos: Positive
                        := Ada.Strings.Fixed.Index(S, "bag", Pos);
                  New_Entry: Bag_Entry
                        := ( Quantity => Quantity, Description => <> );

               begin

                  -- initialize, record new value
                  New_Entry.Quantity := Quantity;
                  for I in Pos .. Second_Pos - 1 loop
                     New_Entry.Description(I - Pos + 1) := S(I);
                  end loop;
                  All_Rules(Key).Insert(New_Entry);

                  if not All_Rules.Contains(New_Entry.Description) then
                     All_Rules.Insert(New_Entry.Description, Bag_Rules.Empty_Set);
                     All_Bags.Insert(New_Entry.Description);
                  end if;

                  -- move along
                  Pos := Second_Pos + 3; -- "bag";
                  if S(Pos) = 's' then Pos := Pos + 1; end if;
                  Pos := Pos + 2; -- ", "

               end;

            end if;

         end loop;

      end;
   end loop;

   -- mostly useless diagnostic, but still interesting

   Put(All_Bags.Length'Image); Put(" kinds of bags"); New_Line(1);
   Put(All_Rules.Length'Image); Put(" rules on bags"); New_Line(1);
   New_Line(1);

   -- finally! exercise 1
   for Rule of All_Bags loop
      if Rule /= Shiny_Gold then
         Bags_That_Can_Contain_Shiny_Gold
               := Bags_That_Can_Contain_Shiny_Gold
                     + ( if Found_Shiny_Gold(All_Rules(Rule)) then 1 else 0);
      end if;
   end loop;

   Put(Bags_That_Can_Contain_Shiny_Gold, 0);
   Put(" bags can contain shiny gold");
   New_Line(1);

   -- and now exercise 2

   Put(Shiny_Gold(
       1..Ada.Strings.Fixed.Index(
             Shiny_Gold, " ", Ada.Strings.Fixed.Index(Shiny_Gold, " ", 1) + 1
                   )
             )
      );
   Put("must ");
   Put(Count_Contained_Bags(Shiny_Gold), 0);
   Put(" other bags");
   New_Line(1);

end Main;
