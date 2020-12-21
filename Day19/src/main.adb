-- Advent of Code 2020, Day 19
--
-- John Perry
--
-- apologies for any bad Ada style
--
-- Monster Messages
--
-- part 1: identify messages that don't satisfy the rules of a given grammar;
-- the rules are non-looping
--
-- part 2: identify messages that satisfy the rules of a given grammar after
-- "correcting" a couple of rules to looping
--
-- this gave me a devil of a time, what on account of

pragma Ada_2020;

with Ada.Text_IO;
use Ada.Text_IO;

with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;

with Ada.Strings.Fixed;

with Ada.Strings.Maps;

with Ada.Containers.Hashed_Maps;

with Ada.Containers.Hashed_Sets;

procedure Main is

   F: File_Type; -- input file

   -- we define a type for each rule, listed below
   --
   -- each rule can be:
   --    - terminal, where the rule is replaced by a character
   --    - nonterminal, where the rule has one or more options of
   --      references, where a reference is another rule
   --
   -- examples from my input:
   --
   --    29: 116 82 | 119 24   <-- nonterminal, two options, each w/2 references
   --    116: "a"              <-- terminal
   --
   -- I was unsure of how the problem would change, so I allowed
   -- up to 3 options per rule, up to 3 references per option,
   -- and any character to be terminal

   Max_Options: constant := 3;
   Max_Refs_Per_Option: constant := 3;

   -- Potential_Options is the number of options each rule may have
   subtype Potential_Options is Positive range 1 .. Max_Options;
   -- Potential_References is the number of references each option has
   -- when the option is actually a reference
   subtype Potential_References is Positive range 1 .. Max_Refs_Per_Option;
   -- Potential_Number_Of_References is the number of references
   -- each option has, including when it's an invalid option
   -- (e.g., a rule has only two options instead of three, so the third
   -- has 0 references)
   subtype Potential_Number_Of_References
         is Natural range 0 .. Max_Refs_Per_Option;

   -- a list of references has options, and each option has references
   type Reference_List
         is array ( Potential_Options , Potential_References ) of Natural;
   -- we use this to keep track of the number of references in each option
   -- this was kind of dumb; I should simply have made an option type,
   -- which would be a component of each rule,
   -- and each option tracks its own number of references
   type Reference_Length_Array
         is array ( Potential_References ) of Potential_Number_Of_References;

   -- the type for a rule:
   --    - Is_Reference tells us whether the Rule is a reference
   --    - if so, then there will be options:
   --       - Options is the number of options
   --       - Num_Refs is the number of in each option
   --       - References is the list of Options and References
   --      so, for instance, the example above, 29: 116 82 | 119 24, will have
   --         Options => 2
   --         Num_Refs => ( 2, 2, 0 );
   --         References
   --            => ( ( 116, 82, junk ) , ( 119, 24, junk ), ( junk, junk, junk)
   --
   --    - if not a reference, then Value tells us the character that replaces
   --      this rule
   type Rule(Is_Reference: Boolean := True) is record
      case Is_Reference is
      when True =>
         Options: Potential_Options := 1;
         Num_Refs: Reference_Length_Array := ( others => 0 );
         References: Reference_List;
      when False =>
         Value: Character;
      end case;
   end record;

   subtype Terminal_Rule is Rule( Is_Reference => False );
   subtype Nonterminal_Rule is Rule( Is_Reference => True );

   -- rules are hashed according to their number, which can be from 0 .. ???
   -- in retrospect I could have used a vector and sorted it
   -- maybe one day I'll revisit this & do that

   function Hash_Natural(N: Natural) return Ada.Containers.Hash_Type is
   ( Ada.Containers.Hash_Type(N) );

   package Rule_Maps is new Ada.Containers.Hashed_Maps
   (
    Key_Type        => Natural,
    Element_Type    => Rule,
    Hash            => Hash_Natural,
    Equivalent_Keys => "="
   );

   Rule_Map: Rule_Maps.Map;

   -- for some mysterious reason GNAT chokes on Get(String, Natural, Positive)
   -- when String = "29:"; i.e., the colon makes it cry, so I had to define
   -- the following function

   function Read_Natural(S: String; Pos: in out Natural) return Natural is
   -- reads and returns a natural number from S, reading ONLY digits,
   -- starting at Pos
      Result: Natural := 0;
   begin
      while S(Pos) in '0' .. '9' loop
         Result := Result * 10;
         Result := Result + Character'Pos(S(Pos)) - Character'Pos('0');
         Pos := Pos + 1;
      end loop;
      return Result;
   end Read_Natural;

   procedure Put(R: Rule) is
   -- displays R in the form
   --    if value: "value"
   --    if ref  : "<refs for opt 1> | <refs for opt 2> | <refs for opt 3> |"
   -- but in the latter case displays only valid refs
   begin
      if R.Is_Reference then
         for O in 1 .. R.Options loop
            for N in 1 .. R.Num_Refs(O) loop
               Put(R.References(O, N), 0); Put(' ');
            end loop;
            Put("| ");
         end loop;
      else
         Put(R.Value);
      end if;
   end Put;

   procedure Add_Rule_From(S: String) is
   -- reads a rule from S, specified in the manner above ("examples from...")

      Rule_Number: Natural;
      Last: Natural := S'First;

   begin

      -- uh, why is this giving me a data error? this should work!
      -- (asked on comp.lang.ada)
      -- Get(S, Rule_Number, Last);
      -- Last := Last + 3;

      -- rule number
      Rule_Number := Read_Natural(S, Last);
      -- skip colon & space
      Last := Last + 2;

      -- if it's terminal, read the value
      if S(Last) = '"' then

         declare New_Rule: Rule
                  := ( Is_Reference => False , Value => S( Last + 1 ) );
         begin
            Rule_Map.Insert(Rule_Number, New_Rule);
         end;

      -- otherwise, it's a list of options; read and store
      else

         declare

            -- current option
            Option: Positive := 1;
            -- current reference number
            Reference_Number: Natural := 0;
            -- our new rule with references!
            New_Rule: Rule
                  := (
                      Is_Reference => True, Options => <>,
                      Num_Refs => <>, References => <>
                     );

         begin

            -- read options
            while Last <= S'Last loop

               -- read references for each option
               while Last <= S'Last and then S(Last) /= '|' loop

                  Reference_Number := Reference_Number + 1;
                  Get(
                      S(Last .. S'Last),
                      New_Rule.References(Option, Reference_Number),
                      Last
                     );
                  Last := Last + 2;

               end loop;

               New_Rule.Num_Refs(Option) := Reference_Number;
               Option := Option + 1;
               -- reset for next option
               Reference_Number := 0;

               -- skip over space and |
               -- this is careless; effectively I'm counting on there being
               -- more options iff I haven't read the whole line yet, rather
               -- than acting on there being a '|', so the input would be
               -- malformed
               -- it wouldn't take much to fix this, but I've spent too long on
               -- this already
               while Last <= S'Last and then ( S(Last) = ' ' or S(Last) = '|' )
               loop
                  Last := Last + 1;
               end loop;

            end loop;

            -- New_Rule.Options always points to the next available spot, so
            -- once creation is done we need to adjust
            New_Rule.Options := Option - 1;
            Rule_Map.Insert(Rule_Number, New_Rule);

         end;

      end if;

   end Add_Rule_From;

   -- CORRECT IMPLEMENTAITON OF A PARSER
   -- (correct enough to get my gold stars, anyway)
   -- incorrect implementation remains below

   -- my approach was to test the rules recursively
   -- when an option works, I store the positions (plural!) at which may work
   --
   -- the plural of positions is necessary because options can be or'd:
   -- one option may terminate correctly at one position, but a different option
   -- may terminate correctly at a different position
   --
   -- and's do not have this issue; if they start with only one correct position
   -- then they end with only one correct position

   package Position_Sets is new Ada.Containers.Hashed_Sets
   (
    Element_Type => Positive,
    Hash => Hash_Natural,
    Equivalent_Elements => "="
   );

   -- forward declaration; see actual definitions for details

   function Matches_One(
      S: String;
      Pos: Positive;
      R: Rule;
      Level: Positive := 1
   )
   return Position_Sets.Set;

   function Matches_All(
      S: String;
      Pos: Positive;
      Rule: Reference_List;
      Num_Refs: Natural;
      Option: Positive;
      Level: Positive := 1
   )
   -- given S, test whether the rules *referenced* by Rule at option Option,
   -- which has Num_Refs valid references (i.e. Rule(Option, Num_Refs + 1)
   -- is junk), are satisfied
   -- Level is used only for debugging, which is commented out

   return Position_Sets.Set

   is

      I: Positive := 1; -- which reference we consider in Rule(Option, ...)

      -- positions that we are testing at reference I
      Old_Positions: Position_Sets.Set;
      -- positions verified at reference I
      New_Positions: Position_Sets.Set;

   begin

      New_Positions.Insert(Pos);

      -- debugging
--        for J in 1 .. Level loop Put(' '); end loop;
--        Put("and ");
--        for J in 1 .. Num_Refs loop Put(Rule(Option, J), 0); Put(' '); end loop;
--        Put(S(Pos .. S'Last));
--        New_Line;

      -- keep testing until either we run out of potential options
      -- or we run out of positions that were valid heretofore

      while Natural(New_Positions.Length) > 0 and then I <= Num_Refs loop

         Old_Positions := New_Positions;
         New_Positions.Clear;

         if not Rule_Map(Rule(Option, I)).Is_Reference then

            -- check which positions work with the next terminal rule

            for P of Old_Positions loop
               if P <= S'Length and S(P) = Rule_Map(Rule(Option, I)).Value then
                  New_Positions.Insert(P + 1);
               end if;
            end loop;

         else

            -- check which positions work with one of the options
            -- of the referenced positions (keep all that work)

            for P of Old_Positions loop
               New_Positions
                     := New_Positions.Union(
                           Matches_One(
                                 S, P, Rule_Map(Rule(Option, I)), Level + 1
                           )
                     );
            end loop;

         end if;

         -- debugging
--           for J in 1 .. Level loop Put(' '); end loop;
--           Put("all? " );
--           for J in 1 .. Num_Refs loop Put(Rule(Option, J), 0); Put(' '); end loop;
--           Put(" ( "); Put(I, 0); Put(" ) { ");
--           Put(New_Positions'Image); Put(" } "); New_Line;

         I := I + 1; -- I wish Ada would warn when this doesn't happen

      end loop;

      return New_Positions;

   end Matches_All;

   function Matches_One(
      S: String;
      Pos: Positive;
      R: Rule;
      Level: Positive := 1
   )
   -- given S, test whether one of rule R's options is satisfied at position Pos
   -- Level is used only for debugging, which is commented out

   return Position_Sets.Set

   is

      I: Positive := 1; -- which option we are considering in Rule

      -- positions verified at option I
      New_Positions: Position_Sets.Set;

   begin


      -- debugging
--        for J in 1 .. Level loop Put(' '); end loop;
--        Put("or "); Put(R); Put(' '); Put(S(Pos .. S'Last)); New_Line;

      -- the implementation is built in such a way that
      -- we never enter Matches_One unless R.Is_Reference is true!
      -- (I only noticed while documenting, when an obvious bug never occurred)
      -- so we merely need to check each option, which will be a list of refs,
      -- and in each option all the refs must be true, so we call Matches_All
      -- on the option

      while Pos <= S'Length and then I <= R.Options loop

         -- debugging
--              for J in 1 .. Level loop Put(' '); end loop;
--              Put(I, 0); Put(" [ or "); Put(R); Put(' '); Put(S(Pos .. S'Last));
--              Put(" ]"); New_Line;

         New_Positions
               := New_Positions.Union(
                                      Matches_All(
                                            S, Pos,
                                            R.References, R.Num_Refs(I), I,
                                            Level + 1
                                      )
                                     );
         I := I + 1;

      end loop;

--        for J in 1 .. Level loop Put(' '); end loop;
--        Put("one? "); Put(R); Put(' '); Put(Pos, 0);
--        Put(" { "); Put(New_Positions'Image); Put(" } ");
--        New_Line;

      return New_Positions;

   end Matches_One;

   -- the following is an INCORRECT IMPLEMENTATION that could possibly be fixed
   -- to a correct one, especially now that I have the Position_Sets stuff that
   -- I introduced only very late in the game
   --
   -- this actually works for Part 1

   function Matches_Rule(
      S: String;
      Pos: in out Positive;
      Rule: Natural;
      Level: Positive := 1
   )
   -- checks that the rule indexed by Rule is satisfied by the substring of S
   -- starting at position Pos
   -- Level is used for debugging but also to check the final answer is True
   -- only if we have exhausted the string

   return Boolean

   is

      Found_Match: Boolean := False;
      Current_Rule: natural := Rule;

   begin

      -- for a non-terminal rule, check the references
      -- this is erroneous because it short-circuits, but I'd have to think
      -- about how to fix it, and rather than do that I just rewrote it as above

      if Rule_Map(Current_Rule).Is_Reference then

         declare

            R: Nonterminal_Rule := Rule_Map(Current_Rule);
            New_Pos: Positive;

         begin

            for Option in 1 .. R.Options loop

               New_Pos := Pos;
               Found_Match := True;

               for I in 1 .. R.Num_Refs(Option) loop

                  -- debugging
--                    for I in 1 .. Level loop Put(' '); end loop;
--                    Put(R.References(Option, I)); New_Line;

                  if not Matches_Rule(
                                      S, New_Pos,
                                      R.References(Option, I), Level + 1
                                     )
                  then
                     Found_Match := False;
                     exit;
                  end if;

                  if not Found_Match then exit; end if;

               end loop;

               if Found_Match and (Level > 1 or New_Pos > S'Last) then
                  Pos := New_Pos; exit;
               end if;

            end loop;

         end;

      else

         -- terminal: evaluate S(Pos) with the terminal value

         if Pos <= S'Last and then S(Pos) = Rule_Map(Current_Rule).Value then
            Found_Match := True;
            Pos := Pos + 1;
         end if;

      end if;

      -- if this is a top-level verification, make sure we've exhausted S

      if Found_Match and Level = 1 then
         Found_Match := Pos > S'Last;
      end if;

      -- debugging
--        if Found_Match then
--           for I in 1 .. Level - 1 loop Put(' '); end loop;
--           Put(Level, 0); Put(" matches ");
--           Put(S(S'First .. Pos - 1));
--           New_Line;
--        end if;

      return Found_Match;

   end Matches_Rule;

   Filename: constant String
         := "/Users/user/common/Ada/AoC2020/Day19/input.txt";
   --      := "/Users/user/common/Ada/AoC2020/Day19/example.txt";
   --      := "/Users/user/common/Ada/AoC2020/Day19/example2.txt";

begin

   Open(F, In_File, Filename);

   -- read rules

   loop
      declare
         S: String := Get_Line(F);
      begin
         if S = "" then exit; end if;
         Add_Rule_From(S);
      end;
   end loop;

   -- part 1: read and verify messages

   declare

      Number_Of_Rules_Matching_0: Natural := 0; -- first implementation count
      Number_Of_Rules_Second: Natural := 0; -- second implementation count

   begin

      while not End_Of_File(F) loop

         declare

            S: String := Get_Line(F);
            Pos: Positive := S'First;
            First_Worked: Boolean := False;

         begin

            if Matches_Rule(S, Pos, 0) then
               First_Worked := True;
               Number_Of_Rules_Matching_0 := Number_Of_Rules_Matching_0 + 1;
            end if;

            -- recheck with the second implementation
            -- Matches_One returns the positions AFTER the substring(s)
            -- that satisfy the given rules
            -- to make sure that we've used S entirely, we don't check here
            -- whether the result contains a position, but rather that one
            -- of the positions returned is S'Length + 1; that is,
            -- the position you'd expect if you'd exhausted S
            Pos := 1;
            if Matches_One(S, Pos, Rule_Map(0), 1).Contains(S'Length + 1) then
               -- debugging
               -- Put("detected "); Put(S); New_Line;
               Number_Of_Rules_Second := Number_Of_Rules_Second + 1;
            end if;

         end;

      end loop;

      Put("incorrect implementation: ");
      Put(Number_Of_Rules_Matching_0, 0); New_Line;
      Put("correct implementation: ");
      Put(Number_Of_Rules_Second, 0); New_Line;

   end;

   Close(F);

   -- part 2: adjust rules to include a loop, and re-check

   declare

      -- new definitions of rules 8 and 11

      New_Rule_8: Rule
            := (
                Is_Reference => True,
                Options => 2,
                Num_Refs => ( 1, 2, 0 ),
                References => ( ( 42, 0, 0 ), ( 42, 8, 0 ), ( 0, 0, 0 ) )
               );

      New_Rule_11: Rule
            := (
                Is_Reference => True,
                Options => 2,
                Num_Refs => ( 2, 3, 0 ),
                References => ( ( 42, 31, 0 ), ( 42, 11, 31 ), ( 0, 0, 0 ) )
               );

   begin
      -- do the deed

      Rule_Map.Replace( 8, New_Rule_8);
      Rule_Map.Replace( 11, New_Rule_11);

   end;

   declare

      Number_Of_Rules_Matching_0: Natural := 0; -- first implementation count
      Number_Alternate: Natural := 0; -- second implementation count

   begin

      Open(F, In_File, Filename);

      -- skip the rule definitions

      loop
         declare S: String := Get_Line(F);
         begin
            exit when S = "";
         end;
      end loop;

      -- reread the messages; check against new rules

      while not End_Of_File(F) loop

         declare

            S: String := Get_Line(F);
            Pos: Positive := S'First;

         begin

            if Matches_Rule(S, Pos, 0) then
               -- debugging
               -- Put(S); New_Line;
               Number_Of_Rules_Matching_0 := Number_Of_Rules_Matching_0 + 1;
            end if;

            Pos := S'First;
             if Matches_One(S, Pos, Rule_Map(0), 1).Contains(S'Length + 1) then
               -- debugging
               -- Put("detected "); Put(S); New_Line;
               Number_Alternate := Number_Alternate + 1;
            end if;

        end;

      end loop;

      Put("incorrect implementation: ");
      Put(Number_Of_Rules_Matching_0, 0); New_Line;
      Put("correct implementation: ");
      Put(Number_Alternate, 0); New_Line;

      Close(F);
   end;

end Main;
