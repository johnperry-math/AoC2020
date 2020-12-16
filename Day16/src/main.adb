-- Advent of Code 2020, Day 16
--
-- John Perry
--
-- apologies for any bad Ada style
--
-- Ticket Translation
--
-- part 1: identify tickets that don't belong to your group of tickets,
-- because they can't possibly satisfy the rules
--
-- part 2: identify how the rules (which were given out of order)
-- match up to the order given

pragma Ada_2020;

with Ada.Text_IO;
use Ada.Text_IO;

with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;

with Ada.Strings.Fixed;

with Ada.Strings.Maps;

with Ada.Containers.Vectors;

procedure Main is

   F: File_Type; -- input file

   -- the rule has a label (none longer than 18 characters or so)
   -- and two bounds on what its value can be

   type Label_String is new String(1..20);

   type Ticket_Rule is record
      Label: Label_String := ( others => ' ' );
      Min1, Min2, Max1, Max2: Natural;
      Located: Boolean := False;
   end record;

   -- storage of rules

   package Ticket_Rule_Vector is new Ada.Containers.Vectors
   (
    Element_Type => Ticket_Rule,
    Index_Type   => Positive
   );

   Ticket_Rules: Ticket_Rule_Vector.Vector;

   -- reading the rules
   -- I'll follow Maxim Reznik's lead and use Ada.Strings.Fixed.Get_Token
   -- at least for the first part

   package ASM renames Ada.Strings.Maps;
   package ASF renames Ada.Strings.Fixed;

   Colon: Asm.Character_Set := Asm.To_Set(":");

   procedure Read_Rules is
   -- reads all the rules from the input file
   -- first searches for the colon, since that is the rule's label
   -- after that, the format is pretty straightforward: read four integers
   -- that indicate minimum and maximum values
   begin
      loop
         declare
            S: String := Get_Line(F);
            New_Rule: Ticket_Rule;
            First: Positive;
            Last: Natural;
            I: Natural := S'First;
         begin
            exit when S = "";
            -- get the rule label
            Asf.Find_Token(S, Colon, Ada.Strings.Outside, First, Last);
            Asf.Move(S(First..Last), String(New_Rule.Label));
            -- now read the rules in
            Get(S(Last+3..S'Last), New_Rule.Min1, Last);
            Get(S(Last+2..S'Last), New_Rule.Max1, Last);
            Get(S(Last+5..S'Last), New_Rule.Min2, Last);
            Get(S(Last+2..S'Last), New_Rule.Max2, Last);
            Ticket_Rules.Append(New_Rule);
         end;
      end loop;
   end Read_Rules;

   -- we don't need anything special for tickets

   type Ticket is array ( Positive range <> ) of Natural;

   function Read_Ticket return Ticket is
   -- read a ticket from F
      S: String := Get_Line(F);
      Number_Of_Rules renames Positive(Ticket_Rules.Length);
      Result: Ticket(1..Number_Of_Rules);
      Last: Natural := S'First; -- for processing integeres from S via Get
   begin
      for I in 1 .. Number_Of_Rules loop
         Get( S( Last .. S'Last ), Result(I), Last );
         Last := Last + 2;
      end loop;
      return Result;
   end Read_Ticket;

   function Read_Your_Ticket return Ticket is
   -- finds and reads "your" ticket
   begin
      loop
         declare S: String := Get_Line(F);
         begin
            exit when S = "your ticket:";
         end;
      end loop;
      return Read_Ticket;
   end;

   -- I found it useful to use an array of boolean

   type Boolean_List is array ( Positive range <> ) of Boolean with Pack;

   function Number_True( A: Boolean_List ) return Natural is
   -- returns how many indices of A are "true"
      Result: Natural := 0;
   begin
      for Each of A loop
         if Each then Result := Result + 1; end if;
      end loop;
      return Result;
   end Number_True;

   function Which_True( A: Boolean_List ) return Positive is
   -- returns the first index of A that is true
      Result: Positive := 1;
   begin
      for I in A'First .. A'Last loop
         if A(I) then
            Result := I;
            exit;
         end if;
      end loop;
      return Result;
   end Which_True;

begin

   Open(F, In_File, "/Users/user/common/Ada/AoC2020/Day16/input.txt");

   -- first read the rules

   Read_Rules;

   -- we'll go ahead and print them

   for Rule of Ticket_Rules loop
      Put(String(Rule.Label)); Put(": ");
      Put(Rule.Min1, 0); Put('-'); Put(Rule.Max1, 0); Put(" or ");
      Put(Rule.Min2, 0); Put('-'); Put(Rule.Max2, 0); New_Line(1);
   end loop;

   Put("there are "); Put(Ticket_Rules.Length'Image); Put(" rules");
   New_Line(1);

   -- read the tickets
   -- this declare doesn't end until the bitter end!

   declare

      Your_Ticket: Ticket := Read_Your_Ticket;

      Number_Of_Rules renames Positive(Ticket_Rules.Length);
      subtype Actual_Ticket is Ticket ( 1 .. Number_Of_Rules );

      package Tickets_Vector is new Ada.Containers.Vectors
            (
             Element_Type => Actual_Ticket,
             Index_Type   => Positive
            );

      Other_Tickets: Tickets_Vector.Vector;

   begin

      Put("your ticket: "); Put(Your_Ticket'Image); New_Line(1);

      -- part 1: read other tickets, identify erroneous ones

      -- first skip some fluff

      loop
         declare S: String := Get_Line(F);
         begin
            exit when S = "nearby tickets:";
         end;
      end loop;

      -- now read the other tickets, report the "error rate",
      -- which is the sum of invalid values on invalid tickets

      declare

         Error_Rate: Natural := 0;
         Neighbors_Ticket: Ticket(1 .. Number_Of_Rules);
         Valid: Boolean;

      begin

         Through_Other_Tickets:
         while not End_Of_File(F) loop

            Neighbors_Ticket := Read_Ticket;

            -- we'll check it as soon as we read it
            -- that way we discard invalid tickets immediately

            Check_Ticket:
            for I of Neighbors_Ticket loop

               Valid := False;

               for Rule of Ticket_Rules loop
                  if ( I >= Rule.Min1 and I <= Rule.Max1 )
                        or ( I >= Rule.Min2 and I <= Rule.Max2 )
                  then
                     Valid := True;
                     exit;
                  end if;
               end loop;

               if not Valid then
                  Error_Rate := Error_Rate + I;
                  exit;
               end if;

            end loop Check_Ticket;

            if Valid then
               Other_Tickets.Append(Neighbors_Ticket);
            end if;

         end loop Through_Other_Tickets;

         Put("error rate is "); Put(Error_Rate, 0);
         New_Line(1);

      end;

      -- part 2: identify correct field index from the valid tickets

      declare

         -- matching rules to positions
         type Boolean_Double_Array is
               array ( 1 .. Number_Of_Rules ) of
                     Boolean_List ( 1 .. Number_Of_Rules ) with pack;
         Rule_Matchup: Boolean_Double_Array := ( others => ( others => True ) );

         Position: Positive; -- a counter for a while loop

         Product: Long_Integer := 1; -- the result of part 2

         procedure Report_Rule_Possibilities(A: Boolean_Double_Array) is
         -- a convenience function that allows us to show vividly
         -- how we narrow the list down
         begin
            for I in 1 .. Number_Of_Rules loop
               if not Ticket_Rules(I).Located then
                  Put(String(Ticket_Rules(I).Label)); Put(" can go to ");
                  for J in 1 .. Number_Of_Rules loop
                     Put( (
                          if A(I)(J) then J'Image & " " else ""
                         ) );
                  end loop;
                  New_Line(1);
               end if;
            end loop;
         end Report_Rule_Possibilities;

      begin

         -- figure out which positions each rule can actually fit into,
         -- using neighbors' valid tickets to eliminate positions

         for Rule_Number in 1 .. Number_Of_Rules loop

            Position := 1;

            while Position <= Number_Of_Rules loop
               for Neighbors_Ticket of Other_Tickets loop
                  declare
                     Rule: Ticket_Rule_Vector.Reference_Type
                           renames Ticket_Rules(Rule_Number);
                     Value renames Neighbors_Ticket(Position);
                  begin
                     if (Value < Rule.Min1 or Value > Rule.Max1)
                           and (Value < Rule.Min2 or Value > Rule.Max2)
                     then
                        Rule_Matchup(Rule_Number)(Position) := False;
                        exit;
                     end if;
                  end;
               end loop;
               Position := Position + 1;
            end loop;

         end loop;

         -- kind of interesting to see that most rules can fit into
         -- several positions (and my first attempt failed because of this)
         Report_Rule_Possibilities(Rule_Matchup);
         New_Line(2);

         -- by process of elimination, we should be able to figure out
         -- where each rule goes

         Position := 1;
         while Position <= Rule_Matchup'Length loop

            if Ticket_Rules(Position).Located then

               -- we've already located this rule, so move on

               Position := Position + 1;

            else

               -- a rule is located if there's only one possible position for it

               if Number_True( Rule_Matchup(Position) ) /= 1 then

                  -- this rule hasn't been narrowed down yet; move on

                  Position := Position + 1;

               else

                  -- we've now located this rule
                  -- record it, then eliminate the position for the other rules

                  Ticket_Rules(Position).Located := True;

                  declare
                     Which: Positive := Which_True( Rule_Matchup(Position) );

                  begin

                     Put(String(Ticket_Rules(Position).Label));
                     Put(" must go to "); Put(Which, 0);
                     New_Line(1);

                     if Ticket_Rules(Position).Label(1..9) = "departure" then
                        Product := Product * Long_Integer(Your_Ticket(Which));
                     end if;

                     for I in 1 .. Number_Of_Rules loop
                        if I /= Position then
                           Rule_Matchup(I)(Which) := False;
                           -- this next line will also work, and is kind of cute
                           -- Rule_Matchup(I) := Rule_Matchup(I) and
                           --       not Rule_Matchup(Position);
                        end if;
                     end loop;

                     -- nice to see the progress through the list
                     Report_Rule_Possibilities(Rule_Matchup);
                     New_Line(2);

                  end;

                  -- restart the search
                  Position := 1;

               end if;

            end if;

         end loop;

         -- done!
         Put("product is "); Put(Product'Image); New_Line(1);

      end;

   end;

   -- we could perhaps close this earlier...
   Close(F);

end Main;
