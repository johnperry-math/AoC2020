-- Advent of Code 2020, Day 22
--
-- John Perry
--
-- apologies for any bad Ada style
--
-- Crab Combat
--
-- play a game of War (?) against a crab
--
-- part 1: play, report score
--
-- part 2: play recursively, according to new rules, report score
--

pragma Ada_2020;

with Ada.Text_IO;
use Ada.Text_IO;

with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;

with Ada.Containers.Hashed_Sets;
with Ada.Containers.Ordered_Sets;

with Ada.Containers.Vectors;

with Ada.Containers.Generic_Array_Sort;

procedure Main is

   -- SECTION
   -- input-related

   F: File_Type; -- input file

   Testing: constant Boolean := False;

   Filename: constant String
         := (
             if Testing then "/Users/user/common/Ada/AoC2020/Day22/example.txt"
             else "/Users/user/common/Ada/AoC2020/Day22/input.txt"
            );

   -- SECTION
   -- player's decks

   -- I wasn't able to get the library's Queues to work the way I'd like
   -- (or even to work at all I think) so I implemented my own

   -- it looks as if the game can be up to 50 cards;
   -- the example is only 10 though

   type Mod52 is mod ( if Testing then 12 else 52 );
   type Card_Array is array ( Mod52 ) of Natural;

   type Card_Deck is record
   -- treat it like a queue, should probably use a tagged record / class
   -- but not in the mood
      Cards: Card_Array;
      Top, Bottom: Mod52 := 0;
   end record;

   procedure Draw(Deck: in out Card_Deck; Card: out Natural) is
   -- removes a card from top of deck
   begin
      Card := Deck.Cards( Deck.Top );
      Deck.Top := @ + 1;
   end Draw;

   procedure Add(Deck: in out Card_Deck; Card: Natural) is
   -- adds card to bottom of deck
   begin
      Deck.Cards(Deck.Bottom) := Card;
      Deck.Bottom := @ + 1;
   end Add;

   -- two players, each has a deck
   Decks: array (1 .. 2) of Card_Deck;

   function Size(Deck: Card_Deck) return Natural is
   -- returns the number of cards in the deck, computed using top & bottom loc's
   (
         if Deck.Top <= Deck.Bottom then Natural( Deck.Bottom - Deck.Top )
         else Natural( Mod52'Last - Deck.Top + Deck.Bottom + 1 )
   );

   procedure Play_Round is
   -- plays a round of ordinary Combat
   -- this is pretty straightforward, even easy
      Player1_Card, Player2_Card: Natural;
   begin
      Draw(Decks(1), Player1_Card);
      Draw(Decks(2), Player2_Card);
      if Player1_Card > Player2_Card then
         Add(Decks(1), Player1_Card);
         Add(Decks(1), Player2_Card);
      else
         Add(Decks(2), Player2_Card);
         Add(Decks(2), Player1_Card);
      end if;
   end Play_Round;

   procedure Put(Deck: Card_Deck) is
   -- shows the cards in the deck
   begin
      Put("[");
      if Deck.Top < Deck.Bottom then
         for I in Deck.Top .. Deck.Bottom - 1 loop
            Put(Deck.Cards(I), 0); Put(", ");
         end loop;
      else
         for I in Deck.Top .. Mod52'Last loop
            Put(Deck.Cards(I), 0); Put(", ");
         end loop;
         for I in 0 .. Deck.Bottom - 1 loop
            Put(Deck.Cards(I), 0); Put(", ");
         end loop;
      end if;
      Put_Line("]");
   end Put;

   function Score(Deck: in out Card_Deck) return Natural is
   -- computes the Deck's score
   -- let n = Size(Deck)
   -- then score is n*(top card's value) + (n-1)*(next card's value) + ...
   --
   -- I was kind of disappointed here to learn that Ada's counters don't
   -- loop "correctly" for modular types, where "correctly" means cycliing
   -- around automatically if the first value is larger than the second

      Result: Natural := 0;
      I: Natural := Size(Deck);

   begin

      if Deck.Top < Deck.Bottom then

         for J in Deck.Top .. Deck.Bottom loop
            Result := Result + Deck.Cards(J) * I;
            if I > 0 then I := I - 1; end if;
         end loop;

      else

         for J in Deck.Top .. Mod52'Last loop
            Result := Result + Deck.Cards(J) * I;
            if I > 0 then I := I - 1; end if;
         end loop;
         for J in 0 .. Deck.Bottom loop
            Result := Result + Deck.Cards(J) * I;
            if I > 0 then I := I - 1; end if;
         end loop;

      end if;

      return Result;

   end Score;

   -- SECTION
   -- recursive combat types etc.

   -- copies of the original input
   Old_Deck1, Old_Deck2: Card_Deck;

   procedure Copy(
         Deck1: in Card_Deck; Deck2: out Card_Deck;
         Number: Natural := 0
   ) is
   -- we need to copy decks from time to time
      I: Mod52 := Deck1.Top;
      J: Natural := 0;
      Limit: Natural := ( if Number /= 0 then Number else Size(Deck1) );
   begin
      while J < Limit and then I /= Deck1.Bottom loop
         Add(Deck2, Deck1.Cards(I));
         I := I + 1;
         J := J + 1;
      end loop;
   end Copy;

   -- the status of a recursive game
   type Status is ( Player1_Wins , Player2_Wins, Unknown );

   type Recursive_Score is record
   -- used to report information on a recursive game to the caller
      Who_Won: Status := Unknown;
      Score: Natural := 0;
   end record;

   -- used to track how far we've recursed
   Level: Positive := 1;

   -- the next few functions & types prepare for the tracking
   -- of game configurations

   function Smaller_Config(First, Second: Card_Deck) return Boolean is
   -- used (indirectly) by Ordered_Maps of Card_Deck_Pair,
   -- to help order and identify equivalent elements
   begin
      if Size(First) < Size(Second) then return True;
      elsif Size(First) > Size(Second) then return False;
      else
         declare
            I: Mod52 := First.Top;
            J: Mod52 := Second.Top;
         begin
            while I /= First.Bottom and then J /= Second.Bottom and then First.Cards(I) = Second.Cards(J) loop
               I := I + 1; J := J + 1;
            end loop;
            if I = First.Bottom then return False; end if;
            return First.Cards(I) < Second.Cards(J);
         end;
      end if;
   end Smaller_Config;

   function Equal_Config(First, Second: Card_Deck) return Boolean is
   -- used (indirectly) by Ordered_Maps of Card_Deck_Pair,
   -- to help identify equivalent elements
   begin
      if Size(First) /= Size(Second) then
         return False;
      else
         declare
            I: Mod52 := First.Top;
            J: Mod52 := Second.Top;
         begin
            while I /= First.Bottom and then J /= Second.Bottom
                  and then First.Cards(I) = Second.Cards(J)
            loop
               I := I + 1; J := J + 1;
            end loop;
            return I = First.Bottom and J = Second.Bottom;
         end;
      end if;
   end Equal_Config;

   type Card_Deck_Pair is record
   -- we need to remember which decks the players had, with the same cards,
   -- in the same order
      Deck1, Deck2: Card_Deck;
      Level, Round: Natural;
   end record;

   function Smaller_Pair_Config(First, Second: Card_Deck_Pair) return Boolean is
   -- used for Ordered_Maps of Card_Deck_Pair,
   -- to help order and identify equivalent elements
   (
         Smaller_Config(First.Deck1, Second.Deck1) or else
         (
               Equal_Config(First.Deck1, Second.Deck2) and then
               Smaller_Config(First.Deck2, Second.Deck2)
         )
   );

   function Equal_Pair_Config(First, Second: Card_Deck_Pair) return Boolean is
   -- used by Ordered_Maps of Card_Deck_Pair,
   -- to help identify equivalent elements
   (
         Equal_Config(First.Deck1, Second.Deck1) and then
         Equal_Config(First.Deck2, Second.Deck2)
   );

   package Deck_Configs is new Ada.Containers.Ordered_Sets
   (
    Element_Type => Card_Deck_Pair,
    "<" => Smaller_Pair_Config,
    "=" => Equal_Pair_Config
   );

   function Play_Recursive_Combat(
         Old_Deck1, Old_Deck2: Card_Deck;
         Number1, Number2: Natural
   ) return Recursive_Score
   -- implements recursive combat

   is

      Deck1: Card_Deck;
      Deck2: Card_Deck;

      Old_Configs: Deck_Configs.Set;

      Result: Recursive_Score;
      Round: Natural := 0; -- used purely for debugging

   begin

      Level := Level + 1;
      Copy(Old_Deck1, Deck1, Number1);
      Copy(Old_Deck2, Deck2, Number2);

      -- play game according to recursive rules
      while Size(Deck1) /= 0 and Size(Deck2) /= 0 loop

         Round := Round + 1;

         -- uncomment for details or debugging
         -- Put("game "); Put(Level, 0); Put(" round "); Put(Round, 0); New_Line;
         -- Put(Deck1); New_Line; Put(Deck2); New_Line;

         if Old_Configs.Contains( ( Deck1, Deck2, Level, Round ) ) then

            -- player 1 wins game when we repeat a score in THIS game

            -- Put_Line("found recursion");
            Result.Who_Won := Player1_Wins;
            exit;

         else

            Old_Configs.Insert( ( Deck1, Deck2, Level, Round ) );

            -- draw a card as normal, then evaluate according to new rules
            -- essentially, if you can recurse, then do so

            declare Player1_Card, Player2_Card: Natural;
            begin

               Draw(Deck1, Player1_Card);
               Draw(Deck2, Player2_Card);

               if Size(Deck1) < Player1_Card or Size(Deck2) < Player2_Card then

                  -- Put_Line("ordinary play");
                  -- player with higher-value card wins round
                  if Player1_Card > Player2_Card then
                     Add(Deck1, Player1_Card);
                     Add(Deck1, Player2_Card);
                  else
                     Add(Deck2, Player2_Card);
                     Add(Deck2, Player1_Card);
                  end if;

               else

                  -- Put_Line("recursive play");
                  -- player who wins recursive combat wins round
                  declare
                     Subresult: Recursive_Score
                           := Play_Recursive_Combat(
                                                    Deck1, Deck2,
                                                    Player1_Card, Player2_Card
                                                   );
                  begin
                     if Subresult.Who_Won = Player1_Wins then
                        Add(Deck1, Player1_Card);
                        Add(Deck1, Player2_Card);
                     else
                        Add(Deck2, Player2_Card);
                        Add(Deck2, Player1_Card);
                     end if;
                  end;

               end if;

            end;

         end if;

      end loop;

      -- if we haven't recorded the winner, it's because we never replayed
      -- a previous hand, so we have to look at the decks

      if Result.Who_Won = Unknown then
         Result.Who_Won
               := ( if Size(Deck1) = 0 then Player2_Wins else Player1_Wins );
      end if;
      Result.Score
            := (
                if Result.Who_Won = Player1_Wins then Score(Deck1)
                else Score(Deck2)
               );

      Level := Level - 1;
      -- Put_Line("finished with"); Put(Deck1); Put(Deck2);

      return Result;

   end Play_Recursive_Combat;

begin

   -- SECTION
   -- get the players' hands

   Open(F, In_File, Filename);

   -- read first player's cards

   -- first line
   declare S: String := Get_Line(F); begin null; end;

   loop
      declare
         S: String := Get_Line(F);
         V: Natural;
         Last: Positive;
      begin
         exit when S = "";
         Get(S, V, Last);
         Add(Decks(1), V);
      end;
   end loop;

   Put(Size(Decks(1)), 0); Put_Line(" cards in deck 1");

   -- read second player's cards

   -- first line
   declare S: String := Get_Line(F); begin null; end;

   while not End_Of_File(F) loop
      declare
         S: String := Get_Line(F);
         V: Natural;
         Last: Positive;
      begin
         Get(S, V, Last);
         Add(Decks(2), V);
      end;
   end loop;

   Close(F);

   Put(Size(Decks(2)), 0); Put_Line(" cards in deck 2");

   -- need these for part 2

   Copy( Decks(1), Old_Deck1 );
   Copy( Decks(2), Old_Deck2 );

   -- SECTION
   -- part 1: play a complete game and report points

   while Size(Decks(1)) /= 0 and Size(Decks(2)) /= 0 loop
      Play_Round;
   end loop;

   Put("score after combat: ");
   Put( (
        if Size(Decks(1)) /= 0 then Score(Decks(1))
        else Score(Decks(2))
       ), 0 );
   New_Line;

   -- SECTION
   -- part 2: play a game of recursive combat and report points

   Put("score after recursive combat: ");
   declare
      Subresult: Recursive_Score
            := Play_Recursive_Combat(
                                     Old_Deck1, Old_Deck2,
                                     Size(Old_Deck1), Size(Old_Deck2)
                                    );
   begin
      Put( Subresult.Score, 0 ); New_Line;
   end;

end Main;
