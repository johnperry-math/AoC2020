-- Advent of Code 2020, Day 21
--
-- John Perry
--
-- apologies for any bad Ada style
--
-- Allergen Assessment
--
-- given a list of foods' ingredients, associated with warnings:
--
-- part 1: identify which foods do not contain allergens
--
-- part 2: identify which ingredient corresponds to which allergen; sort by
-- allergen, then report
--

pragma Ada_2020;

with Ada.Text_IO;
use Ada.Text_IO;

with Ada.Containers.Ordered_Maps;

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
             if Testing then "/Users/user/common/Ada/AoC2020/Day21/example.txt"
             else "/Users/user/common/Ada/AoC2020/Day21/input.txt"
            );

   -- SECTION
   -- types for ingredients and allergens

   subtype Name_String is String( 1 .. 10 );

   type Possibilities is ( Suspicious, Cleared );
   -- Possibilities will apply to both Ingredients and Allergens (see below)
   -- and will say whether an ingredient is suspected of being an allergen
   -- or vice-versa.

   package Correlation_Maps is new Ada.Containers.Ordered_Maps
   -- Correlation_Maps is used in each Allergen or Ingredient.
   -- The map will list the Ingredients or Allergens, respectively, for which
   -- an association is known.
   -- For example, for the Ingredient is "secret sauce", we may list
   --    dairy -> suspicious, strawberry -> cleared,
   -- and ingredients for which a relationship isn't known aren't listed.
   (
    Key_Type => Name_String,
    Element_Type => Possibilities
   );

   -- Ingredients and Allergens have a name and a Correlation_Map

   type Ingredient is record
      Name: Name_String := ( others => ' ' );
      Allergen_Knowledge: Correlation_Maps.Map;
   end record;

   type Allergen is record
      Name: Name_String := ( others => ' ' );
      Ingredient_Knowledge: Correlation_Maps.Map;
   end record;

   package Name_Sets is new Ada.Containers.Ordered_Sets
   -- needed for part 2
   (
    Element_Type => Name_String
   );

   type Food is record
   -- needed for part two, to determine which ingredient is which allergen
      Ingredients, Allergens: Name_Sets.Set;
   end record;

   package Food_Vectors is new Ada.Containers.Vectors
   -- needed for part 2
   (
    Element_Type => Food,
    Index_Type => Positive
   );

   All_Foods: Food_Vectors.Vector;

   -- Name_Maps helps find where a given name lies in its vector
   -- I figured this would be smarter than constantly searching the vector

   package Name_Maps is new Ada.Containers.Ordered_Maps
   (
    Key_Type => Name_String,
    Element_Type => Positive
   );

   Find_Ingredient: Name_Maps.Map;
   Find_Allergen: Name_Maps.Map;

   -- types for the vectors that actually store information

   package Ingredient_Vectors is new Ada.Containers.Vectors
   (
    Element_Type => Ingredient,
    Index_Type => Positive
   );

   package Allergen_Vectors is new Ada.Containers.Vectors
   (
    Element_Type => Allergen,
    Index_Type => Positive
   );

   All_Ingredients: Ingredient_Vectors.Vector;
   All_Allergens: Allergen_Vectors.Vector;

   -- SECTION
   -- functions necessary to solve the puzzles

   procedure Analyze_Food(S: String) is
   -- read a line, determine the food's ingredients and allergens, if any,
   -- and determine the correspondence between each

      New_Food: Food;
      Pos: Natural := S'First; -- for moving through the string

      -- a couple of conveniences
      Line_Ingredients renames New_Food.Ingredients;
      Line_Allergens renames New_Food.Allergens;

   begin

      -- read ingredients

      while S(Pos) /= '(' loop

         declare

            Name: Name_String := ( others => ' ' );
            Name_Pos: Positive := Name'First;

         begin

            while S(Pos) /= ' ' loop
               Name(Name_Pos) := S(Pos);
               Pos := Pos + 1;
               Name_Pos := Name_Pos + 1;
            end loop;

            if not Line_Ingredients.Contains(Name) then
               Line_Ingredients.Insert(Name);
            end if;

         end;

         while S(Pos) = ' ' loop Pos := Pos + 1; end loop;

      end loop;

      Pos := Pos + 10; -- skip " contains "

      -- read allergens

      while Pos < S'Last loop

         declare

            Name: Name_String := ( others => ' ' );
            Name_Pos: Positive := Name'First;

         begin

            while S(Pos) /= ',' and S(Pos) /= ')' loop
               Name(Name_Pos) := S(Pos);
               Pos := Pos + 1;
               Name_Pos := Name_Pos + 1;
            end loop;

            if not Line_Allergens.Contains(Name) then
               Line_Allergens.Insert(Name);
            end if;

            Pos := Pos + 2; -- skip ", "

         end;

      end loop;

      -- now we analyze the ingredients and determine which allergens are
      -- suspicious, but also we clear new ingredients of suspicion
      -- in particular, if an allergen appears on two lines, but any ingredient
      -- does not appear on both, then that ingredient is not that allergen

      Analyze_Line_Ingredients:
      for Name of Line_Ingredients loop

         if not Find_Ingredient.Contains(Name) then

            declare

               New_Ingredient: Ingredient := ( Name => Name, others => <> );

            begin

               for LA of Line_Allergens loop

                  if not ( Find_Allergen.Contains(LA) ) then

                     -- new allergen; suspicious!
                     New_Ingredient.Allergen_Knowledge.Insert(LA, Suspicious);

                  else

                     -- known allergen; clear this ingredient of suspicion!
                     declare A: Allergen_Vectors.Reference_Type
                        := All_Allergens.Reference( Find_Allergen(LA) );
                     begin
                        A.Ingredient_Knowledge.Insert(Name, Cleared);
                     end;

                  end if;

               end loop;

               -- update records
               All_Ingredients.Append(New_Ingredient);
               Find_Ingredient.Insert(Name, Positive(All_Ingredients.Length));

            end;

         else

            -- known ingredient! we can also clear this one of some allergens

            declare I: Ingredient_Vectors.Reference_Type
                     := All_Ingredients.Reference( Find_Ingredient(Name) );

            begin

               for LA of Line_Allergens loop

                  if Find_Allergen.Contains(LA) then

                     declare

                        A: Allergen_Vectors.Reference_Type
                              := All_Allergens.Reference( Find_Allergen(LA) );

                     begin

                        if not A.Ingredient_Knowledge.Contains(Name) then
                           A.Ingredient_Knowledge.Insert(Name, Cleared);
                        end if;
                        if not I.Allergen_Knowledge.Contains(LA) then
                           I.Allergen_Knowledge.Insert(LA, Cleared);
                        end if;

                     end;

                  end if;

               end loop;

            end;

         end if;

      end loop Analyze_Line_Ingredients;

      -- now analyze the allergens
      -- for new allergens, new ingredients on this line are always suspicious
      -- and old ingredients may also be suspicious because the allergen may not
      -- have been listed previously

      Analyze_Line_Allergens:
      for Name of Line_Allergens loop

         if not Find_Allergen.Contains(Name) then

            -- new allergen

            declare New_Allergen: Allergen := ( Name => Name, others => <> );

            begin

               for LI of Line_Ingredients loop

                  New_Allergen.Ingredient_Knowledge.Insert(LI, Suspicious);

                  declare I: Ingredient_Vectors.Reference_Type
                     renames All_Ingredients(Find_Ingredient(LI));
                  begin
                     if not I.Allergen_Knowledge.Contains(Name) then
                        I.Allergen_Knowledge.Insert(Name, Suspicious);
                     end if;
                  end;

               end loop;

               -- update records
               All_Allergens.Append(New_Allergen);
               Find_Allergen.Insert(Name, Positive(All_Allergens.Length));

            end;

         else

            -- old allergen: we can clear suspected ingredients that don't
            -- appear on this line, and clear all new ingredients on this line

            declare A: Allergen_Vectors.Reference_Type
                     := All_Allergens.Reference( Find_Allergen(Name) );
            begin

               -- if known allergen's suspected ingredient does not appear
               -- on this line, clear it of suspicion

               for I of All_Ingredients loop

                  if A.Ingredient_Knowledge.Contains(I.Name)
                        and then A.Ingredient_Knowledge(I.Name) = Suspicious
                  then

                     if not Line_Ingredients.Contains(I.Name) then
                        A.Ingredient_Knowledge(I.Name) := Cleared;
                        I.Allergen_Knowledge( Name ) := Cleared;
                     end if;

                  end if;

               end loop;

            end;

         end if;

      end loop Analyze_Line_Allergens;

      All_Foods.Append(New_Food);

   end Analyze_Food;

   function Num_Suspicious(I: Ingredient) return Natural is
   -- return number of suspicious allergens in I
   -- needed for part 2
      Result: Natural := 0;
   begin
      for A of I.Allergen_Knowledge loop
         if A = Suspicious then Result := Result + 1; end if;
      end loop;
      return Result;
   end Num_Suspicious;

begin

   -- SECTION
   -- get the food data

   Open(F, In_File, Filename);

   while not End_Of_File(F) loop
      declare S: String := Get_Line(F);
      begin
         Analyze_Food(S);
      end;
   end loop;

   -- SECTION
   -- part 1:
   -- report which ingredients do not correspond to an allergen ("cleared")
   -- and how many times they appear in the all the foods' ingredients

   declare Appearances: Natural := 0;
   begin

      Put("cleared ingredients: ");

      for I of All_Ingredients loop

         -- if none of the associations with I are suspicious, then I is clear

         if (
               for all A of All_Allergens =>
                     not ( I.Allergen_Knowledge.Contains(A.Name)
                           and then I.Allergen_Knowledge(A.Name) = Suspicious )
         ) then
            Put(I.Name); Put(", ");
            -- an unpleasant inner loop but that's life
            for F of All_Foods loop
               if F.Ingredients.Contains(I.Name) then
                  Appearances := Appearances + 1;
               end if;
            end loop;
         end if;

      end loop;

      New_Line;
      Put("appearances they make in foods: "); Put(Appearances'Image); New_Line;

   end;

   -- SECTION
   -- part 2:
   -- analyze the list of ingredients that correspond to allergens
   -- to determine precisely which ingredient corresponds to precisely which
   -- allerge
   --
   -- the strategy is this: hopefully, one ingredient will already correspond
   -- to only one allergen (in fact, several did!) and use that to eliminate
   -- that allergen from other ingredients' possibilities

   declare Bad_Ingredients: Ingredient_Vectors.Vector;
   begin

      -- first I just reported on which ingredients were not clear
      -- and how many allergens they corresponded to;
      -- this can be removed if desired

      Put("non-cleared ingredients: ");
      for I of All_Ingredients loop
         if (
                  for some A of All_Allergens =>
                        ( I.Allergen_Knowledge.Contains(A.Name)
                              and then I.Allergen_Knowledge(A.Name) = Suspicious )
                 )
         then
            Bad_Ingredients.Append(I);
            Put(I.Name); Put(", ");
            declare Num_Allergens: Natural := 0;
            begin
               for A of All_Allergens loop
                  if I.Allergen_Knowledge(A.Name) = Suspicious then
                     Num_Allergens := Num_Allergens + 1;
                  end if;
               end loop;
               Put(Num_Allergens'Image); Put("; ");
            end;
         end if;
      end loop;
      New_Line;

      -- the actual work of part 2:
      -- as long as some ingredient has more than one suspicious allergen,
      -- use ingredients with just one allergen to eliminate other ingredients'
      -- allergens

      while ( for some I of Bad_Ingredients => ( Num_Suspicious(I) > 1 ) ) loop

         for I in Bad_Ingredients.First_Index .. Bad_Ingredients.Last_Index loop

            declare

               BI: Ingredient_Vectors.Reference_Type renames Bad_Ingredients(I);
               Number_Suspicious: Natural := Num_Suspicious(BI);

            begin

               if Number_Suspicious = 1 then

                  -- eliminate the sole allergen from other ingredients

                  declare
                     Bad_Allergen: Correlation_Maps.Cursor
                           := BI.Allergen_Knowledge.First;
                  begin

                     -- first identify which one is sole allergen!

                     while Correlation_Maps.Element(Bad_Allergen) /= Suspicious loop
                        Correlation_Maps.Next(Bad_Allergen);
                     end loop;

                     -- now eliminate from others

                     for J in Bad_Ingredients.First_Index
                           .. Bad_Ingredients.Last_Index
                     loop

                        if J /= I then -- don't check against same one!

                           declare
                              JA: Correlation_Maps.Cursor
                                 := Bad_Ingredients(J).Allergen_Knowledge.First;
                              use Correlation_Maps;
                           begin

                              while JA /= Correlation_Maps.No_Element
                                    and then Key(JA) /= Key(Bad_Allergen)
                              loop
                                 Next(JA);
                              end loop;

                              if JA /= No_Element then
                                 Bad_Ingredients(J).Allergen_Knowledge(JA)
                                       := Cleared;
                              end if;

                           end;

                        end if;

                     end loop;

                  end;

               end if;

            end;

         end loop;

         for I of Bad_Ingredients loop
            Put(I.Name); Put(": "); Put(Num_Suspicious(I)'Image); New_Line;
         end loop;
         New_Line;

      end loop;

      -- now sort them by name; I solved this using a new array of paired names
      -- and a generic function on that typ eto sort by allergen

      declare

         -- the pair
         type Name_Pair is record
            Ingredient, Allergen: Name_String;
         end record;
         -- the array's type
         type Pairings_Array
               is array ( Positive range <> ) of Name_Pair;
         -- the actual array
         Pairings: Pairings_Array( 1 .. Positive( Bad_Ingredients.Length ) );

         -- the sorter and its ordering function

         function Order_Pair(First, Second: Name_Pair) return Boolean is
         (
            First.Allergen < Second.Allergen
         );
         procedure Sort_Pairings is new Ada.Containers.Generic_Array_Sort
         (
          Index_Type => Positive,
          Element_Type => Name_Pair,
          Array_Type => Pairings_Array,
          "<" => Order_Pair
         );

      begin

         -- first pair them up

         for I in 1 .. Positive( Bad_Ingredients.Length ) loop

            Pairings(I).Ingredient := Bad_Ingredients(I).Name;

            declare
               A: Correlation_Maps.Cursor
                     := Bad_Ingredients(I).Allergen_Knowledge.First;
            begin

               while Correlation_Maps.Element(A) /= Suspicious loop
                  Correlation_Maps.Next(A);
               end loop;
               Pairings(I).Allergen := Correlation_Maps.Key(A);

            end;

         end loop;

         -- sort by allergen
         Sort_Pairings(Pairings);

         -- report
         for P of Pairings loop
            Put(P.Allergen); Put(", "); Put_Line(P.Ingredient);
         end loop;

      end;

   end;

end Main;
