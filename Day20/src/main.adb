-- Advent of Code 2020, Day 20
--
-- John Perry
--
-- apologies for any bad Ada style
--
-- Jurassic Jigsaw
--
-- part 1: identify corner tiles of an image divided into many 10x10 times
--
-- part 2: find monsters in the image, then determine "water roughness"; i.e.,
-- the number of rough water squares in the image when you remove the monsters
--

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

   -- SECTION
   -- input-related

   F: File_Type; -- input file

   Testing: constant Boolean := False;

   Filename: constant String
         := (
             if Testing then "/Users/user/common/Ada/AoC2020/Day20/example.txt"
             else "/Users/user/common/Ada/AoC2020/Day20/input.txt"
            );

   -- SECTION

   -- images consist of tiles (the number depends on the input);
   -- each tile is a 10x10 image

   Tiles_Per_Image_Side: constant := ( if Testing then 3 else 12 );

   Max_Tile_Pixel: constant := 10;
   MTP: constant := Max_Tile_Pixel;

   subtype Tile_Dimension is Positive range 1 .. MTP;

   -- we use several types to track which sides meet among the tiles
   -- Side_Data and Edge_Data store each side in a 1-dimensional array
   -- Matched_Sides tracks which tile corresponds to the indexed side

   type Tile_Sides is ( North, South, East, West );

   type Side_Data is array ( Tile_Dimension'Range ) of Boolean;

   type Edge_Data is array ( Tile_Sides ) of Side_Data;

   type Matched_Sides is array ( Tile_Sides ) of Natural;

   -- image data

   type Raw_Tile_Data
         is array ( Tile_Dimension'Range , Tile_Dimension'Range ) of Boolean;

   -- all the data we have or need for a tile is stored in in the Tile record
   --
   -- Raw_Data is the image data as read from the input
   --
   -- Sides and Sides_Reversed store a convenient 1-dimensional representation
   -- of each side for easy testing against other sides
   --
   -- Matches records which other tile has a side that matches the indexed one;
   -- a value of 0 means that no other side matches that one

   type Tile is record
      Raw_Data: Raw_Tile_Data;
      Sides: Edge_Data;
      Sides_Reversed: Edge_Data;
      Matches: Matched_Sides := ( others => 0 );
   end record;

   -- SECTION
   -- storing Tiles in a map

   function Natural_Hash(N: Natural) return Ada.Containers.Hash_Type is
   ( Ada.Containers.Hash_Type( N ) );

   package Tile_Maps is new Ada.Containers.Hashed_Maps
   (
    Key_Type => Natural,
    Element_Type => Tile,
    Hash => Natural_Hash,
    Equivalent_Keys => "="
   );

   All_Tiles: Tile_Maps.Map;

   -- I didn't check if the tiles actually do occur in a sequence;
   -- it was easier simply to record the indices in a set

   package Tile_Index_Sets is new Ada.Containers.Hashed_Sets
   (
    Element_Type => Natural,
    Hash => Natural_Hash,
    Equivalent_Elements => "="
   );

   Tile_Indices: Tile_Index_Sets.Set;

   -- SECTION
   -- utility functions for reading and recording Tiles

   function Get_Side(T: Tile; Which: Tile_Sides) return Side_Data is
   -- transform the indicated side of T into a 1-dimensional array;
   -- this requires rotating the east and west sides

-- again, the current GNAT bug; hope we won't have to wait too long for the fix
--     (
--        for I in 1 .. 10 => (
--           case Which is
--           when North => T.Raw_Data(  1,  I ),
--           when South => T.Raw_Data( 10,  I ),
--           when East  => T.Raw_Data(  I, 10 ),
--           when West  => T.Raw_Data(  I,  1 )
--        )
--     );

      Result: Side_Data;

   begin

      for I in 1 .. MTP loop

         Result(I) := (
            case Which is
               when North => T.Raw_Data(   1,   I ),
               when South => T.Raw_Data( MTP,   I ),
               when East  => T.Raw_Data(   I, MTP ),
               when West  => T.Raw_Data(   I,   1 )
         );

      end loop;

      return Result;

   end Get_Side;

   procedure Get(F: File_Type; T: out Tile) is
   -- read a file from a tile and set up all fields that we know
   -- (this includes Raw_Data, Sides, and Sides_Reversed)

   begin

      for I in Tile_Dimension'Range loop

         declare S: String := Get_Line(F);
         begin

            for J in Tile_Dimension'Range loop
               T.Raw_Data(I,J) := S(J) = '#';
            end loop;

         end;

      end loop;

      for Side in Tile_Sides loop

         T.Sides(Side) := Get_Side(T, Side);
         T.Sides_Reversed(Side)
            := ( for I in Tile_Dimension'Range
                 => T.Sides(Side)(Tile_Dimension'Last - I + 1 )
               );

      end loop;
   end Get;

   procedure Put(S: Side_Data) is
   -- useful for debugging
   begin
      for I in Tile_Dimension'Range loop
         Put( ( if S(I) then '#' else '.' ) );
      end loop;
   end Put;

   procedure Put(T: Tile; Edges_Too: Boolean := False) is
   -- useful for debugging, such as making sure I've read the tile correctly
   begin
      for I in Tile_Dimension'Range loop
         for J in Tile_Dimension'Range loop
            Put( ( if T.Raw_Data(I,J) then '#' else '.' ) );
         end loop;
         New_Line;
      end loop;
      if Edges_Too then
         Put("north: "); Put(T.Sides(North)); New_Line;
         Put("reversed: "); Put(T.Sides_Reversed(North)); New_Line;
         Put("south: "); Put(T.Sides(South)); New_Line;
         Put("reversed: "); Put(T.Sides_Reversed(South)); New_Line;
         Put("east:  "); Put(T.Sides(East )); New_Line;
         Put("reversed: "); Put(T.Sides_Reversed(East)); New_Line;
         Put("west:  "); Put(T.Sides(West )); New_Line;
         Put("reversed: "); Put(T.Sides_Reversed(West)); New_Line;
      end if;
      Put("north matches "); Put(T.Matches(North)); New_Line;
      Put("south matches "); Put(T.Matches(South)); New_Line;
      Put("east matches "); Put(T.Matches(East)); New_Line;
      Put("west matches "); Put(T.Matches(West)); New_Line;
   end Put;

   -- SECTION
   -- types, objects, and subprograms related to image processing

   -- after we identify which tiles match, we remove the borders,
   -- so a processed image tile is smaller than the raw image tile

   type Processed_Image_Tile is array ( 1 .. MTP - 2, 1 .. MTP - 2 ) of Boolean;

   -- although this is called "Image", it's really an intermediate step
   -- in moving from the tile representation convenient for part 1
   -- to a flattened representation convenient for part 2

   Image: array( 1 .. Tiles_Per_Image_Side , 1 .. Tiles_Per_Image_Side )
         of Processed_Image_Tile;

   procedure Rotate_Data(T: in out Tile) is
   -- rotates the data in T 90 degrees clockwise and updates T.Matches to the
   -- correct sides
      Tmp_Data: Raw_Tile_Data;
      Tmp_Match: Natural;
   begin
      for I in Tile_Dimension'Range loop
         for J in Tile_Dimension'Range loop
            Tmp_Data(I, J) := T.Raw_Data(Tile_Dimension'Last - J + 1, I);
         end loop;
      end loop;
      T.Raw_Data := Tmp_Data;
      Tmp_Match := T.Matches(North);
      T.Matches(North) := T.Matches(West);
      T.Matches(West) := T.Matches(South);
      T.Matches(South) := T.Matches(East);
      T.Matches(East) := Tmp_Match;
   end Rotate_Data;

   procedure Flip_Data(T: in out Tile) is
   -- flips the data in T across the horizontal access and updates T.Matches
   -- to the correct sides
      Tmp: Raw_Tile_Data;
      Tmp_Match: Natural;
   begin
      for I in Tile_Dimension'Range loop
         for J in Tile_Dimension'Range loop
            Tmp(I, J) := T.Raw_Data(Tile_Dimension'Last - I + 1, J);
         end loop;
      end loop;
      T.Raw_Data := Tmp;
      Tmp_Match := T.Matches(North);
      T.Matches(North) := T.Matches(South);
      T.Matches(South) := Tmp_Match;
   end Flip_Data;

   -- we use this to record which tile fits in which spot on the grid

   type Tile_Correspondence
         is array(
                  1 .. Tiles_Per_Image_Side,
                  1 .. Tiles_Per_Image_Side
                 ) of Natural;

   -- the Rotation type will indicate how many rotations are needed

   type Rotation is (None, Rot_One, Rot_Two, Rot_Three);

   No_Tile: constant := 0;

   function Determine_Rotation_For_West_Tile(T: Tile; West_Index: Natural)
   -- determines the number of rotations necessary
   -- to correctly place T east of the tile indexed at West_Index
   -- the idea is that you want T rotated to meet the tile west of it,
   -- so you are rotating "for the tile to the west"; that is, the "west tile"
   --
   -- this also works for a tile on the edge; just use No_Tile for West_Index
   return Rotation
   is
   (
         if T.Matches(West) = West_Index then None
         elsif T.Matches(North) = West_Index then Rot_Three
         elsif T.Matches(East) = West_Index then Rot_Two
         else Rot_One
   );

   procedure Place_Tile(
         Which, I, J: Natural;
         Assignments: in out Tile_Correspondence
   ) is
   -- places the tile indexed by Which at position I, J of Assignments,
   -- after rotating by a multiple of 90 degrees clockwise, and
   -- if necessary flipping horizontally to match other tiles
   --
   -- the correct values to flip and rotate are determined automatically

      -- the tile we're interested in
      T: Tile_Maps.Reference_Type renames All_Tiles(Which);

      -- the number of rotations this tile will need
      Rot: Rotation := (
              if I = 1 and J = 1 then
                 Determine_Rotation_For_West_Tile(T, No_Tile)
              elsif I = 1 then
                 Determine_Rotation_For_West_Tile(T, Assignments(1, J - 1) )
              elsif J = 1 then
                 Determine_Rotation_For_West_Tile(T, No_Tile)
              else
                 Determine_Rotation_For_West_Tile(T, Assignments(I, J-1) )
             );

   begin

      -- update the record of assignments
      Assignments(I, J) := Which;

      -- rotate
      for R in Rotation'Pos(None) .. Rotation'Pos(Rot) - 1 loop
         Rotate_Data(All_Tiles(Which));
      end loop;

      -- flip if need be
      if I = 1 and then T.Matches(North) /= 0 then
         Flip_Data(T);
      elsif I > 1 and then T.Matches(North) /= Assignments(I - 1, J) then
         Flip_Data(T);
      end if;

      -- copy the tile data to Image
      for K in 1 .. 8 loop
         for L in 1 .. 8 loop
            Image(I, J)(K, L) := T.Raw_Data(K+1,L+1);
         end loop;
      end loop;

   end Place_Tile;

   procedure Build_Image( First_Corner: Natural ) is
   -- builds an image by organizing the placing of tiles,
   -- which for its part is handled, naturally enough, by Place_Tile

      Assignments: Tile_Correspondence;
      This_Index: Natural := First_Corner;
      Prev_Index: Natural := First_Corner;

   begin

      -- northwest corner first

      Place_Tile( First_Corner, 1, 1, Assignments );

      -- remainder of first row

      for J in 2 .. Tiles_Per_Image_Side loop
         declare Prev_Tile: Tile_Maps.Reference_Type
               renames All_Tiles(This_Index);
         begin
            This_Index := Prev_Tile.Matches(East);
            Place_Tile(This_Index, 1, J, Assignments);
         end;
      end loop;

      -- remaining rows

      for I in 2 .. Tiles_Per_Image_Side loop

         This_Index := All_Tiles(Assignments(I - 1, 1)).Matches(South);
         Place_Tile(This_Index, I, 1, Assignments);

         for J in 2 .. Tiles_Per_Image_Side loop

            This_Index := All_Tiles(Assignments(I - 1, J)).Matches(South);
            Place_Tile(This_Index, I, J, Assignments);

         end loop;

      end loop;

   end Build_Image;

   -- SECTION
   --
   -- in part 2 we need to "flatten" the image to a 2-dimension array of
   -- "pixels", rather then the two-dimensional array of tiles that we used
   -- in part 1
   --
   -- the Flattened_Image_Type takes care of that for us

   type Flattened_Image_Type is array(
                       1 .. Tiles_Per_Image_Side * ( Tile_Dimension'Last - 2 ),
                       1 .. Tiles_Per_Image_Side * ( Tile_Dimension'Last - 2 )
                         ) of Boolean;

   Flattened_Image: Flattened_Image_Type;

   procedure Flatten_Image is
   -- does what it says: converts Image to Flattened_Image

   begin

      for I in 1 .. Tiles_Per_Image_Side loop
         for K in 1 .. Tile_Dimension'Last - 2 loop
            for J in 1 .. Tiles_Per_Image_Side loop
               for L in 1 .. Tile_Dimension'Last - 2 loop
                  Flattened_Image(
                                  (I - 1) * (Tile_Dimension'Last - 2) + K,
                                  (J - 1) * (Tile_Dimension'Last - 2) + L
                                 )
                        := Image(I,J)(K,L);
               end loop;
            end loop;
         end loop;
      end loop;

      -- debugging
--        for I in Flattened_Image'Range(1) loop
--           for J in Flattened_Image'Range(2) loop
--              Put( ( if Flattened_Image(I, J) then '#' else '.' ) );
--           end loop;
--           New_Line;
--        end loop;

   end Flatten_Image;

   -- once we flatten the data, we need both to locate the monsters
   -- and to count the number of rough sea "pixels" that are not monster pixels
   --
   -- this section is a bit bloated; I could probably have handled
   -- Flattened_Image and Monsters_Exposed as one data structure, but,
   -- in for a penny, in for a pound... one day I may revisit this and simplify
   -- it

   -- Detected_Data provides storage for image data,

   type Detected_Data is ( Rough, Calm, Monster );

   Monsters_Exposed: array(
         Flattened_Image_Type'Range(1),
         Flattened_Image_Type'Range(2)
   ) of Detected_Data;

   procedure Prepare_Exposure is
   -- copies Flattened_Image to Monsters_Exposed
   begin
      for I in Flattened_Image_Type'Range(1) loop
         for J in Flattened_Image_Type'Range(2) loop
            Monsters_Exposed(I,J)
                  := ( if Flattened_Image(I, J) then Rough else Calm );
         end loop;
      end loop;
   end Prepare_Exposure;

   Monster_Picture: constant array( 1 .. 3 , 1 .. 20 ) of Boolean
   -- hey, it's Nessie
         := (
             1 => ( 19 => True, others => False ),
             2 => (
                   1 | 6 | 7 | 12 | 13 | 18 | 19 | 20 => True,
                   others => False
                  ),
             3 => ( 2 | 5 | 8 | 11 | 14 | 17 => True, others => False )
            );

   procedure Copy_Monster(I, J: Positive) is
   -- copies the monster onto Monsters_Exposed at the indicated location
   begin
      for K in 1 .. 3 loop
         for L in 1 .. 20 loop
            Monsters_Exposed(I + K - 1, J + L - 1) :=
                  ( if Monster_Picture(K,L) then Monster else @ );
         end loop;
      end loop;
   end Copy_Monster;

   function Count_of_Roughness return Natural is
   -- counts the number of rough squares in Monsters_Exposed
      Result: Natural := 0;
   begin
      for I in Monsters_Exposed'Range(1) loop
         for J in Monsters_Exposed'Range(2) loop
            if Monsters_Exposed(I,J) = Rough then
               Result := Result + 1;
            end if;
         end loop;
      end loop;
      return Result;
   end Count_of_Roughness;

   function Number_Of_Monsters return Natural is
   -- counts the number of monsters and records them into Monsters_Exposed
   -- for good measure

      Result: Natural := 0;

   begin

      Prepare_Exposure;

      for I in 1 .. Flattened_Image'Length(1) - 3 loop
         for J in 1 .. Flattened_Image'Length(2) - 20 loop

            if (
                for all K in 1 .. 3
                => (
                    for all L in 1 .. 20
                    => (
                        if Monster_Picture(K,L)
                        then Flattened_Image(I + K - 1, J + L - 1)
                        else True
                       )
                   )
               )
            then
               Put("found monster at "); Put(I, 0); Put(", "); Put(J, 0);
               New_Line;
               Result := Result + 1;
               Copy_Monster(I, J);
            end if;

         end loop;
      end loop;

      return Result;

   end Number_Of_Monsters;

   procedure Rotate_Flattened_Image is
   -- sometimes we have to rotate Monsters_Exposed in order to detect a monster

      Tmp: Flattened_Image_Type;
      M: Natural := Flattened_Image_Type'Last(1);
      N: Natural := Flattened_Image_Type'Last(2);

   begin

      for I in Flattened_Image_Type'Range(1) loop
         for J in Flattened_Image_Type'Range(2) loop
            Tmp(I,J) := Flattened_Image(N - J + 1, I);
         end loop;
      end loop;

      Flattened_Image := Tmp;

   end Rotate_Flattened_Image;

   procedure Flip_Flattened_Image is
   -- sometimes we have to flip the flattened image

      Tmp: Boolean;

   begin

      for I in 1 .. Flattened_Image_Type'Last(1) / 2 loop
         for J in Flattened_Image_Type'Range(2) loop
            Tmp := Flattened_Image(Flattened_Image'Last(1) - I + 1, J);
            Flattened_Image(Flattened_Image'Last(1) - I + 1, J)
                  := Flattened_Image(I, J);
            Flattened_Image(I, J) := Tmp;
         end loop;
      end loop;

   end Flip_Flattened_Image;

   procedure Put_Flattened_Image is
   -- prints Flattened_Image to the standard output

   begin

      for I in Flattened_Image_Type'Range(1) loop
         for J in Flattened_Image_Type'Range(2) loop
            Put( ( if Flattened_Image(I,J) then '#' else '.' ) );
         end loop;
         New_Line;
      end loop;

      New_Line(2);

   end Put_Flattened_Image;

begin

   -- SECTION
   -- get the image data

   Open(F, In_File, Filename);

   while not End_Of_File(F) loop

      -- format: repetitions of
      --    Tile (Natural):
      --    (10x10 array of #, .)

      declare

         S: String := Get_Line(F);

         Which: Natural := 0; -- the tile number we're reading
         T: Tile; -- where we'll set it up
         Pos: Natural := S'First; -- location in S for reading

      begin

         -- skip "Tile ", read the number
         Pos := Pos + 5;
         while S(Pos) in '0'..'9' loop
            Which := Which * 10 + Character'Pos(S(Pos)) - Character'Pos('0');
            Pos := Pos + 1;
         end loop;

         Get(F, T);
         All_Tiles.Insert( Which, T );
         Tile_Indices.Insert( Which );

         -- read the blank line after each tile
         declare Blank_Line: String := Get_Line(F); begin null; end;

      end;

   end loop;

   Close(F);

   Put("there are "); Put(Natural(All_Tiles.Length), 0); Put(" tiles");
   New_Line;

   -- SECTION
   -- part 1: determine the corner tiles and report the product of their numbers
   --    (this seems inelegant)

   declare

      -- Corner_Range is probably overkill
      subtype Corner_Range is Positive range 1 .. 4;
      Corner_Tiles: array ( Corner_Range ) of Natural := ( others => 0 );

      Currently_Seeking: Corner_Range := 1; -- a counter for Corner_Tiles

      Product: Long_Integer;

   begin

      Outer_Tile_Loop:
      for I of Tile_Indices loop

         declare

            T: Tile_Maps.Reference_Type renames All_Tiles(I);
            Number_Found: Natural := 0;

         begin

            Inner_Tile_Loop:
            for J of Tile_Indices loop

               if I /= J then

                  declare U: Tile_Maps.Reference_Type renames All_Tiles(J);
                  begin

                     -- match sides
                     for T_Side in Tile_Sides loop

                        -- no point in re-matching when we already have a match
                        if T.Matches(T_Side) = 0 then

                           for U_Side in Tile_Sides loop

                              if T.Sides(T_Side) = U.Sides(U_Side) or else
                                    T.Sides(T_Side) = U.Sides_Reversed(U_Side)
                              then
                                 T.Matches(T_Side) := J;
                                 U.Matches(U_Side) := I;
                              end if;

                           end loop;

                        end if;

                     end loop;

                  end;

                  exit Inner_Tile_Loop when
                        ( for all Side in Tile_Sides => T.Matches(Side) /= 0 );
               end if;

            end loop Inner_Tile_Loop;

            Number_Found := 0;

            for Side in Tile_Sides loop
               if T.Matches(Side) /= 0 then
                  Number_Found := Number_Found + 1;
               end if;
            end loop;

            if Number_Found = 2 then
               Corner_Tiles(Currently_Seeking) := I;
               if Currently_Seeking < 4 then
                  Currently_Seeking := Currently_Seeking + 1;
               end if;
            end if;

         end;

      end loop Outer_Tile_Loop;

      Put("corner tiles: ");
      Put(Corner_Tiles(1), 0); Put(' ');
      Put(Corner_Tiles(2), 0); Put(' ');
      Put(Corner_Tiles(3), 0); Put(' ');
      Put(Corner_Tiles(4), 0); Put(' ');
      New_Line;
      Put("their product is ");
      Product := Long_Integer(Corner_Tiles(1)) * Long_Integer(Corner_Tiles(2))
                * Long_Integer(Corner_Tiles(3)) * Long_Integer(Corner_Tiles(4));
      Put(Product'Image); New_Line;

      -- SECTION
      -- part 2: put the tiles together into an image and detect monsters!

      Build_Image( Corner_Tiles(1) );

      -- flatten it (I should do this in the Build_Image phase but I'm tired)
      Flatten_Image;

      -- go monster hunting

      declare

         Monsters_Found: Natural := Number_Of_Monsters;

      begin

         -- rotate the image 3 times, look for monsters each time
         -- if you don't see one on a rotation, try flipping it, too
         -- if you do see one, AWESOME, quit the loop & report
         for I in 1 .. 3 loop

            exit when Monsters_Found > 0;

            Rotate_Flattened_Image;
            Monsters_Found := Number_Of_Monsters;

            if Monsters_Found = 0 then
               Flip_Flattened_Image;
               Monsters_Found := Number_Of_Monsters;
               if Monsters_Found = 0 then Flip_Flattened_Image; end if;
            else
               exit;
            end if;

         end loop;

         Put("found "); Put(Monsters_Found, 0); Put(" monsters"); New_Line;

      end;

      -- update the map with monsters so we can count rough seas

      for I in Monsters_Exposed'Range(1) loop

         for J in Monsters_Exposed'Range(2) loop

            Put( (
                 if Monsters_Exposed(I,J) = Calm then '.'
                 elsif Monsters_Exposed(I,J) = Rough then '#'
                 else 'O'
               ) );

         end loop;

         New_Line;

      end loop;

      Put("water roughness: "); Put(Count_Of_Roughness, 0); New_Line;

--        Rotate_Flattened_Image; Rotate_Flattened_Image;
--        Put("try again: "); New_Line; Put(Number_Of_Monsters, 0); New_Line;
--        Put_Flattened_Image;

  end;

end Main;
