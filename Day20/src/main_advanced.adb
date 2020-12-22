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
-- part 2:
--
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

   F: File_Type; -- input file

   Testing: constant Boolean
         := True;
         -- := False;

   Filename: constant String
         := (
             if False then "/Users/user/common/Ada/AoC2020/Day20/input.txt"
             else "/Users/user/common/Ada/AoC2020/Day20/example.txt"
            );

   Tiles_Per_Image_Side: constant := ( if Testing then 3 else 12 );

   subtype Tile_Dimension is Positive range 1 .. 10;

   type Tile_Sides is ( North, South, East, West );

   type Side_Data is array ( Tile_Dimension'Range ) of Boolean;

   type Raw_Tile_Data
         is array ( Tile_Dimension'Range , Tile_Dimension'Range ) of Boolean;

   type Edge_Data is array ( Tile_Sides ) of Side_Data;

   type Matched_Sides is array ( Tile_Sides ) of Natural;

   type Rotation is (None, Rot_One, Rot_Two, Rot_Three);

   type Tile is record
      Raw_Data: Raw_Tile_Data;
      Sides: Edge_Data;
      Sides_Reversed: Edge_Data;
      Matches: Matched_Sides := ( others => 0 );
      Rotates: Rotation := None;
      Flips: Boolean := False;
   end record;

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

   package Tile_Index_Sets is new Ada.Containers.Hashed_Sets
   (
    Element_Type => Natural,
    Hash => Natural_Hash,
    Equivalent_Elements => "="
   );

   Tile_Indices: Tile_Index_Sets.Set;

   function Get_Side(T: Tile; Which: Tile_Sides) return Side_Data is
      Result: Side_Data;
   begin
      for I in 1 .. 10 loop
         Result(I) := (
            case Which is
               when North => T.Raw_Data(  1,  I ),
               when South => T.Raw_Data( 10,  I ),
               when East  => T.Raw_Data(  I, 10 ),
               when West  => T.Raw_Data(  I,  1 )
         );
      end loop;
      return Result;
   end Get_Side;
-- again, the current GNAT bug
--     (
--        for I in 1 .. 10 => (
--           case Which is
--           when North => T.Raw_Data(  1,  I ),
--           when South => T.Raw_Data( 10,  I ),
--           when East  => T.Raw_Data(  I, 10 ),
--           when West  => T.Raw_Data(  I,  1 )
--        )
--     );

   procedure Get(F: File_Type; T: out Tile) is
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
   begin
      for I in Tile_Dimension'Range loop
         Put( ( if S(I) then '#' else '.' ) );
      end loop;
   end Put;

   procedure Put(T: Tile; Edges_Too: Boolean := False) is
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

   type Processed_Image_Tile is array ( 1 .. 8, 1 .. 8 ) of Boolean;

   Image: array( 1 .. Tiles_Per_Image_Side , 1 .. Tiles_Per_Image_Side )
         of Processed_Image_Tile;

   procedure Rotate_Data(T: in out Tile) is
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

   type Tile_Correspondence
         is array(
                  1 .. Tiles_Per_Image_Side,
                  1 .. Tiles_Per_Image_Side
                 ) of Natural;

   procedure Place_Tile(
         Which, I, J: Natural; Rot: Rotation;
         Assignments: in out Tile_Correspondence
   ) is
      T: Tile_Maps.Reference_Type renames All_Tiles(Which);
   begin
      Assignments(I, J) := Which;
      for R in Rotation'Pos(None) .. Rotation'Pos(Rot) - 1 loop
         Rotate_Data(All_Tiles(Which));
      end loop;
      if I = 1 and then T.Matches(North) /= 0 then
         Flip_Data(T);
      elsif I > 1 and then T.Matches(North) /= Assignments(I - 1, J) then
         Flip_Data(T);
      end if;
      for K in 1 .. 8 loop
         for L in 1 .. 8 loop
            Image(I, J)(K, L) := T.Raw_Data(K+1,L+1);
         end loop;
      end loop;
   end Place_Tile;

   function Determine_Rotation_For_West_Tile(T: Tile; West_Index: Natural)
   return Rotation
   is
   (
         if T.Matches(West) = West_Index then None
         elsif T.Matches(North) = West_Index then Rot_Three
         elsif T.Matches(East) = West_Index then Rot_Two
         else Rot_One
   );

   function Determine_Rotation_For_West_Edge(T: Tile) return Rotation is
   (
         if T.Matches(West) = 0 then None
         elsif T.Matches(North) = 0 then Rot_Three
         elsif T.Matches(East) = 0 then Rot_Two
         else Rot_One
   );

   procedure Build_Image( First_Corner: Natural ) is
      Assignments: Tile_Correspondence;
      Rot: Rotation;
      Flip: Boolean := False;
      NW: Tile_Maps.Reference_Type renames All_Tiles(First_Corner);
      This_Index: Natural := First_Corner;
      Prev_Index: Natural := First_Corner;
   begin

      -- northwest corner first

      Rot := Determine_Rotation_For_West_Edge(Nw);
      Place_Tile( First_Corner, 1, 1, Rot, Assignments );

      -- remainder of first row

      for J in 2 .. Tiles_Per_Image_Side loop
         declare Prev_Tile: Tile_Maps.Reference_Type
               renames All_Tiles(This_Index);
         begin
            This_Index := Prev_Tile.Matches(East);
            declare
               This_Tile: Tile_Maps.Reference_Type
                     renames All_Tiles(This_Index);
            begin
               Rot := Determine_Rotation_For_West_Tile(
                                           This_Tile, Assignments(1, J - 1)
                                                      );
               Place_Tile(This_Index, 1, J, Rot, Assignments);
            end;
         end;
      end loop;

      -- remaining rows

      for I in 2 .. Tiles_Per_Image_Side loop

         This_Index := All_Tiles(Assignments(I - 1, 1)).Matches(South);
         declare
            This_Tile: Tile_Maps.Reference_Type renames All_Tiles(This_Index);
         begin
            Rot := Determine_Rotation_For_West_Edge(This_Tile);
         end;
         Place_Tile(This_Index, I, 1, Rot, Assignments);

         for J in 2 .. Tiles_Per_Image_Side loop

            This_Index := All_Tiles(Assignments(I - 1, J)).Matches(South);
            declare
               This_Tile: Tile_Maps.Reference_Type
                     renames All_Tiles(This_Index);
            begin
               Rot := Determine_Rotation_For_West_Tile(
                     This_Tile, Assignments(I, J-1)
               );
            end;
            Place_Tile(This_Index, I, J, Rot, Assignments);

         end loop;

      end loop;

   end Build_Image;

begin

   Open(F, In_File, Filename);

   while not End_Of_File(F) loop

      -- format: repetitions of
      --    Tile (Natural):
      --    (10x10 array of #, .)

      declare
         S: String := Get_Line(F);
         Which: Natural := 0;
         T: Tile;
         Pos: Natural := S'First;
      begin
         Pos := Pos + 5; -- "Tile "
         while S(Pos) in '0'..'9' loop
            Which := Which * 10 + Character'Pos(S(Pos)) - Character'Pos('0');
            Pos := Pos + 1;
         end loop;
         -- Put("reading tile "); Put(Which, 0); New_Line;
         Get(F, T);
         All_Tiles.Insert( Which, T );
         Tile_Indices.Insert( Which );
         declare Blank_Line: String := Get_Line(F); begin null; end;
         -- Put(T); New_Line;
      end;

   end loop;

   Close(F);

   Put("there are "); Put(Natural(All_Tiles.Length), 0); Put(" tiles");
   New_Line;

   declare
      subtype Corner_Range is Positive range 1 .. 4;
      Corner_Tiles: array ( Corner_Range ) of Natural := ( others => 0 );
      Currently_Seeking: Corner_Range := 1;
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

      Build_Image( Corner_Tiles(1) );

      for I in 1 .. Tiles_Per_Image_Side loop
         for K in 1 .. Tile_Dimension'Last - 2 loop
            for J in 1 .. Tiles_Per_Image_Side loop
               for L in 1 .. Tile_Dimension'Last - 2 loop
                  Put( ( if Image(I,J)(K,L) then '#' else '.' ) );
               end loop;
            end loop;
         New_Line;
         end loop;
      end loop;

  end;

end Main;
