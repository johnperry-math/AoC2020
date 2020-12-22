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

   Filename: constant String
         := "/Users/user/common/Ada/AoC2020/Day20/input.txt";
         -- := "/Users/user/common/Ada/AoC2020/Day20/example.txt";

   subtype Tile_Dimension is Positive range 1 .. 10;

   type Tile_Sides is ( North, South, East, West );

   type Side_Data is array ( Tile_Dimension'Range ) of Boolean;

   type Raw_Tile_Data
         is array ( Tile_Dimension'Range , Tile_Dimension'Range ) of Boolean;

   type Edge_Data is array ( Tile_Sides ) of Side_Data;

   type Matched_Sides is array ( Tile_Sides ) of Natural;

   type Tile is record
      Raw_Data: Raw_Tile_Data;
      Sides: Edge_Data;
      Sides_Reversed: Edge_Data;
      Matches: Matched_Sides := ( others => 0 );
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
--        Put(Which'Image); Put(" side: "); Put(Result'Image); New_Line;
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

   procedure Put(T: Tile) is
   begin
      for I in Tile_Dimension'Range loop
         for J in Tile_Dimension'Range loop
            Put( ( if T.Raw_Data(I,J) then '#' else '.' ) );
         end loop;
         New_Line;
      end loop;
      Put("north: "); Put(T.Sides(North)); New_Line;
      Put("reversed: "); Put(T.Sides_Reversed(North)); New_Line;
      Put("south: "); Put(T.Sides(South)); New_Line;
      Put("reversed: "); Put(T.Sides_Reversed(South)); New_Line;
      Put("east:  "); Put(T.Sides(East )); New_Line;
      Put("reversed: "); Put(T.Sides_Reversed(East)); New_Line;
      Put("west:  "); Put(T.Sides(West )); New_Line;
      Put("reversed: "); Put(T.Sides_Reversed(West)); New_Line;
   end Put;

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
         Put("reading tile "); Put(Which, 0); New_Line;
         Get(F, T);
         All_Tiles.Insert( Which, T );
         Tile_Indices.Insert( Which );
         declare Blank_Line: String := Get_Line(F); begin null; end;
         Put(T); New_Line;
      end;

   end loop;

   Close(F);

   declare
      subtype Corner_Range is Positive range 1 .. 4;
      Corner_Tiles: array ( Corner_Range ) of Natural := ( others => 0 );
      Currently_Seeking: Corner_Range := 1;
      Product: Long_Integer;
   begin
      Outer_Tile_Loop:
      for I of Tile_Indices loop
         declare
            T: Tile := All_Tiles(I);
            Number_Found: Natural := 0;
         begin
            Inner_Tile_Loop:
            for J of Tile_Indices loop
               if I /= J then
                  declare U: Tile := All_Tiles(J);
                  begin
                     -- match sides
                     for T_Side in Tile_Sides loop
                        if T.Matches(T_Side) = 0 then
                           for U_Side in Tile_Sides loop
                              if T.Sides(T_Side) = U.Sides(U_Side) or else
                                    T.Sides(T_Side) = U.Sides_Reversed(U_Side)
                              then
                                 Put(I, 0); Put(T_Side'Image); Put(" matches ");
                                 Put(J, 0); Put(U_Side'Image); New_Line;
                                 Number_Found := Number_Found + 1;
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
            Put(I, 0); Put(" matches "); Put(Number_Found, 0); New_Line;
            if Number_Found = 2 then
               Corner_Tiles(Currently_Seeking) := I;
               exit Outer_Tile_Loop when Currently_Seeking = 4;
               Currently_Seeking := Currently_Seeking + 1;
            end if;
         end;
      end loop Outer_Tile_Loop;
      Put(Corner_Tiles(1), 0); Put(' ');
      Put(Corner_Tiles(2), 0); Put(' ');
      Put(Corner_Tiles(3), 0); Put(' ');
      Put(Corner_Tiles(4), 0); Put(' ');
      Product := Long_Integer(Corner_Tiles(1)) * Long_Integer(Corner_Tiles(2))
                * Long_Integer(Corner_Tiles(3)) * Long_Integer(Corner_Tiles(4));
      Put(Product'Image); New_Line;
  end;

end Main;
