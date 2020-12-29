-- Advent of Code 2020, Day 24
--
-- John Perry
--
-- apologies for any bad Ada style
--
-- Lobby Layout
--
-- flip hexagonal tiles
--
-- part 1: follow directions, report black tiles
--
-- part 2: another variant on the game of life, this time with the hexagonal
-- tiles
--

pragma Ada_2020;

with Ada.Text_IO;
use Ada.Text_IO;

with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;

procedure Main is

   -- SECTION
   -- input-related

   F: File_Type; -- input file

   Testing: constant Boolean := False;

   Filename: constant String
         := (
             if Testing then "/Users/user/common/Ada/AoC2020/Day24/example.txt"
             else "/Users/user/common/Ada/AoC2020/Day24/input.txt"
            );

   -- SECTION
   -- definition of a tile

   type Location is record
      X, Y: Integer;
   end record;

   package Tiles is
   -- effectively a class for tiles

      type Tile_Colors is ( Black, White );

      type Tile is tagged private;

      function Initialize_To(X, Y: Integer) return Tile;
      -- initializes a tile to position (X, Y), color white, returns it

      procedure Flip(T: in out Tile);
      -- changes T's color from white to black or vice versa

      function Color(T: in Tile) return Tile_Colors;
      -- returns T's color

      function My_Location(T: in Tile) return Location;
      -- returns T's Location

      -- the following give the location that neighbors T,
      -- in the indicated direction
      -- I've set it up as follows:
      --
      --     (-1,+1) (+0,+1)
      --         \     /
      -- (-1,+0)--(X,Y)--(+1,+0)
      --         /     \
      --     (-1,-1) (+0,-1)
      --
      -- ...so long as Y mod 2 is 0. When Y mod 2 is 1, modify accordingly.

      function NE_Location(T: in Tile) return Location;
      function E_Location(T: in Tile) return Location;
      function SE_Location(T: in Tile) return Location;
      function SW_Location(T: in Tile) return Location;
      function W_Location(T: in Tile) return Location;
      function NW_Location(T: in Tile) return Location;

   private

      type Tile is tagged record
         Located_At: Location;
         Color: Tile_Colors := White;
      end record;

   end Tiles;

   package body Tiles is

      function Initialize_To(X, Y: Integer) return Tile is
      ( ( Located_At => ( X => X, Y => Y ) , Color => <> ) );

      function Color(T: in Tile) return Tile_Colors is ( T.Color );

      procedure Flip(T: in out Tile) is
      begin
         T.Color := ( if T.Color = White then Black else White );
      end Flip;

      function My_Location(T: in Tile) return Location is
      ( ( T.Located_At.X, T.Located_At.Y ) );

      function NE_Location(T: in Tile) return Location is
      (
            if T.Located_At.Y mod 2 = 0
            then ( T.Located_At.X, T.Located_At.Y + 1 )
            else ( T.Located_At.X + 1, T.Located_At.Y + 1 )
      );

      function E_Location(T: in Tile) return Location is
      ( ( T.Located_At.X + 1, T.Located_At.Y ) );

      function SE_Location(T: in Tile) return Location is
      (
            if T.Located_At.Y mod 2 = 0
            then ( T.Located_At.X, T.Located_At.Y - 1 )
            else ( T.Located_At.X + 1, T.Located_At.Y - 1 )
      );

      function W_Location(T: in Tile) return Location is
      ( ( T.Located_At.X - 1, T.Located_At.Y ) );

      function SW_Location(T: in Tile) return Location is
      (
            if T.Located_At.Y mod 2 = 0
            then ( T.Located_At.X - 1, T.Located_At.Y - 1 )
            else ( T.Located_At.X, T.Located_At.Y - 1 )
      );

      function NW_Location(T: in Tile) return Location is
      (
            if T.Located_At.Y mod 2 = 0
            then ( T.Located_At.X - 1, T.Located_At.Y + 1 )
            else ( T.Located_At.X, T.Located_At.Y + 1 )
      );

   end Tiles;

   Endpoint: constant := 100; -- set to a safe number, larger than the floor

   type Floor_Array is
         array ( -Endpoint .. Endpoint, -Endpoint .. Endpoint ) of Tiles.Tile;

   Floor: Floor_Array
         := (
             for I in -Endpoint .. Endpoint =>
                (
                 for J in -Endpoint .. Endpoint =>
                       ( Tiles.Initialize_To( I, J ) )
                )
            );

   procedure Put(F: Floor_Array) is
   -- show the floor (useful for debugging, but painful if Endpoint is large)
      use Tiles;
   begin
      for I in -Endpoint .. Endpoint loop
         for J in -Endpoint .. Endpoint loop
            Put( ( if F(I,J).Color = Black then '*' else '.' ) );
         end loop;
         New_Line;
      end loop;
   end Put;

   procedure Flip_Tile_Indicated_By(S: String) is
   -- for part 1: read the string, flip the corresponding tile

      Loc: Location := ( 0, 0 );
      Pos: Natural := S'First;

   begin

      while Pos <= S'Last loop -- first locate the tile

         case S(Pos) is

            when 'n' =>
               case S(Pos + 1) is
                  when 'e' => Loc := Floor(Loc.X, Loc.Y).NE_Location;
                  when 'w' => Loc := Floor(Loc.X, Loc.Y).NW_Location;
                  when others => raise Data_Error with "can only read ne or nw";
               end case;
               Pos := Pos + 1;

            when 's' =>
               case S(Pos + 1) is
                  when 'e' => Loc := Floor(Loc.X, Loc.Y).SE_Location;
                  when 'w' => Loc := Floor(Loc.X, Loc.Y).SW_Location;
                  when others => raise Data_Error with "can only read se or sw";
               end case;
               Pos := Pos + 1;

            when 'e' => Loc := Floor(Loc.X, Loc.Y).E_Location;

            when 'w' => Loc := Floor(Loc.X, Loc.Y).W_Location;

            when others =>
               raise Data_Error with "can only read ne, nw, se, sw, e, w";

         end case;

         Pos := Pos + 1;

      end loop;

      Floor(Loc.X, Loc.Y).Flip; -- now flip it

   end;

   -- making a feature of Tiles visible
   function "="(A, B: Tiles.Tile_Colors) return Boolean is
   ( Tiles."="(A, B) );

   function Count_Black_Neighbors(T: Tiles.Tile) return Natural is
   -- count the number of black neighbors around T

      Result: Natural := 0;

      -- locaiton of T
      Loc: Location := T.My_Location;
      X: Integer := Loc.X;
      Y: Integer := Loc.Y;

      Black renames Tiles.Black;

   begin

      -- west
      if Floor(X - 1, Y).Color = Black then Result := Result + 1; end if;
      --east
      if Floor(X + 1, Y).Color = Black then Result := Result + 1; end if;

      -- for ne, nw, se, sw, check which sort of row we're on
      if Y mod 2 = 0 then

         -- ne
         if Floor(X, Y + 1).Color = Black then Result := Result + 1; end if;
         -- se
         if Floor(X, Y - 1).Color = Black then Result := Result + 1; end if;
         -- nw
         if Floor(X - 1, Y + 1).Color = Black then Result := Result + 1; end if;
         -- sw
         if Floor(X - 1, Y - 1).Color = Black then Result := Result + 1; end if;

      else

         -- nw
         if Floor(X, Y + 1).Color = Black then Result := Result + 1; end if;
         -- sw
         if Floor(X, Y - 1).Color = Black then Result := Result + 1; end if;
         -- ne
         if Floor(X + 1, Y + 1).Color = Black then Result := Result + 1; end if;
         -- se
         if Floor(X + 1, Y - 1).Color = Black then Result := Result + 1; end if;

      end if;

      return Result;

   end Count_Black_Neighbors;

   function Should_Flip(T: Tiles.Tile) return Boolean is
   -- whether we should flip T according to the living art exhibit
   ( (
      if T.Color = Tiles.Black then
      ( Count_Black_Neighbors(T) = 0 or Count_Black_Neighbors(T) > 2 )
      else
      ( Count_Black_Neighbors(T) = 2 )
   ) );

   procedure Apply_Living_Art_Rule is
   -- apply one iteration of the living art exhibit

      Result: Floor_Array; -- double-buffered art: read Floor, write to Result

   begin

      for I in -Endpoint + 1 .. Endpoint - 1 loop
         for J in -Endpoint + 1 .. Endpoint - 1 loop
            Result(I,J) := Floor(I,J);
            if Should_Flip(Floor(I,J)) then Result(I,J).Flip; end if;
         end loop;
      end loop;

      Floor := Result;

   end;

   function Number_Of_Black_Tiles return Natural is
   -- number of black tiles on Floor

      Black_Tiles: Natural := 0;

   begin

      for I in Floor'First(1) .. Floor'Last(1) loop
         for J in Floor'First(2) .. Floor'Last(2) loop
            if Floor(I,J).Color = Tiles.Black then
               Black_Tiles := Black_Tiles + 1;
            end if;
         end loop;
      end loop;

      return Black_Tiles;

   end Number_Of_Black_Tiles;

begin

   -- SECTION
   -- read and follow instructions

   Open(F, In_File, Filename);

   while not End_Of_File(F) loop
      declare
         S: String := Get_Line(F);
      begin
         Flip_Tile_Indicated_By(S);
      end;
   end loop;

   Close(F);

   -- part 1: count number of tiles flipped to black

   Put(Number_Of_Black_Tiles, 0); Put_Line(" tiles are black");

   -- part 2: living art exhibit

   for I in 0 .. 100 loop
      if I in 0 .. 10 or else I mod 10 = 0 or else I = 99 then
         -- Put(Floor);
         Put("day "); Put(I, 0); Put(": "); Put(Number_Of_Black_Tiles, 0);
         New_Line(2);
      end if;
      Apply_Living_Art_Rule;
   end loop;

end Main;
