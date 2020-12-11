-- Advent of Code 2020, Day 10
--
-- John Perry
--
-- apologies for any bad Ada style
--
-- seating system
--
-- essentially two variants on the game of Life

with Ada.Text_IO;
use Ada.Text_IO;

with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;

procedure Main is

   -- I/O

   F: File_Type;

   -- a seat and its possibilities

   type Status is ( Floor, Empty, Occupied );

   Num_Rows, Num_Cols: Natural := 0;

   -- seating chart will be an array b/c I didn't feel like vectors today

   type Chart_Array is array( Positive range <> , Positive range <> ) of Status;

   procedure Put(Chart: Chart_Array) is
   -- displays Chart according to the format shown in the problem
   begin
      for I in 1 .. Chart'Length(1) loop
         for J in 1 .. Chart'Length(2) loop
            Put( (
                if Chart(I,J) = Empty then 'L'
                elsif Chart(I,J) = Floor then '.'
                else '#'
            ) );
         end loop;
         New_Line(1);
      end loop;
   end Put;

   function First_In_Direction_Is_Occupied(
         Chart: Chart_Array; I, J: Positive; Dx, Dy: Integer
   ) return Boolean
   -- determines whether the first seat in sight of the Chart position (I,J)
   -- is Occuped, where Dx and Dy indicate the direction to take
   is

      First_Seat_Is_Occupied: Boolean := False; -- our result

      M: constant Positive := Chart'Length(1);
      N: constant Positive := Chart'Length(2);

      X: Positive := I;
      Y: Positive := J;

   begin

      while X + Dx > 0 and X + Dx <= M and Y + Dy > 0 and Y + Dy <= N loop

         X := X + Dx;
         Y := Y + Dy;

         if Chart(X,Y) = Empty then exit;

         elsif Chart(X,Y) = Occupied then

            First_Seat_Is_Occupied := True;
            exit;

         end if;

      end loop;

      return First_Seat_Is_Occupied;

   end First_In_Direction_Is_Occupied;

   function Number_Occupied_In_Sight(Chart: Chart_Array; I, J: Positive)
   return Natural
   -- returns the number of occupied seats in sight of Chart position (I,J)
   is

      Result: Natural := 0;

      M: constant Positive := Chart'Length(1);
      N: constant Positive := Chart'Length(2);

   begin

      -- check all 8 valid directions

      if First_In_Direction_Is_Occupied(Chart, I, J, -1, -1) then
         Result := Result + 1;
      end if;

      if First_In_Direction_Is_Occupied(Chart, I, J, -1,  0) then
         Result := Result + 1;
      end if;

      if First_In_Direction_Is_Occupied(Chart, I, J, -1,  1) then
         Result := Result + 1;
      end if;

      if First_In_Direction_Is_Occupied(Chart, I, J,  0, -1) then
         Result := Result + 1;
      end if;

      if First_In_Direction_Is_Occupied(Chart, I, J,  0,  1) then
         Result := Result + 1;
      end if;

      if First_In_Direction_Is_Occupied(Chart, I, J,  1, -1) then
         Result := Result + 1;
      end if;

      if First_In_Direction_Is_Occupied(Chart, I, J,  1,  0) then
         Result := Result + 1;
      end if;

      if First_In_Direction_Is_Occupied(Chart, I, J,  1,  1) then
         Result := Result + 1;
      end if;

      return Result;

   end Number_Occupied_In_Sight;

   function Number_Occupied_Adjacent(Chart: Chart_Array; I, J: Positive)
   return Natural
   -- counts the number of seats adjacent to Chart position (I,J) that are
   -- occupied
   --
   -- it might have been smarter to do something similar here to
   -- Number_Occupied_In_Sight but what's done is done
   is

      Result: Natural := 0;

      M: constant Positive := Chart'Length(1);
      N: constant Positive := Chart'Length(2);

   begin

      -- the first four check the corners

      if I = 1 and J = 1 then

         if Chart(1,2) = Occupied then Result := Result + 1; end if;
         if Chart(2,1) = Occupied then Result := Result + 1; end if;
         if Chart(2,2) = Occupied then Result := Result + 1; end if;

      elsif I = 1 and J = N then

         if Chart(1,N-1) = Occupied then Result := Result + 1; end if;
         if Chart(2,N  ) = Occupied then Result := Result + 1; end if;
         if Chart(2,N-1) = Occupied then Result := Result + 1; end if;

      elsif I = M and J = 1 then

         if Chart(M-1,1) = Occupied then Result := Result + 1; end if;
         if Chart(M-1,2) = Occupied then Result := Result + 1; end if;
         if Chart(M  ,2) = Occupied then Result := Result + 1; end if;

      elsif I = M and J = N then

         if Chart(M  ,N-1) = Occupied then Result := Result + 1; end if;
         if Chart(M-1,N  ) = Occupied then Result := Result + 1; end if;
         if Chart(M-1,N-1) = Occupied then Result := Result + 1; end if;

      -- the next four check the extreme rows' interiors
      elsif I = 1 then

         if Chart(1,J-1) = Occupied then Result := Result + 1; end if;
         if Chart(2,J-1) = Occupied then Result := Result + 1; end if;
         if Chart(2,J  ) = Occupied then Result := Result + 1; end if;
         if Chart(2,J+1) = Occupied then Result := Result + 1; end if;
         if Chart(1,J+1) = Occupied then Result := Result + 1; end if;

      elsif I = M then

         if Chart(M  ,J-1) = Occupied then Result := Result + 1; end if;
         if Chart(M-1,J-1) = Occupied then Result := Result + 1; end if;
         if Chart(M-1,J  ) = Occupied then Result := Result + 1; end if;
         if Chart(M-1,J+1) = Occupied then Result := Result + 1; end if;
         if Chart(M  ,J+1) = Occupied then Result := Result + 1; end if;

      elsif J = 1 then

         if Chart(I-1,1) = Occupied then Result := Result + 1; end if;
         if Chart(I-1,2) = Occupied then Result := Result + 1; end if;
         if Chart(I  ,2) = Occupied then Result := Result + 1; end if;
         if Chart(I+1,2) = Occupied then Result := Result + 1; end if;
         if Chart(I+1,1) = Occupied then Result := Result + 1; end if;

      elsif J = N then

         if Chart(I-1,N  ) = Occupied then Result := Result + 1; end if;
         if Chart(I-1,N-1) = Occupied then Result := Result + 1; end if;
         if Chart(I  ,N-1) = Occupied then Result := Result + 1; end if;
         if Chart(I+1,N-1) = Occupied then Result := Result + 1; end if;
         if Chart(I+1,N  ) = Occupied then Result := Result + 1; end if;

      -- this checks strictly interior cells

      else

         if Chart(I-1,J-1) = Occupied then Result := Result + 1; end if;
         if Chart(I-1,J  ) = Occupied then Result := Result + 1; end if;
         if Chart(I-1,J+1) = Occupied then Result := Result + 1; end if;
         if Chart(I  ,J-1) = Occupied then Result := Result + 1; end if;
         if Chart(I  ,J+1) = Occupied then Result := Result + 1; end if;
         if Chart(I+1,J+1) = Occupied then Result := Result + 1; end if;
         if Chart(I+1,J  ) = Occupied then Result := Result + 1; end if;
         if Chart(I+1,J-1) = Occupied then Result := Result + 1; end if;

      end if;

      return Result;

   end Number_Occupied_Adjacent;

   -- a type that allows us to use the same basic iteration code
   -- with different checking functions

   type Checking_Function
         is access function(Chart: Chart_Array; I, J: Positive) return Natural;

   procedure Perform_Round(
         Source: in Chart_Array;
         Result: out Chart_Array;
         Differ: out Boolean;
         Method: Checking_Function;
         Intolerance: Positive
   )
   -- performs one round of the seating iteration
   -- analyzes Source, and writes the result of the seating rules to Result
   -- Differ is True if and only if one of the seats changes Status
   -- Method allows us to choose Number_Occupied_Adjacent
   --    or Number_Occupied_In_Sight
   -- Intolerance indicates how many occupied seats Method should detect for
   --    a set to become empty
   is

      M: constant Positive := Source'Length(1);
      N: constant Positive := Source'Length(2);

   begin

      Differ := False; -- must detect a change

      Rows:
      for I in 1 .. M loop

         Columns:
         for J in 1 .. N loop

            case Source(I,J) is

            when Floor => Result(I,J) := Floor; -- floor remains floor

            when Empty =>

               -- occupy only if all neighbors are empty

               if Method(Source, I, J) /= 0
               then
                  Result(I,J) := Empty;
               else
                  Differ := True;
                  Result(I,J) := Occupied;
               end if;

            when Occupied =>

               -- remain occupied only if neighbors are lower than tolerance

               if Method(Source, I, J) < Intolerance
               then
                  Result(I,J) := Occupied;
               else
                  Differ := True;
                  Result(I,J) := Empty;
               end if;

            end case;

         end loop Columns;

      end loop Rows;

   end Perform_Round;

   procedure Iterate_Seating(
         Chart: in out Chart_Array;
         Method: Checking_Function;
         Intolerance: Positive := 4
   )
   -- repeat the rules on Chart, using Method to detect the number of occupied
   -- seats we care about, with the given level of Intolerance for neighbors
   is

      M: constant Positive := Chart'Length(1);
      N: constant Positive := Chart'Length(2);

      -- double-buffered seating charts

      Chart1: Chart_Array( 1 .. M , 1 .. N ) := Chart;
      Chart2: Chart_Array( 1 .. M , 1 .. N );

      Changing_First: Boolean := True; -- True iff we are changing Chart 1
                                       -- otherwise we are changing Chart 2

      Changed: Boolean := True; -- whether the seating chart changed on a loop

   begin

      while Changed loop

         if Changing_First then Perform_Round(
                                              Source => Chart1,
                                              Result => Chart2,
                                              Differ => Changed,
                                              Method => Method,
                                              Intolerance => Intolerance
                                             );
         else Perform_Round(
                            Source => Chart2, Result => Chart1,
                            Differ => Changed,
                            Method => Method,
                            Intolerance => Intolerance
                           );
         end if;

         Changing_First := not Changing_First; -- switch buffers

      end loop;

      -- now that we're done, copy final chart to function's chart

      Chart := ( if Changing_First then Chart1 else Chart2 );

   end Iterate_Seating;

   function Count_Occupied_Seats(Chart: Chart_Array) return Natural is
   -- count the number of occupied seats in Chart
      Result: Natural := 0;
   begin
      for I in 1 .. Chart'Length(1) loop
         for J in 1 .. Chart'Length(2) loop
            if Chart(I,J) = Occupied then Result := Result + 1; end if;
         end loop;
      end loop;
      return Result;
   end Count_Occupied_Seats;

begin

   -- I didn't want to deal with vectors, so this time I read the file twice:
   -- once to determine the dimensions of the array I'd need, and again to
   -- read the array into memory

   -- first pass: determine dimensions

   Open(F, In_File, "/Users/user/common/Ada/AoC2020/Day11/input.txt");

   declare

      S: String := Get_Line(F);

   begin

      Num_Cols := S'Length;
      Num_Rows := 1;

      while not End_Of_File(F) loop
         S := Get_Line(F);
         Num_Rows := Num_Rows + 1;
      end loop;

   end;

   Close(F);

   -- second pass: read the seating chart

   declare

      I: Natural := 1; -- current row

      -- we'll use Seating_Chart for the rest of the program

      Seating_Chart: Chart_Array ( 1 .. Num_Rows , 1 .. Num_Cols );

   begin

      Open(F, In_File, "/Users/user/common/Ada/AoC2020/Day11/input.txt");

      while not End_Of_File(F) loop

         declare S: String := Get_Line(F);

         begin

            for J in 1 .. Num_Cols loop
               Seating_Chart(I, J) := (
                     if    S(J) = '.' then Floor
                     elsif S(J) = 'L' then Empty
                     else  Occupied
               );
            end loop;
            I := I + 1;

         end;

      end loop;

      Close(F);

      -- exercise 1: iterate until we arrive at a stable state by considering
      -- adjacent seats only and a tolerance for fewer than four neighbors,
      -- then report the number of occupied seats

      declare

         First_Chart: Chart_Array := Seating_Chart;

      begin

         Iterate_Seating(
                         Chart => First_Chart,
                         Method => Number_Occupied_Adjacent'Access
                        );

         Put(Count_Occupied_Seats(First_Chart), 0); Put(" seats are occupied");
         New_Line(1);

      end;

      Put("----------"); New_Line(1);

      -- exercise 2: iterate until we arrive at a stable state by considering
      -- first seats in sight and a tolerance for fewer than five neighbors,
      -- then report the number of occupied seats

      declare

         Second_Chart: Chart_Array := Seating_Chart;

      begin

         Iterate_Seating(
                         Chart => Second_Chart,
                         Method => Number_Occupied_In_Sight'Access,
                         Intolerance => 5
                        );

         Put(Count_Occupied_Seats(Second_Chart), 0); Put(" seats are occupied");
         New_Line(1);

      end;

   end;

end Main;
