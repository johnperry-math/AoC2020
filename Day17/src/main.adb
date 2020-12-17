-- Advent of Code 2020, Day 16
--
-- John Perry
--
-- apologies for any bad Ada style
--
-- Conway Cubes
--
-- yet another variant of the game of life
--
-- part 1: count the number of cubes active in 3-space after 6 iterations,
-- given an initial state, with the rules that
--
--   - an inactive cube becomes active if it has exactly 3 active neighbors;
--
--   - an active cube becomes inactive if it does not have 2 or 3
--     active neighbors
--
-- part 2: same, but in 4-space

pragma Ada_2020;

with Ada.Text_IO;
use Ada.Text_IO;

with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;

procedure Main is

   F: File_Type; -- input file

   -- a cube can be active or inactive

   type Cube is ( Active, Inactive );

   -- a space is 3-dimensional

   type Space is
         array ( Integer range <> , Integer range <> , Integer range <> )
         of Cube;

   -- a hyperspace is 4-dimensional

   type Hyperspace is
         array (
                Integer range <> , Integer range <> ,
                Integer range <> , Integer range <>
               ) of Cube;

   function Embed_In_Hyperspace(S: Space) return Hyperspace
   -- embeds S in a hyperspace's 0th component
   is
      Result: Hyperspace(
                         0 .. 0 ,
                         S'First(1) - 1 .. S'Last(1) + 1 ,
                         S'First(2) - 1 .. S'Last(2) + 1 ,
                         S'First(3) - 1 .. S'Last(3) + 1
                        )
            := ( others => ( others => ( others => ( others => Inactive ) ) ) );
-- gnat crashes when I try the following initialization,
-- with Constraint_Error erroneous memory access
--              := (
--                  0 => (
--                        for I in -1 .. S'Last(1) + 1 => (
--                           for J in -1 .. S'Last(2) + 1 => (
--                              for K in -1 + S'Last(3) + 1 => (
--                                 if I = -1 or I = S'Last(1) + 1 then Inactive
--                                 elsif J = -1 or J = S'Last(2) + 1 then Inactive
--                                 elsif K = -1 or K = S'Last(3) + 1 then Inactive
--                                 else S(I,J,K)
--                              )
--                           )
--                        )
--                       )
--                 );
   begin
      for I in S'Range(1) loop
         for J in S'Range(2) loop
            for K in S'Range(3) loop
               Result(0,I,J,K) := S(I,J,K);
            end loop;
         end loop;
      end loop;
      return Result;
   end Embed_In_Hyperspace;

   function Read_Initial_State(Rows, Cols: Positive) return Space
   -- returns a space corresponding to the input's definition
   -- of the initial state
   is

      Row: Positive := 1;

      S: Space( 0 .. 0 , 1 .. Rows , 1 .. Cols )
            := ( others => ( others => ( others => Inactive ) ) );

   begin

      Open(F, In_File, "/Users/user/common/Ada/AoC2020/Day17/input.txt");

      while not End_Of_File(F) loop

         declare L: String := Get_Line(F);
         begin
            for I in L'Range loop
               S( 0, Row, I - L'First + 1 )
                     := ( if L(I) = '#' then Active else Inactive );
            end loop;
         end;

         Row := Row + 1;

      end loop;

      Close(F);

      return S;

   end Read_Initial_State;

   procedure Put(S: Space) is
   -- prints S to standard output
      Bar: String(1..S'Length(3)) := ( others => '-' );
   begin
      Put(Bar); New_Line(2);
      for I in S'Range(1) loop
         Put(I, 0); Put(": "); New_Line(1);
         for J in S'Range(2) loop
            for K in S'Range(3) loop
               Put( ( if S(I,J,K) = Active then '#' else '.' ) );
            end loop;
            New_Line(1);
         end loop;
         New_Line(2);
      end loop;
      Put(Bar);
      New_Line(2);
   end Put;

   function Number_Of_Neighbors(S: Space; I, J, K: Integer) return Natural is
   -- returns the number of active neighbors (in 3 dimensions) of S(I,J,K)
      Result: Natural := 0;
   begin
      for A in -1 .. 1 loop
         for B in -1 .. 1 loop
            for C in -1 .. 1 loop
               if ( A /= 0 or B /= 0 or C /= 0 )
                     and I+A in S'Range(1)
                     and J+B in S'Range(2)
                     and K+C in S'Range(3)
               then
                  if S(I+A,J+B,K+C) = Active then Result := Result + 1; end if;
               end if;
            end loop;
         end loop;
      end loop;
      return Result;
   end Number_Of_Neighbors;

   function Number_Of_Neighbors_4d(S: Hyperspace; I, J, K, L: Integer)
   -- returns the number of active neighbors (in 4 dimensions) of S(I,J,K,L)
   return Natural
   is
      Result: Natural := 0;
   begin
      for A in -1 .. 1 loop
         for B in -1 .. 1 loop
            for C in -1 .. 1 loop
               for D in -1 .. 1 loop
                  if ( A /= 0 or B /= 0 or C /= 0 or D /= 0 )
                        and I+A in S'Range(1)
                        and J+B in S'Range(2)
                        and K+C in S'Range(3)
                        and L+D in S'Range(4)
                  then
                     if S(I+A,J+B,K+C,L+D) = Active then
                        Result := Result + 1;
                     end if;
                  end if;
               end loop;
            end loop;
         end loop;
      end loop;
      return Result;
   end Number_Of_Neighbors_4d;

   function Result_Of_Testing(S: Space; I, J, K: Integer) return Cube is
   -- applies the rules to S(I,J,K)
   (
    if S(I,J,K) = Active and then Number_Of_Neighbors(S,I,J,K) not in 2 .. 3
    then Inactive
    elsif S(I,J,K) = Inactive and then Number_Of_Neighbors(S,I,J,K) = 3
    then Active
    else S(I,J,K)
   );

   function Result_Of_Testing_4d(S: Hyperspace; I, J, K, L: Integer) return Cube
   -- applies the rules to S(I,J,K,L)
   is
   (
    if S(I,J,K,L) = Active
         and then Number_Of_Neighbors_4d(S,I,J,K,L) not in 2 .. 3
    then Inactive
    elsif S(I,J,K,L) = Inactive and then Number_Of_Neighbors_4d(S,I,J,K,L) = 3
    then Active
    else S(I,J,K,L)
   );

   function Iterate(S: Space; Iterations: Positive) return Space is
   -- uses recursion to iterate S the number of times specified
      Copy, Result: Space(
                          S'First(1) - 1 .. S'Last(1) + 1,
                          S'First(2) - 1 .. S'Last(2) + 1,
                          S'First(3) - 1 .. S'Last(3) + 1
                         ) := ( others => ( others => ( others => Inactive ) ) );
-- the following lines cause Constraint_Error erroneous memory access from command line
--              := (
--                  for I in S'Range(1)
--                  => (
--                      for J in S'Range(2)
--                      => (
--                          for K in S'Range(3)
--                          => (
--                              if S(I,J,K) = Active
--                                    and then Number_Of_Neighbors(S,I,J,K) not in 2 .. 3
--                              then Inactive
--                              elsif S(I,J,K) = Inactive
--                                    and then Number_Of_Neighbors(S,I,J,K) = 3
--                              then Active
--                              else S(I,J,K)
--                             )
--                         )
--                     )
--                 );
   begin

      -- to minimize the number of if statements, we'll make a copy of S
      -- this rather unfortunately means we waste time copying
      -- had I known from the beginning that there would be only 6 iterations,
      -- then I would have created the initial space with the necessary
      -- number of dimensions, but I didn't know at the start that we would
      -- stop after only 6 iterations in both puzzles

      for I in S'Range(1) loop
         for J in S'Range(2) loop
            for K in S'Range(3) loop
               Copy(I,J,K) := S(I,J,K);
            end loop;
         end loop;
      end loop;

      -- now apply the rules

      for I in Result'Range(1) loop
         for J in Result'Range(2) loop
            for K in Result'Range(3) loop
               Result(I,J,K) := Result_Of_Testing(Copy,I,J,K);
            end loop;
         end loop;
      end loop;

      -- if we're not done, do it again

      if Iterations > 1 then
         return Iterate(Result, Iterations - 1);
      else
         return Result;
      end if;

   end Iterate;

   function Iterate_4D(S: Hyperspace; Iterations: Positive) return Hyperspace is
   -- uses recursion to iterate S the number of times specified
      Copy, Result: Hyperspace(
                               S'First(1) - 1 .. S'Last(1) + 1,
                               S'First(2) - 1 .. S'Last(2) + 1,
                               S'First(3) - 1 .. S'Last(3) + 1,
                               S'First(4) - 1 .. S'Last(4) + 1
                              )
            := ( others => ( others => ( others => ( others => Inactive ) ) ) );
   begin

      -- to minimize the number of if statements, we'll make a copy of S

      for I in S'Range(1) loop
         for J in S'Range(2) loop
            for K in S'Range(3) loop
               for L in S'Range(4) loop
                  Copy(I,J,K,L) := S(I,J,K,L);
               end loop;
            end loop;
         end loop;
      end loop;

      -- now apply the rules

      for I in Result'Range(1) loop
         for J in Result'Range(2) loop
            for K in Result'Range(3) loop
               for L in Result'Range(4) loop
                  Result(I,J,K,L) := Result_Of_Testing_4d(Copy,I,J,K,L);
               end loop;
            end loop;
         end loop;
      end loop;

      -- if we're not done, do it again

      if Iterations > 1 then
         return Iterate_4D(Result, Iterations - 1);
      else
         return Result;
      end if;
   end Iterate_4D;

   Initial_Number_Of_Rows, Initial_Number_Of_Cols: Natural := 0;

begin

   -- first determine dimensions of initial state

   Open(F, In_File, "/Users/user/common/Ada/AoC2020/Day17/input.txt");

   while not End_Of_File(F) loop
      declare S: String := Get_Line(F);
      begin
         Initial_Number_Of_Cols := S'Length;
         Initial_Number_Of_Rows := Initial_Number_Of_Rows + 1;
      end;
   end loop;

   Close(F);

   -- now we perform the exercises

   declare

      -- get the input

      Initial: Space
            := Read_Initial_State(Initial_Number_Of_Rows, Initial_Number_Of_Cols);

   begin

      Put(Initial);

      -- part 1: count the number of active cubes after 6 iterations in 3-space

      declare

         Number_Active: Natural := 0;

         Final_State: Space := Iterate(Initial, 6);

      begin

         for I in Final_State'Range(1) loop
            for J in Final_State'Range(2) loop
               for K in Final_State'Range(3) loop
                  if Final_State(I,J,K) = Active then
                     Number_Active := Number_Active + 1;
                  end if;
               end loop;
            end loop;
         end loop;

         Put(Number_Active, 0); Put(" cubes active"); New_Line(1);

      end;

      -- part 2: count the number of active cubes after 6 iterations in 4-space

      declare

         Number_Active: Natural := 0;

         Final_State: Hyperspace
               := Iterate_4D( Embed_In_Hyperspace(Initial), 6 );

      begin

         for I in Final_State'Range(1) loop
            for J in Final_State'Range(2) loop
               for K in Final_State'Range(3) loop
                  for L in Final_State'Range(4) loop
                     if Final_State(I,J,K,L) = Active then
                        Number_Active := Number_Active + 1;
                     end if;
                  end loop;
               end loop;
            end loop;
         end loop;

         Put(Number_Active, 0); Put(" cubes active"); New_Line(1);

      end;

   end;

end Main;
