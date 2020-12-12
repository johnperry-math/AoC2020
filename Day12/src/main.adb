-- Advent of Code 2020, Day 12
--
-- John Perry
--
-- apologies for any bad Ada style
--
-- rain risk
--
-- hey, the sidewalk metric, but on a boat? bizarre
--
-- exercise 1: misunderstand what n, s, e, w, l, r mean,
-- and compute the sidewalk distance from origin to final destination
-- when you think they indicate motion relative to you, ya egotistical prat
--
-- exercise 2: notice the correct meaning on the back of the instructions,
-- and compute the sidewalk distance from origin to final destination
-- when in fact they indicate motion to a waypoint
--
-- I sort of went nuts with enumerations and arrays here, but it's nice
-- because I avoid lots of if / then statements that way

with Ada.Text_IO;
use Ada.Text_IO;

with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;

with Ada.Containers.Vectors;

procedure Main is

   -- I/O

   Input_File: File_Type;

   -- packages, types, and variables associated with the instructions

   -- the only options are north, south, east, west, left, right, forward

   type Instruction_Kind is ( N, S, E, W, L, R, F );

   -- learned from Maxim Reznik; read enumerations using Enumeration_IO

   package Instruction_IO is new Ada.Text_IO.Enumeration_IO (Instruction_Kind);
   use Instruction_IO;

   -- to follow a list of instructions we need to remember its kind and value

   type Instruction_Type is record
      Dir: Instruction_Kind;
      Val: Natural;
   end record;

   package Instruction_Vectors is new Ada.Containers.Vectors
   (
    Element_Type => Instruction_Type,
    Index_Type => Positive
   );

   Instructions: Instruction_Vectors.Vector;

   -- let's avoid if statements by exploiting Ada's highly legible enumerations
   -- and arrays

   type Direction_Value is ( North, South, East, West );

   -- left and right turns are always multiples of 90 degrees

   type Direction_Delta is ( Ninety, One_Eighty, Two_Seventy );

   -- let's take care of left turns (relative to us) using an arry

   Left_Turn: array( Direction_Value, Direction_Delta ) of Direction_Value :=
   (
          ( West,  South, East  ),   -- 90, 180, 270 from north
          ( East,  North, West  ),   -- ...from south
          ( North, West,  South ),   -- ...from east
          ( South, East,  North )    -- ...from west
   );

   -- let's take care of right turns (relative to us) using an array

   Right_Turn: array( Direction_Value, Direction_Delta ) of Direction_Value :=
   (
          ( East,  South, West  ), -- 90, 180, 270 from north
          ( West,  North, East  ), -- ...from south
          ( South, West,  North ), -- ...from east
          ( North, East,  South )  -- ...from west
   );

   -- I don't need this now, but I wish I'd had it when I first tried part 1
   Value_Error: exception;

   procedure Perform_Instruction(
         Instruction: Instruction_Type;
         X, Y: in out Integer;
         Direction: in out Direction_Value
   )
   -- this version interprets the instructions relative to us:
   -- north means to move north, right means to turn ourselves right, etc.
   is
      Val: Natural := Instruction.Val;
      D: Direction_Delta;
   begin
      case Instruction.Dir is
         when F => -- move forward in current heading
            case Direction is
               when North => Y := Y + Val;
               when South => Y := Y - Val;
               when East  => X := X + Val;
               when West  => X := X - Val;
            end case;
         when R => -- turn right
            -- as an aside, I first used an if-else statement here,
            -- but I didn't realize at first that were 270-degree turns,
            -- as they are mentioned neither in the directions nor the example,
            -- nor do they show up in my input until pretty late, actually.
            -- using the exception would have made me aware of this.
            D := (
                  case Val is
                     when 90     => Ninety,
                     when 180    => One_Eighty,
                     when 270    => Two_Seventy,
                     when others =>
                        raise Value_Error
                              with "I can only turn 90, 180, or 270 degrees"
                 );
            Direction := Right_Turn(Direction, D);
         when L => -- turn left
            D := (
                  case Val is
                     when 90     => Ninety,
                     when 180    => One_Eighty,
                     when 270    => Two_Seventy,
                     when others =>
                        raise Value_Error
                              with "I can only turn 90, 180, or 270 degrees"
                 );
            Direction := Left_Turn(Direction, D);
         when N => Y := Y + Val; -- move north
         when S => Y := Y - Val; -- move south
         when E => X := X + Val; -- move east
         when W => X := X - Val; -- move west
      end case;
   end Perform_Instruction;

   procedure Follow_Waypoint(
         Instruction: Instruction_Type;
         X, Y: in out Integer;
         Wx, Wy: in out Integer
   )
   -- this version interprets the instructions relative to a waypoint:
   -- north means to move the waypoint north,
   -- right means to rotate the waypoint around us, etc.
   is
      Val: Natural := Instruction.Val;
      Tmp: Integer;
   begin
      case Instruction.Dir is
         when F => -- move towards waypoint
            X := X + Wx * Val;
            Y := Y + Wy * Val;
         when R => -- rotate waypoint right
            Tmp := Wx;
            case Val is
               when 90  => Wx :=  Wy; Wy := -Tmp;
               when 180 => Wx := -Wx; Wy := - Wy;
               when 270 => Wx := -Wy; Wy :=  Tmp;
               when others =>
                  raise Value_Error with "I only know how to turn 90, 180, 270";
            end case;
         when L => -- rotate waypoint left
            Tmp := Wx;
            case Val is
               when 90  => Wx := -Wy; Wy :=  Tmp;
               when 180 => Wx := -Wx; Wy := - Wy;
               when 270 => Wx :=  Wy; Wy := -Tmp;
               when others =>
                  raise Value_Error with "I only know how to turn 90, 180, 270";
            end case;
         when N => Wy := Wy + Val; -- move waypoint north
         when S => Wy := Wy - Val; -- move waypoint south
         when E => Wx := Wx + Val; -- move waypoint east
         when W => Wx := Wx - Val; -- move waypoint west
      end case;
   end Follow_Waypoint;


begin

   -- read the instructions

   Open(Input_File, In_File, "/Users/user/common/Ada/AoC2020/Day12/input.txt");

   while not End_Of_File(Input_File) loop
      declare
         S: String := Get_Line(Input_File);
         Last: Positive;
         Instruction: Instruction_Type;
      begin
         Get(S(1..1), Instruction.Dir, Last);
         Get(S(2..S'Length), Instruction.Val, Last);
         Instructions.Append(Instruction);
      end;
   end loop;

   Close(Input_File);

   -- part 1: follow the instructions, thinking that they tell us what to do
   -- relative to ourselves

   declare
      X, Y: Integer := 0; -- we start at origin
      Direction: Direction_Value := East; -- initial direction is east
   begin
      for Instruction of Instructions loop
         Perform_Instruction(Instruction, X, Y, Direction);
      end loop;
      Put("we are now "); Put(abs(X) + abs(Y), 0);
      Put(" units away from origin");
      New_Line(1);
   end;

   -- part 2: follow the instructions, realizing that they tell us what to do
   -- relative to a way point

   declare
      X, Y: Integer :=  0; -- we start at origin
      Wx: Integer   := 10; -- waypoint starts 10 units east
      Wy: Integer   :=  1; -- waypoint starts 1 unit north
   begin
      for Instruction of Instructions loop
         Follow_Waypoint(Instruction, X, Y, Wx, Wy);
      end loop;
      Put("we are now "); Put(abs(X) + abs(Y), 0);
      Put(" units away from origin");
      New_Line(1);
   end;

end Main;
