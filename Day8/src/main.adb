-- Advent of Code 2020, Day 8
--
-- John Perry
--
-- apologies for any bad Ada style
--
-- an infinite loop
--
-- first exercise: read the instructions to an assembly code, then determine
-- the value in the accumulator before an instruction runs a second time
--
-- second exercise: find the nop or jump instruction that has been swapped
-- to a jump or nop, respectively, and that by changing back leads to
-- termination, then determine the value in the accumulator upon termination
--

with Ada.Text_IO;
use Ada.Text_IO;

with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;

with Ada.Containers.Vectors;

procedure Main is

   F: File_Type;

   -- opcodes, instructions, and the program

   type Opcodes is ( Acc, Jmp, Nop ); -- only 3 opcodes in this assembly code

   type Instruction is record
      Opcode: Opcodes := Nop;
      Data: Integer := 0;
   end record;

   package Code_Vector is new Ada.Containers.Vectors
   (
    Element_Type => Instruction,
    Index_Type => Natural
   );

   Program: Code_Vector.Vector;

   function Parse(S: String) return Instruction is
   -- decipher the assembly code
   ( (
         Opcode =>
         (
          if S(S'First..S'First + 2) = "acc" then Acc
          elsif S(S'First..S'First + 2) = "jmp" then Jmp
          else Nop
         ),
         Data =>
         (
          Integer'Value(S(S'First + 4..S'Last))
         )
   ) );

   procedure Execute(Idx: in out Natural; Accumulator: in out Natural) is
   -- execute the assembly code
   begin
      case Program(Idx).Opcode is
      when Nop => Idx := Idx + 1;
      when Acc =>
         Accumulator := Accumulator + Program(Idx).Data;
         Idx := Idx + 1;
      when Jmp => Idx := Idx + Program(Idx).Data;
      end case;
   end Execute;

   -- inspecting the code
   --
   -- for each run, we will reset an accumulator and assume the error of an
   -- infinite loop
   --

   type Execution_Data is record
      Accumulator: Natural := 0;
      Success: Boolean := False;
   end record;

   Result_Of_Execution: Execution_Data;

   procedure Inspect is
   -- execute the current program and store diagnostic in Result_of_Execution

      Accumulator: Natural renames Result_Of_Execution.Accumulator;
      Program_Index: Natural := 0;

      type Line_Record is array ( 0 .. Program.Length ) of Boolean;
      pragma Pack(Line_Record);
      Executed_Line: Line_Record := ( others => False );

   begin

      -- reset diagnostic
      Accumulator := 0;
      Program_Index := 0;

      -- watch for infinite loop by storing Program_Index in Executed_Lines

      while not (
            Program_Index >= Natural(Program.Length) or
            Executed_Line(Ada.Containers.Count_Type(Program_Index))
      ) loop

         Executed_Line(Ada.Containers.Count_Type(Program_Index)) := True;
         Execute(Program_Index, Accumulator);

      end loop;

      if Program_Index >= Natural(Program.Length) then
         Result_Of_Execution.Success := True;
      end if;

   end Inspect;

begin

   -- read and parse the program

   Open(F, In_File, "/Users/user/common/Ada/AoC2020/Day8/input.txt");

   while not End_Of_File(F) loop
      Program.Append(Parse(Get_Line(F)));
   end loop;

   Close(F);

   Put("read "); Put(Program.Length'Image); Put(" commands"); New_Line(1);

   -- exercise 1: find value in accumulator before we hit the infinite loop

   Inspect;

   Put("the accumulator contains "); Put(Result_Of_Execution.Accumulator, 0);
   Put(" immediately before the infinite loop");
   New_Line(2);

   -- exercise 2: find value in accumulator after we find a successful
   -- modification of the program
   --
   -- i tried to figure out a more elegant solution,
   -- but a brute force approach turned out quickest in this case

   Brute_Force:
   for I in 0 .. Natural(Program.Length) loop

      if Program(I).Opcode = Jmp then

         Put("modifying line "); Put(I, 0); Put(" from jmp to nop");
         New_Line(1);

         Program(I).Opcode := Nop;
         Inspect;
         if Result_Of_Execution.Success then exit; end if;
         Program(I).Opcode := Jmp;

      elsif Program(I).Opcode = Nop then

         Put("modifying line "); Put(I, 0); Put(" from nop to jmp");
         New_Line(1);

         Program(I).Opcode := Jmp;
         Inspect;
         if Result_Of_Execution.Success then exit; end if;
         Program(I).Opcode := Nop;

      end if;

   end loop Brute_Force;

   Put("after a successful modification, the accumulator contains ");
   Put(Result_Of_Execution.Accumulator, 0);
   New_Line(1);

end Main;
