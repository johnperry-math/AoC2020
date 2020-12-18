-- Advent of Code 2020, Day 18
--
-- John Perry
--
-- apologies for any bad Ada style
--
-- Operation Order
--
-- part 1: evaluate mathematical expressions where addition and multiplication
-- have no precedence over the other, but parentheses still do
--
-- part 2: evaluate mathematical expressions where the precedence of addition
-- and multiplicaiton is reversed, but parentheses remain higher

with Ada.Text_IO;
use Ada.Text_IO;

with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;

procedure Main is

   F: File_Type; -- input file

   Unexpected_Character: exception; -- very useful in debugging

   -- parsing proceeds as follows:
   -- (sort of EBNF: + means repeat 1 or more times, * means repeat 0 or more)
   --
   -- Evaluator:
   --    -> Value -> +( ( ( '+' Value | '*' Factor ) ) ->
   --
   -- Value: -> Number | '(' Evaluator ')'
   --
   -- the meaning of Factor depends on the precedence:
   --    * for equal precedence, Factor = Value
   --    * when addition has higher precedence, then Factor = Evaluator
   --
   -- Number: -> ( '0' | ... | '9' )+ ->

   function Next_Number(S: String; Last: in out Positive) return Long_Integer is
   -- S(Last) is a number; read and return it, updating Last to new position

      Result: Integer;

   begin

      Get(S(Last .. S'Last), Result, Last);
      Last := Last + 1;
      return Long_Integer(Result);

   end Next_Number;

   procedure Skip_White_Space(S: String; Last: in out Positive) is
   -- S(Last) is a space; read until no longer a space, updating Last
   -- to new position

   begin

      while Last < S'Last and then S(Last) = ' ' loop
         Last := Last + 1;
      end loop;

   end Skip_White_Space;

   -- because we're considering two different precedences of operations,
   -- we'll use a dedicated function for each precendence, called Evaluator, and
   -- pass it as an argument to functions that need to evaluate sub-expressions
   --
   -- see Evaluate_Equal_Precedence and Evaluate_Opposite_Precedence
   type Evaluator is access function(S: String; Last: in out Positive)
   return Long_Integer;

   function Expression(S: String; Last: in out Positive; Eval: Evaluator)
   -- forward declaration; see below
   return Long_Integer;

   function Get_Value(S: String; Last: in out Positive; Eval: Evaluator)
   return Long_Integer
   -- returns the value of a number or a sub-expression, whichever is next
   -- starts at Last
   -- if it's a number, last will point to the first non-numerical character
   -- if it's a sub-expression, last will point to the closing parens
   -- raises an exception if unexpected input is encountered

   is
   (
      case S(Last) is
      when '(' => Expression(S, Last, Eval),
      when '0'..'9' => Next_Number(S, Last),
      when others => raise Unexpected_Character with S(Last..S'Last)
   );

   function Evaluate_Equal_Precedence(S: String; Last: in out Positive)
   return Long_Integer
   -- returns the value of the (sub-)expression in S, starting at Last,
   -- when addition and multiplication have equal precedence
   -- the value of Last will be after any white space
   -- at Get_Value's value of Last
   -- raises an exception if unexpected input is encountered

   is

      Result: Long_Integer := 0;

   begin

      Skip_White_Space(S, Last);
      Result := Get_Value(S, Last, Evaluate_Equal_Precedence'Access);
      Skip_White_Space(S, Last);

      while Last < S'Last loop

         if S(Last) = '+' then

            Last := Last + 1;
            Skip_White_Space(S, Last);
            Result
                  := Result
                        + Get_Value(S, Last, Evaluate_Equal_Precedence'Access);

         elsif S(Last) = '*' then

            Last := Last + 1;
            Skip_White_Space(S, Last);
            Result
                  := Result
                        * Get_Value(S, Last, Evaluate_Equal_Precedence'Access);

         elsif S(Last) = ')' then exit;

         else raise Unexpected_Character with S(Last..S'Last);

         end if;

         Skip_White_Space(S, Last);

      end loop;

      return Result;

   end Evaluate_Equal_Precedence;

   function Evaluate_Opposite_Precedence(S: String; Last: in out Positive)
   return Long_Integer
   -- returns the value of the (sub-)expression in S, starting at Last,
   -- when addition has higher precedence than multiplication
   -- the value of Last will be after any white space
   -- at Get_Value's value of Last
   -- raises an exception if unexpected input is encountered

   is

      Result: Long_Integer := 0;

   begin

      Skip_White_Space(S, Last);
      Result := Get_Value(S, Last, Evaluate_Opposite_Precedence'Access);
      Skip_White_Space(S, Last);

      while Last < S'Last loop

         if S(Last) = '+' then

            Last := Last + 1;
            Skip_White_Space(S, Last);
            Result
                  := Result
                     + Get_Value(S, Last, Evaluate_Opposite_Precedence'Access);

         elsif S(Last) = '*' then

            Last := Last + 1;
            Skip_White_Space(S, Last);
            Result
                  := Result
                     * Evaluate_Opposite_Precedence(S, Last);

         elsif S(Last) = ')' then exit;

         else raise Unexpected_Character with S(Last..S'Last);

         end if;

         Skip_White_Space(S, Last);

      end loop;

      return Result;

   end Evaluate_Opposite_Precedence;

   function Expression(S: String; Last: in out Positive; Eval: Evaluator)
   return Long_Integer
   -- evaluates a sub-expression, expecting to start at '(', then calls Eval
   -- on the sub-expression, expecting to end at ')'
   -- raises an exception if parentheses do not satisfy requirements

   is

      Result: Long_Integer := 0;

   begin

      if S(Last) /= '(' then
         raise Unexpected_Character with S(Last..S'Last);
      end if;

      Last := Last + 1;
      Result := Eval(S, Last);

      if S(Last) /= ')' then
         raise Unexpected_Character with S(Last..S'Last);
      end if;

      Last := Last + 1;
      return Result;

   end Expression;

begin

   Open(F, In_File, "/Users/user/common/Ada/AoC2020/Day18/input.txt");

   -- we'll do the input and the arithmetic for both parts in one go

   declare

      Sum_Of_Equals:   Long_Integer := 0;
      Sum_Of_Opposite: Long_Integer := 0;

   begin

      while not End_Of_File(F) loop

         declare

            S: String := Get_Line(F);
            Last: Positive := S'First;

         begin

            -- for debugging I included each line and its partial sums

            Put(S); Put(": ");

            Sum_Of_Equals
                  := Sum_Of_Equals + Evaluate_Equal_Precedence(S, Last);
            Put(Sum_Of_Equals'Image); Put(", ");

            Last := S'First;
            Sum_Of_Opposite
                  := Sum_Of_Opposite + Evaluate_Opposite_Precedence(S, Last);
            Put(Sum_Of_Opposite'Image);

            New_Line(1);

         end;

      end loop;

   end;

   Close(F);

end Main;
