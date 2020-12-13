-- Advent of Code 2020, Day 13
--
-- John Perry
--
-- apologies for any bad Ada style
--
-- Shuttle Search
--
-- it seemed more appropriate to put description of problems below
-- than to put them here

pragma Ada_2020;

with Ada.Text_IO;
use Ada.Text_IO;

with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;

with Ada.Containers.Vectors;

-- unfortunately, this requires big integers for intermediate steps

with Ada.Numerics.Big_Numbers.Big_Integers;
use Ada.Numerics.Big_Numbers.Big_Integers;

procedure Main is

   -- I/O

   F: File_Type;

   -- information on a Shuttle; turns out I didn't need all this
   -- but after reading Part 1 I wasn't sure that Part 2 wouldn't want
   -- something changed with the out-of-service shuttles

   type Shuttle(In_Service: Boolean := False) is record
      case In_Service is
         when True  => Value: Positive;
         when False => null;
      end case;
   end record;

   subtype Shuttle_Not_In_Service is Shuttle(In_Service => False);
   subtype Shuttle_In_Service is Shuttle(In_Service => True);

   package Shuttle_Vectors is new Ada.Containers.Vectors
   (
    Element_Type => Shuttle,
    Index_Type => Positive
   );

   Shuttles: Shuttle_Vectors.Vector;

   -- to solve the second part I used the Chinese Remainder Theorem
   -- that requires us to compute the inverse of a modulo b
   -- this is possible so long as gcd(a,b)=1
   -- in that case the Extended Euclidean Algorithm will give us the inverse

   function Inverse_Modulo(A, B: Big_Integer) return Big_Integer
   -- determines the inverse of a modulo b; that is,
   -- determines the value x such that a * x mod b = 1
   is

      S_Prev, T_Prev,
      S_Curr, T_Curr,
      S_Next, T_Next,
      M, N, Q, R: Big_Integer;

   begin

      -- this algorithm can be found in standard number theory textbooks;
      -- e.g., Rosen's Elementary Number Theory

      S_Prev := 1; T_Prev := 0; S_Curr := 0; T_Curr := 1;

      M := Max(A, B);
      N := Min(A, B);

      loop

         Q := M / N;
         R := M mod N;

         S_Next := S_Prev - Q * S_Curr;
         T_Next := T_Prev - Q * T_Curr;

         S_Prev := S_Curr; S_Curr := S_Next;
         T_Prev := T_Curr; T_Curr := T_Next;

         M := N;
         N := R;

         exit when R = 0;

      end loop;

      return ( if A > B then S_Prev else T_Prev );

   end Inverse_Modulo;

   function Crt(A1, A2, M1, M2: Big_Integer) return Big_Integer is
   -- Chinese Remainder Theorem:
   -- computes x so that x mod m_i = a_i, so long as gcd( m_1, m_2 ) = 1
   --
   -- alas, the From_- and To_Bigum's make it a little hard to read,
   -- but the basic idea is this:
   --
   --   - choose q_1 such that q_1 * m_1 + a_1 = x
   --
   --   - then q_1 * m_1 + a_1 mod m_2 = a_2
   --
   --   - let n_1 be the multiplicative inverse of m_1 modulo m_2
   --     (we know it exists because they are relatively prime)
   --
   --   - then q_1 + a_1 * n_1 mod m_2 = a_1 * n_1,
   --     or, q_1 mod m_2 = (a_2 - a_1) * n_1
   --
   --   - now back-substitute: x = [ (a_2 - a_1) * n_1 ] * m_1 + a_1
   (
      (
        ( M1 * ( ( A2 - A1 ) * Inverse_Modulo( M1, M2 ) ) + A1 ) mod ( M1 * M2 )
      )
   );

   Arrival_Time: Positive; -- your arrival time (we need this in both parts)


begin

   -- read the input

   Open(F, In_File, "/Users/user/common/Ada/AoC2020/Day13/input.txt");

   declare

      -- only two lines of input!

      S1: String := Get_Line(F);
      S2: String := Get_Line(F);

      -- used to read the second line: "Last" is last position read;
      -- time is the time read in, and arrival_time is your personal arrival

      Last: Positive;
      Time: Positive;

   begin

      -- read your arrival time

      Get(S1, Arrival_Time, Last);

      -- now read shuttle times

      Last := S2'First;

      while Last < S2'Length loop

         if S2(Last) = 'x' then
            Shuttles.Append( ( In_Service => False ) );
         else
            Get(S2(Last .. S2'Length), Time, Last);
            Shuttles.Append( ( In_Service => True, Value => Time ) );
         end if;

         Last := Last + 2;

      end loop;

   end;

   Close(F);

   -- part 1: determine the ID of the bus that arrives
   -- closest to your arrival time and report the product of its ID
   -- with the time you'll have to wait for it

   declare

      Min_Time: Natural := Natural'Last;
      Best_Bus: Positive := Positive'Last;

      Q, R: Natural; -- quotient and remainder, but of different operations

   begin

      for S of Shuttles loop

         if S.In_Service then

            -- how many times will s cycle before you arrive
            Q := Arrival_Time / S.Value;
            -- how much time will be left after s's next cycle?
            R := S.Value * ( Q + 1 ) - Arrival_Time;
            -- I didn't actually need this next line but it's probably best
            if R = S.Value then R := 0; end if;

            if R < Min_Time then
               Min_Time := R;
               Best_Bus := S.Value;
            end if;

         end if;

      end loop;

      Put("best bus "); Put(Best_Bus, 0); New_Line(1);
      Put("you will wait "); Put(Min_Time, 0); New_Line(1);
      Put("the product is "); Put(Best_Bus * Min_Time, 0); New_Line(1);

   end;

   -- part 2: find the earliest timestamp such that
   --    - first bus ID departs precisely then
   --    - second bus ID, which is n_1 buses after the first bus ID in the list,
   --      departs n_1 minutes after the first bus ID
   --    - third bus ID, which is n_2 buses after the first bus ID in the list,
   --      departs n_2 minutes after the first bus ID
   -- etc.
   --
   -- we could reformulate this as finding the timestamp t such that
   -- the bus listed in position i leaves i minutes after t, then adding 1
   --
   -- this reformulation shows that we have a Chinese Remainder Theorem problem:
   -- if we let the bus at position p_i have id b_i then we want the solution to
   --    { x = -p_1 ( mod b_1 )
   --    { x = -p_2 ( mod b_2 )
   --    { ...
   -- where "=" actually means "congruent to"
   --
   -- for example, my input has the values 13,x,x,41,x,x,...
   -- so I want the solution to
   --    { x = -1 ( mod 13 )
   --    { x = -4 ( mod 41 )
   --    { ...
   --
   -- to make iteration "easy", I will reformulate it as
   --    { x =  0 ( mod  1 )
   --    { x = -1 ( mod 13 )
   --    { x = -4 ( mod 41 )
   --    { ...
   -- since that doesn't change the solution
   --
   -- iteration is possible since the numbers are apparently all prime
   -- (and thus relatively prime) and we once we find the solution to one pair,
   -- say
   --    { x = a_1 ( mod p_1 ) , x = a_2 ( mod p_2 ) },
   -- we know that x is unique modulo p_1 * p_2 , so we can extend to
   --    { y = x ( mod p_1 * p_2 ) , y = a_3 ( mod p_3 ) }
   -- and so forth

   declare

      Pos: Big_Integer := 1;
      Common_Time: Big_Integer := 0;
      Common_Modulus: Big_Integer := 1;

   begin

      for S of Shuttles loop

         if S.In_Service then

            Common_Time := Crt(
                  Common_Time, -Pos ,
                  Common_Modulus, To_Big_Integer( S.Value )
            );
            Common_Modulus := Common_Modulus * To_Big_Integer( S.Value );

         end if;

         Pos := Pos + 1;

         Put("crt sol is now "); Put( Common_Time'Image); New_Line(1);

      end loop;

      Common_Time := Common_Time + 1;
      Put("we want the time "); Put( Common_Time'Image ); New_Line(1);

   end;

end Main;
