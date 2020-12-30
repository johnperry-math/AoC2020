-- Advent of Code 2020, Day 25
--
-- John Perry
--
-- apologies for any bad Ada style
--
-- Combo Breaker
--
-- part 1: hack an encryption (not hard at all)
--
-- the encryption is as follows:
--
--    + the door has a public key; the card has a public key
--    + the door has a private key ("loop size"); the card has a private key
--    + each transforms the subject number of 7 according to its private key,
--      essentially by exponentiation: 7^x = public key where x is private key
--    + the encryption key is the personal private key raised to the public key
--
-- this is pretty easily solved with Ada's modular type and exponentiation
-- operator!
--
-- somewhere someone said this was the discrete logarithm scheme,
-- and that sounds right to me, but I don't honestly recall now
--
-- part 2: there was no part 2!
--

with Ada.Text_IO;
use Ada.Text_IO;

with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;

procedure Main is

   type Field_Element is mod 20201227; -- the modulus; given
                                       -- kind of cool to use a prime number
                                       -- that starts with 2020!

   Subject_Number: constant Field_Element := 7; -- the base; given

   -- data from the example, used to check that I wasn't misreading
   Example_Door_Key: constant Field_Element := 17807724;
   Example_Card_Key: constant Field_Element := 5764801;

   -- data from my input
   Door_Key: constant Field_Element := 11349501;
   Card_Key: constant Field_Element := 5107328;

   function Loop_Size(Key: Field_Element) return Positive is
   -- determine the private key / loop size

      Result: Positive := 1;
      Product: Field_Element := Subject_Number;

   begin

      while Product /= Key loop
         Result := Result + 1;
         Product := Product * Subject_Number;
      end loop;

      return Result;

   end Loop_Size;

begin

   Put("loop size for example door key: "); Put(Loop_Size(Example_Door_Key), 0);
   New_Line;
   Put("loop size for example card key: "); Put(Loop_Size(Example_Door_Key), 0);
   New_Line(2);

   Put("loop size for my door key: "); Put(Loop_Size(Door_Key), 0); New_Line;
   Put("loop size for my card key: "); Put(Loop_Size(Card_Key), 0); New_Line(2);

   declare

      Example_Door_Loop: Positive := Loop_Size(Example_Door_Key);
      Example_Encryption_Key: Field_Element
            := Example_Card_Key ** Example_Door_Loop;

      Door_Loop: Positive := Loop_Size(Door_Key);
      Encryption_Key: Field_Element := Card_Key ** Door_Loop;

   begin

      Put("example encryption key: ");
      Put(Example_Encryption_Key'Image); New_Line(2);
      Put("encryption key: ");
      Put(Encryption_Key'Image); New_Line;

   end;

   Put_Line("I completed Advent of Code 2020 entirely in Ada!");

end Main;
