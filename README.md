# AoC2020

Advent of Code 2020. Gives me an excuse to work with Ada.

## Day 1: Report Repair

Expense report, completed.

## Day 2: Password Philosophy

Password validation, completed.

## Day 3: Toboggan Trajectory

Trees a downhilling toboggan might encounter, completed.

## Day 4: Passport Processing

Checking validity of passports, completed. Kind of early to have an exercise drive one mad.
Reacquainted myself with Ada's enumerations.

## Day 5: Binary Boarding

Lost your boarding bass; try to find your seat, completed.

## Day 6: Custom Customs

Help people fill out customs forms, completed.
Acquainted myself with `Strings.Maps` and `Containers.Hash_Sets`.

## Day 7: Handy Haversacks

Strange regulations about bags that can contain other bags, completed.
Acquainted myself with `Containers.Maps` and **`for all`** expressions.
Also learned about initializing records with default values (use `=> <>`).
On Jeffrey Carter's advice I changed a constant, fixed-length string
from `"                    "` to `others => ' '`.

## Day 8: Handheld Halting

Debugging a child's handheld, stuck on an infinite loop, completed.
This time I `Pack`'d an `array` of `Boolean`
and used `renames` to create a reference.

## Day 9: Encoding Error

Find the weakness in an encryption scheme, completed.
The numbers grow larger than `Natural`, so I used `Long_Long_Integer`.
(Probably could have gotten away with `Long_Integer`.)
After solving it, I replaced a `Boolean` and three `if`'s in the first exercise
with labels and `goto`s. I'm not convinced that either is easier to follow.

## Day 10: Adapter Array

Find the number of ways to arrange some adapters, completed.
I think this is the first time we can actually apply a non-brute-force solution.
My approach is not complete, however, so I defined an `exception` which,
amazingly enough, was not needed.

## Day 11: Seating System

Essentially a variant on the game of Life, completed.
I defined a `function` type, made use of labeled arguments,
and eschewed the use of `Vector`s in favor of two-dimensional `array`s,
which means I encountered Ada's `'Length(N)` attribute.
Later updated to use `task`s, with improvements in timing!

## Day 12: Rain Risk

Cool, we get to work with the sidewalk metric!
Essentially we just follow some directions to figure out where we're going,
with different answers depending on how one understands the directions.
The first is egocentric (directions are relative to me!);
the second is relative to a waypoint.
I made heavy use of enumerations, especially in the first part,
where I used an two-dimensional array indexed by an enumeration
to avoid if statements.
There were also lots of `case` statements.
In fact, there's not a single `if` statement in the whole program,
which is probably not impressive, but it does amuse me.

(It's completed.)


## Day 13: Shuttle Search

Find various intersections of shuttles whose circulation time
is a prime number of minutes, completed.
I used variant `record`s (i.e., a discriminant) to distinguish between shuttles
that are in service and are not.
Unfortunately, while the answer fits in `Long_Long_Integer`,
some intermediate expressions do not,
so I had ("got"?) to use Ada2020's `Ada.Numerics.Big_Numbers` package.

## Day 14: Docking Data

Determine the values or memory to write to,
according to a given bit mask, completed.
Time to use the `mod` types! so that we can `and` and `or`;
Ada only allows masking with `mod` types; see `Turn_On` and `Turn_Off`.
Finally found a place to use `renames` where I didn't feel at least a little
guilty.
Double-buffered an array using a `mod 2` type, following a recommendation
on an earlier assignment.
gnat also surprised me with a (correct) warning that a `String` does not always
starts at index 1.

## Day 15: Rambunctious Recitation

Determine the nth number of a relatively simple sequence, completed.
I didn't come up with anything interesting here.
Kind of disappointed in this puzzle, truth be told.

## Day 16: Ticket Translation

Given specifications for train tickets,
first determine the invalid tickets.
Then figure out which specification applies to which position.
This was my first time using `Ada.Strings.Fixed` and `Find_Token`,
which may have been overkill in this case.
It was also my first time using the `&` operator for string concatenation.
I found that I could have used bitwise operations on an `array of Boolean`
but settled on a little more straightforward.
I also used `renames` a bit more, and increasingly respectably.
Overall though the program strikes me as a bit of a mess.

I guess yesterday's breezer of a puzzle was to give us a rest
before this one, completed.

## Day 17: Conway Cubes

Another variant of the game of life.
The only new "feature" of "Ada" I discovered today is that
gnat crashed when I tried to initialize 3- and 4-dimensional
using complicated aggregates that Ada 202x SHOULD allow.
So I guess I will report a bug.
