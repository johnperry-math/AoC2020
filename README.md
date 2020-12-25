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

Another variant of the game of life, completed.
One new "feature" of "Ada" I discovered today is that
gnat crashed when I tried to initialize 3- and 4-dimensional arrays
using complicated aggregates that Ada 202x SHOULD allow.
So I guess I will report a bug. (Done!)
I also made respectable use of negative array indices.
(Try doing *that* in C/C++.)


## Day 18: Operation Order

Evaluate arithmetic expressions in unusual order, completed.
Nothing really new; I did use functions as parameters.
This is essentially a very simple exercise from classes in
Compiler Construction.

## Day 19: Monster Messages

Verify a message against rules of a grammer, most of which are non-terminal.
In part 2, two of the rules form a loop.

Completed -- eventually; my first approach was incorrect,
and I couldn't shake my brain of it.
I took a glance at Gautier de Montmollin's
[solution](https://github.com/zertovitch/hac/blob/master/exm/aoc/2020/aoc_2020_19_full_ada.adb)
(uses HAC Pack, which I don't have), and that helped shake my brain a bit.
Eventually I worked out a solution that I *think* is different from his.

I did a bit of work with types here, relying on subtypes to define ranges
for rules and options. I don't think I did enough with types; there should
have been an Option type. One day I may rewrite this to take that into account.

## Day 20: Jurassic Jigsaw

Find monsters in monochrome images that have been cut into squares, then
thrown up in the air & allowed to fall to the ground. Yup: rotations and
reflections are in play!

Completed, using a LOT of types. It may actually have been overkill.
The complexity of the code reflects some of the trouble I had with the
assignment. I've actually simplified it a bit already, but I've also noted
where I think it should be simplified further.

Made use of constants, rename expressions, a "for" expression to exit a loop,
references and renames, custom ranges, "for" expressions inside "if"
expressions, and some other stuff I can't recollect at the moment.
If I do, I'll update.

Day 21: Allergen Assessment

Identify which ingredients correspond to allergens, and which do not, completed.
First part asks us to identify the ingredients that do not correspond
to allergens; second part asks us to identify the correct correspondence and
report them in order of allergen. Since the initial data does not narrow all
the ingredients down, we have to do a little work to sort that out.
(Finally! Back to a relatively easy puzzle.)

Here I relied on ordered maps instead of hash maps. I also instantiated
a generic sorter. "for all" expressions came in useful again.

Day 22: Crab Combat

Play a game of Combat (essentially the card game "War") with a crab who has
hitched a ride on your raft. Then modify the rules to allow recursive games,
which are sort of cool.

I tried to implement this using Ada's Queues, but that didn't work out so well;
I kept getting memory errors. Not easily finding any good examples online, or
even any, really, I implemented a queue using an array and a modular type.
I then used sets to track previous configurations in part 2.
Some minor bugs meant I didn't quite get part 2 working for quite a while;
in fact I grew confused with some things Ada told me about equivalent sets,
and even though I was close to the right answer for a while, I dind't realize
the problem was a bug in the code that implemented the rules: through a
copy-and-paste error, I added a card to the wrong player's deck.

I did discover, rather to my disappointment, that the statements

```ada
type Mod52 is mod 52;
A: Mod52 := 50;
B: Mod52 := 2;
for I in A .. B
   etc.
```

...does not cycle around the way I thought it should.

I would have liked to use a tagged type for the Decks, to work a little with
the object-oriented programming in Ada, but I decided against that for now.
