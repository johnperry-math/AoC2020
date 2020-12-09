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
from "                    " to others => ' '.

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
