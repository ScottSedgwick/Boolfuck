# Haskell language interpreter exercise

This is a copy of the [Codewars](https://www.codewars.com) exercise, moved into a [Stack](https://www.haskellstack.org/) project.

Instructions are simple:

1. Clone the `master` branch
2. Run `stack test`
3. Add code to `src/Boolfuck.hs`
4. Repeat steps 2 & 3 until all the tests are green.

If you want to cheat, my submitted answer to Codewars is in the `solution` branch.  It is a bit untidy, and does not make use of some libraries that it should, because they are not in the Codewars environment.

The tidier version, using all the libraries, is in the `refined` branch.

Below is the instructions from Codewars:

## Esolang Interpreters #4 - Boolfuck Interpreter

### About this Kata Series

"Esolang Interpreters" is a Kata Series that originally began as three separate, independent esolang interpreter Kata authored by @donaldsebleung which all shared a similar format and were all somewhat inter-related. Under the influence of a fellow Codewarrior, these three high-level inter-related Kata gradually evolved into what is known today as the "Esolang Interpreters" series.

This series is a high-level Kata Series designed to challenge the minds of bright and daring programmers by implementing interpreters for various esoteric programming languages/Esolangs, mainly Brainfuck derivatives but not limited to them, given a certain specification for a certain Esolang. Perhaps the only exception to this rule is the very first Kata in this Series which is intended as an introduction/taster to the world of esoteric programming languages and writing interpreters for them.

### The Language

Boolfuck is an esoteric programming language (Esolang) based on the famous Brainfuck (also an Esolang) which was invented in 2004 or 2005 according to the official website. It is very similar to Brainfuck except for a few key differences:

- Boolfuck works with bits as opposed to bytes
- The tape for Brainfuck contains exactly 30,000 cells with the pointer starting from the very left; Boolfuck contains an infinitely long tape with the pointer starting at the "middle" (since the tape can be extended indefinitely either direction)
- Each cell in Boolfuck can only contain the values 0 or 1 (i.e. bits not bytes) as opposed to Brainfuck which has cells containing values ranging from 0 to 255 inclusive.
- The output command in Boolfuck is `;` NOT `.`
- The `-` command does not exist in Boolfuck since either `+` or `-` would flip a bit anyway

Anyway, here is a list of commands and their descriptions:

| Command | Description |
| ------- | ----------- |
| + | Flips the value of the bit under the pointer" |
| , | Reads a bit from the input stream, storing it under the pointer. The end-user types information using characters, though. Bytes are read in little-endian order—the first bit read from the character a, for instance, is 1, followed by 0, 0, 0, 0, 1, 1, and finally 0. If the end-of-file has been reached, outputs a zero to the bit under the pointer. |
| ; | Outputs the bit under the pointer to the output stream. The bits get output in little-endian order, the same order in which they would be input. If the total number of bits output is not a multiple of eight at the end of the program, the last character of output gets padded with zeros on the more significant end. |
| < | Moves the pointer left by 1 bit |
| > | Moves the pointer right by 1 bit |
| [ | If the value under the pointer is 0 then skip to the corresponding ] |
| ] | Jumps back to the matching [ character, if the value under the pointer is 1 |

If you haven't written an interpreter for Brainfuck yet I recommend you to complete this Kata first.

### The Task

Write a Boolfuck interpreter which accepts up to two arguments. The first (required) argument is the Boolfuck code in the form of a string. The second (optional) argument is the input passed in by the end-user (i.e. as actual characters not bits) which should default to "" if not provided. Your interpreter should return the output as actual characters (not bits!) as a string.

```(haskell)
boolfuck :: String -> String -> String
```

Preloaded for you is a function brainfuckToBoolfuck()/brainfuck_to_boolfuck()/BrainfuckToBoolfuck() which accepts 1 required argument (the Brainfuck code) and returns its Boolfuck equivalent should you find it useful.

Please note that your interpreter should simply ignore any non-command characters. This will be tested in the test cases.

If in doubt, feel free to refer to the official website (link at top).

Good luck :D

### Kata in this Series

[Esolang Interpreters #1 - Introduction to Esolangs and My First Interpreter (MiniStringFuck)](https://www.codewars.com/kata/esolang-interpreters-number-1-introduction-to-esolangs-and-my-first-interpreter-ministringfuck)

[Esolang Interpreters #2 - Custom Smallfuck Interpreter](http://codewars.com/kata/esolang-interpreters-number-2-custom-smallfuck-interpreter)

[Esolang Interpreters #3 - Custom Paintfuck Interpreter](http://codewars.com/kata/esolang-interpreters-number-3-custom-paintf-star-star-k-interpreter)

[Esolang Interpreters #4 - Boolfuck Interpreter](https://www.codewars.com/kata/esolang-interpreters-number-4-boolfuck-interpreter)

### Other Kata

I have added other Kata from this series as I complete them to this repo.  Currently only MiniStringFuck is complete.
