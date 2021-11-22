# advent-of-code-2019
Compute the solutions to Advent of Code 2019, written in Haskell.

## Build
This project uses the Haskell build tool [stack](https://docs.haskellstack.org/en/stable/README/) for building, testing and running this application.
Once you have stack installed, you can run and build the project like so:

```console
$ stack run advent-of-code -- --help
advent-of-code
usage : advent-of-code day input [-h] [--version]
Compute solutions to Advent of
Code 2019

mandatory arguments:
 day                           The day for to compute solutions
                               for
 input                         The path to the data file used by
                               the solutions

optional arguments:
 -h, --help                    show this help message and exit
 --version                     print the program version and exit
```

You can pass in the specific day and data file (all days stored in the `inputs` directory) to the program and it will compute that day's solution.