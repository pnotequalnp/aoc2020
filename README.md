# Advent of Code 2020

My Haskell solutions for [AoC 2020](https://adventofcode.com/2020/).

## Environment

Requires two environment variables, `AOC_SESSION_KEY` and `AOC_YEAR`. The value for the former can
be found in a cookie on the [AoC website](https://adventofcode.com/) when logged in. `AOC_CACHE` is
an optional but suggested environment variable specifying where to cache info fetched via the AoC
API. If undefined, a temporary directory will be allocated. I recommend using the `dotenv`
function with [direnv](https://direnv.net/) to manage environment variables like these.

## Running

### Nix (Recommended)

```bash
nix run github:pnotequalnp/aoc2020 -- 5 2 # day 5, part 2
```

### Cabal

Requires `ghc 8.8.4`, `cabal-install 3.2`
```bash
gh repo clone pnotequalnp/aoc2020
cd aoc2020
cabal run aoc2020 -- 5 2 # day 5, part 2
```
