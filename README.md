# Diceware

Diceware generates passphrases using a method that conforms to the algorithm
stated here: http://world.std.com/~reinhold/diceware.html

This program uses Go's cryptographically secure psuedorandom number generator to
generate the "dice rolls" for the algorithm.

### Installation

Run `go get github.com/natefinch/diceware`

### Usage

  -b=false: use alternate word list from Alan Beale
  -e=false: generate an extra random character
  -f="": read word list from file (must be tsv)
  -n=6: number of words to generate
