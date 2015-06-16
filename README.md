# Diceware

Diceware generates passphrases using a method that conforms to the algorithm
stated here: http://world.std.com/~reinhold/diceware.html

This program uses Go's cryptographically secure psuedorandom number generator to
generate the "die rolls" for the algorithm.

### Installation

Run `go get github.com/natefinch/diceware`

### Usage

	usage of diceware:
	  -8=false: use diceware 8k word list
	  -b=false: use alternate word list from Alan Beale
	  -e=false: generate an extra random character
	  -f="": read word list from newline delimited file
	  -t="": read word list from tsv file ('12345	word' style)
	  -w=6: number of words to generate
