# Diceware

Diceware generates passphrases using a method that conforms to the algorithm
stated here: http://world.std.com/~reinhold/diceware.html

This program uses Go's cryptographically secure psuedorandom number generator to
generate the "die rolls" for the algorithm.

## What is a passphrase?

![correcthorse](https://cloud.githubusercontent.com/assets/3185864/8198197/06534c20-1471-11e5-981d-fa407b0c4b7d.png)

A passphrase is a password that is made up of a handful of words. This makes the
password easy to remember and easy to type, even on a phone. Note that the
passphrase needs to be longer than a traditional totally random password,
because the passphrase is made of words not just random garbage.

### Installation

Run `go get github.com/natefinch/diceware`

### Usage

```
Usage:
  diceware [options]

Options:
  -w <num>	number of words to generate (default 6)
  -e		generate an extra random character

By default, diceware uses the diceware wordlist shown here:
http://world.std.com/~reinhold/diceware.wordlist.asc. You may change this by
specifying at most one of the following flags:

  -8		use diceware 8k word list
  -b		use alternate word list from Alan Beale
  -f <file>	read word list from newline delimited file
  -t <file>	read word list from tsv file ('12345	word' style)
```

### Examples

**Generate a 6 word passphrase**

	$ diceware
	21311  24361  53244 15131 61135 14612
	colony embark sd    brain thing border

The numbers in the first line represent the virtual die rolls.  The words under
them are the resulting passphrase (you would type it in without the spaces).

**Generate a 7 word passphrase with an extra random character**

	$ diceware -w 7 -e
	33533* 34646  64451 36241 42422 12141 15265
	ic+    juggle wrist lease misty ames  bruit

The * indicates the position of the randomly added special character (in this
case the '+' character).

**Generate a 6 word passphrase from a tab separated values file**

	$ diceware -t words.tsv
	459   2750 7998 7956 4454 3528
	axial gnu  68th 5i   murk jukes

The -t flag expects to be passed a file with tab separated values. The first column should be every possible combination of 5 die rolls (from 11111 to 66666).  The second column is the word.  If there's a quote in the word, the word must be double quoted, and the quote escaped with another quote (so, *hi "son"* would become *"hi ""son"""*)

**Generate a 6 word passphrase from a newline delimited word list**

	$ diceware -f words.txt
	459   2750 7998 7956 4454 3528
	axial gnu  68th 5i   murk jukes

The -f flag expects to be passed a file where every word is on its own line.  Diceware then just randomly chooses a number from 0 to num(lines).  The numbers printed in the first row are the line number of the words that were chosen.
