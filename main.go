package main

import (
	"bufio"
	"crypto/rand"
	"encoding/csv"
	"flag"
	"fmt"
	"io"
	"log"
	"math/big"
	"os"
	"strings"
	"text/tabwriter"
)

const version = "1.0.0"

var length = 6
var extra bool
var beale bool
var dw8k bool
var tsvfile string
var wordfile string

func init() {
	flag.IntVar(&length, "w", 6, "number of words to generate")
	flag.BoolVar(&extra, "e", false, "generate an extra random character")
	flag.BoolVar(&beale, "b", false, "use alternate word list from Alan Beale")
	flag.BoolVar(&dw8k, "8", false, "use diceware 8k word list")
	flag.StringVar(&tsvfile, "t", "", "read word list from tsv file ('12345\tword' style)")
	flag.StringVar(&wordfile, "f", "", "read word list from newline delimited file")

	log.SetFlags(0)

	flag.Usage = usage
}

func main() {
	flag.Parse()

	if len(flag.Args()) > 0 {
		flag.Usage()
		return
	}

	if moreThanOneTrue(beale, dw8k, (tsvfile != ""), (wordfile != "")) {
		log.Fatalln("Only one of -b, -8, -t, and -f may be specified.")
	}

	var chosen, rolls []string
	switch {
	case wordfile != "":
		f, err := os.Open(wordfile)
		if err != nil {
			log.Fatalln(err)
		}
		defer f.Close()
		rolls, chosen = fromWordlist(f)
	case tsvfile != "":
		f, err := os.Open(tsvfile)
		if err != nil {
			log.Fatalln(err)
		}
		defer f.Close()
		rolls, chosen = fromTSV(f)
	case dw8k:
		rolls, chosen = fromWordlist(strings.NewReader(diceware8k))
	case beale:
		rolls, chosen = fromTSV(strings.NewReader(bealeList))
	default:
		rolls, chosen = fromTSV(strings.NewReader(list))
	}

	if extra {
		n, err := rand.Int(rand.Reader, big.NewInt(6))
		if err != nil {
			log.Fatalln(err)
		}
		c, err := rand.Int(rand.Reader, big.NewInt(36))
		if err != nil {
			log.Fatalln(err)
		}
		i := n.Int64()
		rolls[i] += "*"
		chosen[i] += extras[c.Int64()]
	}

	w := &tabwriter.Writer{}
	w.Init(os.Stdout, 0, 0, 1, ' ', 0)
	fmt.Fprintln(w, strings.Join(rolls, "\t"))

	fmt.Fprintln(w, strings.Join(chosen, "\t"))
	w.Flush()
}

func roll() string {
	six := big.NewInt(6)
	const one = byte('1')
	var rolls = make([]byte, 5)
	for x := 0; x < 5; x++ {
		n, err := rand.Int(rand.Reader, six)
		if err != nil {
			log.Fatalln(err)
		}
		rolls[x] = one + byte(n.Int64())
	}
	return string(rolls)
}

func fromTSV(rd io.Reader) (rolls, chosen []string) {
	r := csv.NewReader(rd)
	r.Comma = '\t'
	r.FieldsPerRecord = 2

	words := make(map[string]string, 7776)
	rec, err := r.Read()
	for err == nil {
		words[rec[0]] = rec[1]
		rec, err = r.Read()
	}

	if err != io.EOF {
		log.Fatalln(err)
	}

	if len(words) != 7776 {
		log.Fatalf("Expected exactly 7776 words in tsv wordlist, but found %d", len(words))
	}

	rolls = make([]string, length)
	for x := 0; x < length; x++ {
		rolls[x] = roll()
	}

	chosen = make([]string, length)
	for x, r := range rolls {
		chosen[x] = words[r]
	}
	return rolls, chosen
}

func fromWordlist(r io.Reader) (rolls, chosen []string) {
	scanner := bufio.NewScanner(r)
	// guess at a big size
	words := make([]string, 0, 8192)
	for scanner.Scan() {
		w := scanner.Text()
		if strings.ContainsRune(w, '\t') {
			log.Fatalf("tab found in word %q, maybe you meant to use -t?", w)
		}
		words = append(words, w)
	}

	if err := scanner.Err(); err != nil {
		log.Fatalln(err)
	}
	rolls = make([]string, length)
	chosen = make([]string, length)

	max := big.NewInt(int64(len(words)))
	for x := 0; x < length; x++ {
		n, err := rand.Int(rand.Reader, max)
		if err != nil {
			log.Fatalln(err)
		}
		i := n.Int64()
		rolls[x] = fmt.Sprintf("%d", i)
		chosen[x] = words[i]
	}
	return rolls, chosen
}

func moreThanOneTrue(bools ...bool) bool {
	found := false
	for _, b := range bools {
		if b {
			if found {
				return true
			}
			found = true
		}
	}
	return false
}

func usage() {
	fmt.Printf(`diceware v%s

Diceware generates passphrases using a method that conforms to the algorithm
stated here: http://world.std.com/~reinhold/diceware.html

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
`, version)
}
