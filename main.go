package main

import (
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

var length = 6
var extra bool
var beale bool
var file string

func init() {
	flag.IntVar(&length, "n", 6, "number of words to generate")
	flag.BoolVar(&extra, "e", false, "generate an extra random character")
	flag.BoolVar(&beale, "b", false, "use alternate word list from Alan Beale")
	flag.StringVar(&file, "f", "", "read word list from file (must be tsv)")

	log.SetFlags(0)
}

func main() {
	flag.Parse()

	if len(flag.Args()) > 0 {
		log.Println(`Diceware generates passphrases using a method that conforms to the algorithm
stated here: http://world.std.com/~reinhold/diceware.html
`)
		flag.Usage()
		return
	}

	if beale && file != "" {
		log.Fatalln("Flags -b and -f conflict and cannot both be specified.")
	}

	var r *csv.Reader
	switch {
	case beale:
		r = csv.NewReader(strings.NewReader(bealeList))
	case file != "":
		f, err := os.Open(file)
		if err != nil {
			log.Fatalln(err)
		}
		defer f.Close()
		r = csv.NewReader(f)
	default:
		r = csv.NewReader(strings.NewReader(list))
	}
	r.Comma = '\t'

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
		log.Fatalf("Expected exactly 7776 words in wordlist, but found %d", len(words))
	}

	rolls := make([]string, length)
	for x := 0; x < length; x++ {
		rolls[x] = roll()
	}

	chosen := make([]string, length)
	for x, r := range rolls {
		chosen[x] = words[r]
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
