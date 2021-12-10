package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"sort"
)

var SCORES map[rune]uint = map[rune]uint{'>': 25137, '}': 1197, ']': 57, ')': 3}

func checkLine(line string) (rune, []rune) {
	var last []rune
	for _, ch := range line {
		switch ch {
		case '<':
			last = append([]rune{'>'}, last...)
		case '{':
			last = append([]rune{'}'}, last...)
		case '[':
			last = append([]rune{']'}, last...)
		case '(':
			last = append([]rune{')'}, last...)

		case '>':
			fallthrough
		case ']':
			fallthrough
		case '}':
			fallthrough
		case ')':
			if last[0] != ch {
				return ch, last
			}
			last = last[1:]
		}
	}
	return 0, last
}

func completionScore(completion []rune) int {
	score := 0
	for _, ch := range completion {
		score *= 5
		switch ch {
		case ')':
			score += 1
		case ']':
			score += 2
		case '}':
			score += 3
		case '>':
			score += 4
		}
	}
	return score
}

func main() {
	if len(os.Args) != 2 {
		fmt.Fprintf(os.Stderr, "usage: %s <file>\n", os.Args[0])
		os.Exit(1)
	}

	file, err := os.Open(os.Args[1])
	if err != nil {
		log.Fatal(err)
	}

	buf := bufio.NewScanner(file)

	var partOne uint = 0
	var partTwo []int

	for buf.Scan() {
		line := buf.Text()
		if errch, ac := checkLine(line); errch != 0 {
			partOne += SCORES[errch]
		} else {
			partTwo = append(partTwo, completionScore(ac))
		}
	}

	if err := buf.Err(); err != nil {
		log.Fatal(err)
	}
	sort.Ints(partTwo)

	fmt.Printf("Part one: %v\n", partOne)
	fmt.Printf("Part two: %v\n", partTwo[len(partTwo)/2])

}
