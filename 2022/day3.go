package main

import (
	"bufio"
	"fmt"
	"os"
)

var priorities map[int]int

func makePriorityMap() {
	priorities = make(map[int]int, 52)

	for i := 'a'; i <= 'z'; i++ {
		i := int(i)
		priorities[i] = i - int('a') + 1
	}
	for i := 'A'; i <= 'Z'; i++ {
		i := int(i)
		priorities[i] = i - int('A') + 27
	}
}

func makeRuckSackSet(rs string) map[rune]bool {
	var m map[rune]bool = make(map[rune]bool, len(rs))
	for _, r := range rs {
		m[r] = true
	}
	return m
}

func andSets(m1 map[rune]bool, m2 map[rune]bool) map[rune]bool {

	var inBoth []rune

	for r := range m1 {
		if exists, _ := m2[r]; exists {
			inBoth = append(inBoth, r)
		}
	}

	m := make(map[rune]bool)
	for _, r := range inBoth {
		m[r] = true
	}
	return m
}

func getOnlyValue(m map[rune]bool) rune {
	for k := range m {
		return k
	}
	panic("should have one value")
}

func part1(rucksacks []string) int {
	sum := 0
	for _, rs := range rucksacks {
		mid := len(rs) / 2
		a, b := rs[:mid], rs[mid:]
		m := makeRuckSackSet(a)
		m = andSets(m, makeRuckSackSet(b))

		sum += priorities[int(getOnlyValue(m))]
	}
	return sum
}

func part2(rucksacks []string) int {
	sum := 0
	for i := 0; i < len(rucksacks); i += 3 {
		a, b, c := rucksacks[i], rucksacks[i+1], rucksacks[i+2]
		m := makeRuckSackSet(a)
		m = andSets(m, makeRuckSackSet(b))
		m = andSets(m, makeRuckSackSet(c))
		sum += priorities[int(getOnlyValue(m))]
	}
	return sum
}

func main() {
	makePriorityMap()

	reader := bufio.NewReader(os.Stdin)

	var rucksacks []string
	for {
		line, err := reader.ReadString('\n')
		if err != nil || len(line) < 2 {
			break
		}
		rucksacks = append(rucksacks, line[:len(line)-1])
	}

	fmt.Printf("part 1: %d\n", part1(rucksacks))
	fmt.Printf("part 2: %d\n", part2(rucksacks))
}
