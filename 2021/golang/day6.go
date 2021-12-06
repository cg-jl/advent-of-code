package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strings"
)

func stoUint(value string) uint {
	var i uint = 0
	for _, ch := range value {
		i *= 10
		i += uint(ch) - uint('0')
	}
	return i
}

func readInput(line string) []uint {
	amt := make([]uint, 9)
	for _, snum := range strings.Split(line, ",") {
		index := stoUint(snum)
		amt[index]++
	}
	return amt
}

func day(world *[]uint) {
	timeouted := (*world)[0]

	for i := 1; i <= 8; i++ {
		(*world)[i-1] = (*world)[i]
	}

	(*world)[8] = timeouted
	(*world)[6] += timeouted

}

func count(values []uint) uint {
	var res uint = 0
	for _, v := range values {
		res += v
	}
	return res
}

func main() {
	file, err := os.Open("inputs/day6.txt")
	if err != nil {
		log.Fatal(err)
	}

	buffer := bufio.NewReader(file)
	line, _, err := buffer.ReadLine()
	if err != nil {
		log.Fatal(err)
	}

	world := readInput(string(line))
	for i := 0; i < 256; i++ {
		day(&world)
	}
	fmt.Printf("%v\n", count(world))
}
