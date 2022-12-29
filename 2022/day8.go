package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
)

func parseLine(line string) []int {
	var res []int

	for _, r := range line[:len(line)-1] {
		res = append(res, int(r)-int('0'))
	}

	return res
}

func readInput(filename string) ([][]int, error) {
	file, err := os.Open(filename)
	if err != nil {
		return nil, err
	}

	reader := bufio.NewReader(file)
	var lines [][]int
	for {
		line, err := reader.ReadString('\n')
		if err != nil {
			break
		}
		lines = append(lines, parseLine(line))
	}

	if err != nil {
		return nil, err
	}
	return lines, nil
}

func main() {

	if input, err := readInput("input.test"); err != nil {
		log.Fatalf("%q\n", err)
	} else {
		fmt.Printf("width: %v\n", input)

	}

}
