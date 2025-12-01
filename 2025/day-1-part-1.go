package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
)

type instructionKind int

const (
	kindIncrement instructionKind = iota
	kindDecrement
)

type instruction struct {
	kind   instructionKind
	amount int
}

func parseDirection(c rune) instructionKind {
	switch c {
	case 'L':
		return kindDecrement
	case 'R':
		return kindIncrement
	}

	panic("unreachable")
}

func parseInstruction(text string) (*instruction, error) {
	var pState string = "dir"
	var direction instructionKind
	var int = ""

	for _, c := range text {
		switch pState {
		case "dir":
			direction = parseDirection(c)
			pState = "num"
		case "num":
			int += string(c)
		}
	}

	// finish parsing that number
	amount, err := strconv.Atoi(int)
	if err != nil {
		return nil, fmt.Errorf("failed to parse int %s: %w", int, err)
	}

	return &instruction{
		kind:   direction,
		amount: amount,
	}, nil
}

func getStepsFromInput(path string) ([]instruction, error) {
	file, err := os.Open(path)
	if err != nil {
		return nil, err
	}

	defer file.Close()

	var steps []instruction
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		inst, err := parseInstruction(scanner.Text())
		if err != nil {
			return nil, err
		}
		steps = append(steps, *inst)
	}

	return steps, nil
}
func wrapAround(upperBound int, lowerBound int, value int) int {
	n := (upperBound - lowerBound + 1)
	return lowerBound + (((value-lowerBound)+n)+n)%n
}

func main() {
	cursor := 50
	password := 0

	out, err := getStepsFromInput("inputs/2025/day-01p1.txt")
	if err != nil {
		panic(err)
	}

	for _, step := range out {
		switch step.kind {
		case kindIncrement:
			cursor += step.amount
		case kindDecrement:
			cursor -= step.amount
		}

		cursor = wrapAround(99, 0, cursor)
		if cursor == 0 {
			password += 1
		}
	}

	fmt.Printf("The password is: %d\n", password)
}
