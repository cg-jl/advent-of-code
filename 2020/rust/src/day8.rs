use std::cmp::min;
use std::collections::HashSet;
use std::fmt;

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
enum InstructionType {
    Jmp,
    Acc,
    Nop,
}
#[derive(Debug, Copy, Clone)]
struct Instruction {
    instruction_type: InstructionType,
    value: i32,
}

struct Cpu {
    pub instructions: Vec<Instruction>,
    accumulator: i32,
    current_line: i32,
}

impl From<&str> for Cpu {
    fn from(input: &str) -> Self {
        let mut cpu = Self::new();
        cpu.load_program(&Self::read_instructions(input));
        cpu
    }
}

impl Cpu {
    pub fn new() -> Self {
        Cpu {
            current_line: 0,
            accumulator: 0,
            instructions: Vec::new(),
        }
    }
    pub fn load_program(&mut self, program: &[Instruction]) {
        for line in 0..min(program.len(), self.instructions.len()) {
            self.instructions[line] = program[line];
        }

        if program.len() > self.instructions.len() {
            for line in 0..program.len() - self.instructions.len() {
                self.instructions.push(program[line]);
            }
        }
    }
    fn read_instructions(input: &str) -> Vec<Instruction> {
        let mut vec = Vec::new();
        for line in input.lines() {
            let line = line.trim();
            if line.is_empty() {
                continue;
            }
            let num = line[4..].trim().parse::<i32>().unwrap();
            match &line[..3] {
                "nop" => {
                    vec.push(Instruction {
                        instruction_type: InstructionType::Nop,
                        value: num,
                    });
                }
                "acc" => {
                    vec.push(Instruction {
                        instruction_type: InstructionType::Acc,
                        value: num,
                    });
                }
                "jmp" => {
                    vec.push(Instruction {
                        instruction_type: InstructionType::Jmp,
                        value: num,
                    });
                }
                _ => {}
            }
        }
        vec
    }

    pub fn step(&mut self) -> (i32, Option<Instruction>) {
        let mut opt = None;
        let ret_line = self.current_line;
        match self.instructions.get(self.current_line as usize) {
            Some(i) => {
                opt = Some(*i);
                match i.instruction_type {
                    InstructionType::Jmp => {
                        self.current_line += i.value - 1;
                    }
                    InstructionType::Acc => {
                        self.accumulator += i.value;
                    }
                    _ => {}
                }
                self.current_line += 1;
            }
            // shouldn't get to here as its infinite loop
            None => {} // NOTE: I could tell it to run until theres no more nops
        }
        (ret_line, opt)
    }
    pub fn counter(&self) -> i32 {
        self.current_line
    }
    pub fn get_accumulator(&self) -> i32 {
        self.accumulator
    }

    pub fn swap_nop_jmp(&mut self, line: usize) {
        if let Some(v) = self.instructions.get_mut(line) {
            if v.instruction_type == InstructionType::Jmp {
                v.instruction_type = InstructionType::Nop;
            } else if v.instruction_type == InstructionType::Nop {
                v.instruction_type = InstructionType::Jmp;
            }
        }
    }

    pub fn reset(&mut self) {
        self.current_line = 0;
        self.accumulator = 0;
    }

    // returns true if it could execute with no loops
    pub fn try_execute_no_loops(&mut self) -> bool {
        let mut set = HashSet::new();

        while !set.contains(&self.counter()) {
            let (counter, opt) = self.step();
            if let None = opt {
                return true; // finished
            }
            set.insert(counter);
        }
        false
    }
}

pub fn part1(input: &str) -> i32 {
    let mut cpu = Cpu::from(input);
    cpu.try_execute_no_loops();
    cpu.get_accumulator()
}

pub fn part2(input: &str) -> i32 {
    let mut cpu = Cpu::from(input);
    for line in 0..cpu.instructions.len() {
        if !vec![InstructionType::Jmp, InstructionType::Nop]
            .contains(&(cpu.instructions[line].instruction_type))
        {
            continue;
        }
        cpu.reset();
        cpu.swap_nop_jmp(line);
        if cpu.try_execute_no_loops() {
            return cpu.get_accumulator();
        }
        cpu.swap_nop_jmp(line);
    }
    -1
}

#[cfg(test)]

mod tests {
    const INPUT: &'static str = r#"
        nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6
        "#;
    #[test]
    fn part1() {
        assert_eq!(super::part1(INPUT), 5);
    }

    #[test]
    fn part2() {
        assert_eq!(super::part2(INPUT), 8);
    }
}
