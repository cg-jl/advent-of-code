use std::io::{self, BufRead, BufReader};

#[derive(Debug, Clone, Copy)]
enum Action {
    Forward,
    Up,
    Down,
}

impl Action {
    fn parse(line: &str) -> Option<Self> {
        let it = match line {
            "forward" => Self::Forward,
            "up" => Self::Up,
            "down" => Self::Down,
            _ => return None,
        };
        Some(it)
    }
}


fn parse_line(line: &str) -> (Action, i32) {
    let mut it = line.split(' ');
    let action = it.next().and_then(Action::parse).unwrap();
    let num = it.next().unwrap().parse().unwrap();

    (action, num)
}

fn simulate_part1(actions: impl IntoIterator<Item = (Action, i32)>) -> i32 {
    let mut x = 0;
    let mut y = 0;
    for (action, dv) in actions {
        match action {
            Action::Forward => x += dv,
            Action::Up => y -= dv,
            Action::Down => y += dv,
        }
    }
    x * y
}

fn simulate_part2(actions: impl IntoIterator<Item = (Action, i32)>) -> i32 {
    let mut x = 0;
    let mut y = 0;
    let mut aim = 0;
    for (action, value) in actions {
        match action {
            Action::Forward => {
                x += value;
                y += value * aim;
            }
            Action::Up => {
                aim -= value;
            }
            Action::Down => aim += value,
        }
    }
    x * y
}



fn main() -> io::Result<()> {
    let actions: Vec<_> = std::fs::File::open("inputs/day2.txt").map(BufReader::new).map(BufReader::lines).and_then(|lines| {
        lines.map(|line| {
            let line = line?;
            Ok(parse_line(&line))
        }).collect::<Result<_, _>>()
    })?;
    println!("Part 1: {}", simulate_part1(actions.iter().cloned()));
    println!("Part 2: {}", simulate_part2(actions));
    Ok(())
}
    

