use std::env;
use std::io;


const fn counterpart(ch: char) -> Option<char> {
    match ch {
        '<' => Some('>'),
        '[' => Some(']'),
        '{' => Some('}'),
        '(' => Some(')'),
        _ => None,
    }
}

fn check_line(line: &str) -> Result<impl Iterator<Item= char>, char>{
    let mut build = Vec::new();

    for ch in line.chars() {
        match ch {
            '<' | '[' | '{' | '(' => build.push(counterpart(ch).unwrap()),
            '>' | ']' | '}' | ')' => if build.pop() != Some(ch) { return Err(ch); },
            _ => (),
        }
    }

    Ok(build.into_iter().rev())
}


fn main() -> io::Result<()>  {
    let mut args = env::args().skip(1);
    let filename = if let Some(name) = args.next() {
        name
    } else {
        return Err(io::Error::new(io::ErrorKind::Other, "no input file"));
    };

    use io::BufRead;
    let bufreader = std::fs::File::open(filename).map(io::BufReader::new)?;
    let mut part_one = 0;
    let mut part_two = Vec::new();

    for line in bufreader.lines() {
        let line = line?;
        match check_line(&line) {
            Err(ch) => part_one += match ch {
                '>' => 25137,
                ']' => 57,
                '}' => 1197,
                ')' => 3,
                _ => 0,
            },
            Ok(st) => part_two.push(st.filter_map(|ch| match ch {
                ')' => Some(1usize),
                ']' => Some(2),
                '}' => Some(3),
                '>' => Some(4),
                _   => None,
            }).fold(0, |acc, next| acc * 5 + next)),
        }
    }
    part_two.sort();


    println!("Part one: {}", part_one);
    println!("Part two: {}", part_two[part_two.len() / 2]);


    
    // println!("Part one: {}", part_one.into_iter().filter_map(|ch| match ch {
    //     '>' => Some(25137usize),
    //     ']' => Some(57),
    //     '}' => Some(1197),
    //     '>' => Some(25137),
    //     _ => None,
    // }).sum());

    // println!("Part two: {}", part_two[part_two.len() / 2]);







    Ok(())



}