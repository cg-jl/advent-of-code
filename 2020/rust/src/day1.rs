pub fn part2(input: &str) -> i32 {
    let mut got = Vec::new();
    for line in input.split("\n") {
        if let Ok(n) = line.parse::<i32>() {
            for y in got.iter() {
                for x in got.iter() {
                    if y + x + n == 2020 {
                        return y * x * n;
                    }
                }
            }
            got.push(n);
        }
    }
    -1
}

pub fn part1(input: &str) -> i32 {
    let mut got = Vec::new();
    for line in input.split("\n") {
        if let Ok(n) = line.parse::<i32>() {
            for x in got.iter() {
                if x + n == 2020 {
                    return x * n;
                }
            }
            got.push(n);
        }
    }
    -1
}

#[cfg(test)]
mod tests {
    use super::*;
    const INPUT: &'static str = "1721\n979\n366\n299\n675\n1456\n";
    #[test]
    fn test_part1() {
        assert_eq!(part1(INPUT), 514579);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(INPUT), 241861950);
    }
}
