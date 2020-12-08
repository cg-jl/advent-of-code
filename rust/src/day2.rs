pub fn part1(input: &str) -> i32 {
    input
        .split("\n")
        .map(|x| {
            if check_policy(&parse_policy(&x)) {
                1i32
            } else {
                0i32
            }
        })
        .sum::<i32>()
}

pub fn part2(input: &str) -> i32 {
    input
        .split("\n")
        .map(|x| {
            if check_policy_2(&parse_policy(&x)) {
                1i32
            } else {
                0i32
            }
        })
        .sum::<i32>()
}

fn check_policy_2(policy: &Policy) -> bool {
    let mut chars = policy.s2.chars();
    let mut count = 0;
    if let Some(c) = chars.nth(policy.i1) {
        if c == policy.c {
            count += 1;
        }
    }
    if let Some(c) = chars.nth(policy.i2) {
        if c == policy.c {
            count += 1;
        }
    }
    count == 1
}

#[derive(Debug)]
struct Policy<'a> {
    i1: usize,
    i2: usize,
    c: char,
    s2: &'a str,
}

fn parse_policy(line: &str) -> Policy {
    let space_index = line
        .find(' ')
        .expect("Policy should have a space character.");

    let hyphen_index = line
        .find('-')
        .expect("Policy should have a hyphen separating the two bounds.");

    let semicolon_index = line.find(':').expect("Policy should have a semicolon.");
    let i1 = line[..hyphen_index].parse::<usize>().unwrap();
    let i2 = line[hyphen_index + 1..space_index]
        .parse::<usize>()
        .unwrap();
    let c = line[space_index + 1..semicolon_index]
        .chars()
        .next()
        .unwrap();
    let s2 = &line[semicolon_index + 2..];
    Policy { i1, i2, c, s2 }
}

fn check_policy(policy: &Policy) -> bool {
    let count = policy.s2.matches(&String::from(policy.c)).count();
    policy.i1 <= count && policy.i2 >= count
}

#[cfg(test)]
mod tests {
    use super::*;
    const INPUT: &'static str = "1-3 a: abcde
1-3 b: cdefg
2-9 c: ccccccccc";
    #[test]
    fn test_part1() {
        assert_eq!(part1(INPUT), 2);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(INPUT), 1);
    }
}
