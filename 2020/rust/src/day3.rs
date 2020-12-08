fn count_trees(tree: std::slice::Iter<'_, (usize, u32)>, amount_x: usize, amount_y: usize) -> u32 {
    tree.enumerate()
        .map(|(y, b)| {
            if y % amount_y != 0 {
                return 0u32;
            }
            let len = b.0;
            let num = b.1;
            let num_shifts = len - (((y / amount_y) * amount_x) % len) - 1;
            (num >> num_shifts) & 1
        })
        .sum::<u32>()
}
pub fn part1(input: &str) -> u32 {
    count_trees(TreeParser::new(input).collect::<Vec<_>>().iter(), 3, 1)
}

pub fn part2(input: &str) -> u32 {
    // I hate this but I've got no choice!
    let parsed = TreeParser::new(input).collect::<Vec<_>>();

    vec![(1usize, 1usize), (3, 1), (5, 1), (7, 1), (1, 2)]
        .iter()
        .map(|(skip_x, skip_y)| count_trees(parsed.iter(), *skip_x, *skip_y))
        .fold(1u32, |a, b| {
            println!("a: {}, b: {}", a, b);
            a * b
        })
}

#[derive(Clone)]
struct TreeParser<'a> {
    split_input: std::str::Split<'a, &'a str>,
}

impl<'a> TreeParser<'a> {
    fn new(input: &'a str) -> Self {
        Self {
            split_input: input.split("\n"),
        }
    }
}

impl<'a> Iterator for TreeParser<'a> {
    type Item = (usize, u32);
    fn next(&mut self) -> Option<Self::Item> {
        match self.split_input.next() {
            Some(line) if !line.is_empty() => Some((
                line.len(),
                line.chars().fold(0u32, |acc, c| {
                    (acc << 1) | if c == '#' { 1u32 } else { 0u32 }
                }),
            )),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    const SAMPLE: &'static str = "..##.......\n#...#...#..\n.#....#..#.\n..#.#...#.#\n.#...##..#.\n..#.##.....\n.#.#.#....#\n.#........#\n#.##...#...\n#...##....#\n.#..#...#.#\n";
    #[test]
    pub fn test_part1() {
        assert_eq!(part1(SAMPLE), 7u32);
    }

    #[test]
    pub fn test_part2() {
        let parsed = TreeParser::new(SAMPLE).collect::<Vec<_>>();
        for (test, solution) in [
            ((1usize, 1usize), 2),
            ((3usize, 1usize), 7),
            ((5usize, 1usize), 3),
            ((7usize, 1usize), 4),
            ((1usize, 2usize), 2),
        ]
        .iter()
        {
            println!("count_trees{:?}", *test);
            assert_eq!(count_trees(parsed.iter(), (*test).0, (*test).1), *solution);
        }
        assert_eq!(part2(SAMPLE), 336u32);
    }
}
