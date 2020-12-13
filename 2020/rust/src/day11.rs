// pretty slow :)
use std::collections::HashMap;
use std::convert::From;
use std::fmt;

#[derive(Eq, PartialEq, Copy, Clone)]
enum Seat {
    Occupied,
    Free,
    Floor,
}

impl From<&u8> for Seat {
    #[inline]
    fn from(value: &u8) -> Self {
        match *value {
            b'L' => Self::Free,
            b'#' => Self::Occupied,
            _ => Self::Floor,
        }
    }
}

impl fmt::Debug for Seat {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(&String::from(char::from(self)))
    }
}

impl From<&Seat> for char {
    #[inline]
    fn from(value: &Seat) -> Self {
        match *value {
            Seat::Occupied => '#',
            Seat::Free => 'L',
            Seat::Floor => '.',
        }
    }
}

impl From<&Seat> for u32 {
    #[inline]
    fn from(value: &Seat) -> u32 {
        match *value {
            Seat::Occupied => 1u32,
            _ => 0,
        }
    }
}

struct State {
    line_len: usize,
    seats: Vec<Seat>,
}

impl PartialEq for State {
    fn eq(&self, other: &State) -> bool {
        for (a, b) in self.seats.iter().zip(other.seats.iter()) {
            if *a != *b {
                return false;
            }
        }
        true
    }
}

impl Eq for State {}

impl State {
    pub fn new(input: &str) -> Self {
        // parse
        let mut l = 0;
        let mut vec = Vec::new();
        for line in input.lines().filter_map(|x| {
            let x = x.trim();
            if x.is_empty() {
                None
            } else {
                Some(x.as_bytes())
            }
        }) {
            l = line.len();
            vec.extend(line.iter().map(Seat::from));
        }

        Self {
            line_len: l,
            seats: vec,
        }
    }
    fn apply_changes(&mut self, changes: &HashMap<usize, Seat>) {
        for (key, value) in changes.iter() {
            let v = self.seats.get_mut(*key).unwrap();
            *v = *value;
        }
    }
    #[inline]
    fn get(&self, x: usize, y: usize) -> Option<&Seat> {
        if x >= self.line_len || y >= self.seats.len() {
            None
        } else {
            self.seats.get(y * self.line_len + x)
        }
    }

    // part 2
    fn count_with_sight_direction(&self, x: usize, y: usize, direction: (i32, i32)) -> u32 {
        let mut sum = 0;
        let (dx, dy) = direction;
        /*
        Naive implemmentation:
            while I'm not in a border (x is 0 or len and y is 0 or len) or encountered an occupied seat do
                count if seat is free


        */
        let mut x = x as i32;
        let mut y = y as i32;

        loop {
            // out of bounds check
            x += dx;
            y += dy;
            if x < 0
                || y < 0
                || y as usize >= self.seats.len() / self.line_len
                || x as usize >= self.line_len
            {
                break;
            }
            if let Some(seat) = self.get(x as usize, y as usize) {
                if *seat != Seat::Floor {
                    sum += u32::from(seat);
                    break; // occupied seat, FOV ended.
                }
                continue;
            }
            break;
        }
        sum
    }

    // part 1
    pub fn get_occupied_neighbors(&self, x: usize, y: usize, count_with_sight: bool) -> u32 {
        if let Some(v) = self.get(x, y) {
            if *v == Seat::Floor {
                return 0;
            }
        }
        let mut sum = 0;
        // part1
        for (dx, dy) in [
            (-1, -1), // left up
            (0, -1),  // center up
            (1, -1),  // right up
            (-1, 0),  // left center
            (1, 0),   // right center
            (-1, 1),  // left down
            (0, 1),   // center down
            (1, 1),   // right down
        ]
        .iter()
        {
            if *dx < 0 && x == 0 || (*dy < 0 && y == 0) {
                continue;
            }
            if let Some(v) = self.get((x as i32 + dx) as usize, (y as i32 + *dy) as usize) {
                sum += if !count_with_sight {
                    u32::from(v)
                } else {
                    self.count_with_sight_direction(x, y, (*dx, *dy))
                };
            }
        }

        sum
    }
    // returns wether or not it changed
    // inline? there's a couple branches, not sure if that will
    // be reduced correctly
    #[inline]
    fn do_round_single(&self, x: usize, y: usize, uses_direction: bool) -> Option<(Seat, bool)> {
        let neighbors = self.get_occupied_neighbors(x, y, uses_direction);
        if let Some(v) = self.get(x, y) {
            return match *v {
                Seat::Occupied => Some(if neighbors >= (if uses_direction { 5 } else { 4 }) {
                    (Seat::Free, true)
                } else {
                    (Seat::Occupied, false)
                }),
                Seat::Free => Some(if neighbors == 0 {
                    (Seat::Occupied, true)
                } else {
                    (Seat::Free, false)
                }),
                _ => Some((Seat::Floor, false)),
            };
        }
        None
    }
    #[inline]
    fn do_round(&mut self, uses_direction: bool) -> bool {
        let mut to_change = HashMap::new();
        for i in 0..self.seats.len() {
            let y = i / self.line_len;
            let x = i % self.line_len;
            if let Some((next, has_changed)) = self.do_round_single(x, y, uses_direction) {
                // store the change
                if has_changed {
                    to_change.insert(i, next);
                }
            }
        }

        self.apply_changes(&to_change);

        to_change.len() > 0
    }
    #[inline]
    pub fn run_until_change(&mut self, uses_direction: bool) -> &Self {
        while self.do_round(uses_direction) {}
        self
    }

    pub fn sum(&self) -> u32 {
        self.seats.iter().map(u32::from).sum()
    }
}

impl fmt::Debug for State {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("\n")?;
        for line in self.seats.chunks_exact(self.line_len) {
            f.write_str(&line.iter().map(char::from).collect::<String>())?;
            f.write_str("\n")?;
        }
        Ok(())
    }
}

#[inline]
pub fn part1(input: &str) -> u32 {
    State::new(input).run_until_change(false).sum()
}

#[inline]
pub fn part2(input: &str) -> u32 {
    State::new(input).run_until_change(true).sum()
}

#[cfg(test)]
fn test_stages(stages: &[&str], uses_visibility: bool) {
    let mut state = State::new(stages[0]);
    for (i, &expected) in stages.iter().enumerate() {
        println!("Checking stage {}", i + 1);
        assert_eq!(state, State::new(expected), "Stage {} failed", i + 1);
        state.do_round(uses_visibility);
    }
    assert_eq!(
        state.do_round(uses_visibility),
        false,
        "State should be stabilized now."
    );
}
#[cfg(test)]
mod part1 {
    use super::*;
    const INPUT: &'static str = r#"
L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL
"#;

    mod state {
        use super::*;
        #[test]
        fn parse() {
            assert_eq!(
                format!("{:?}", State::new(INPUT)),
                INPUT,
                "Either bad formatting or bad parsing"
            );
        }

        #[test]
        fn stages() {
            let stages = [
                INPUT, // first state should be the input as parse passes
                r#"
#.##.##.##
#######.##
#.#.#..#..
####.##.##
#.##.##.##
#.#####.##
..#.#.....
##########
#.######.#
#.#####.##
"#,
                r#"
#.LL.L#.##
#LLLLLL.L#
L.L.L..L..
#LLL.LL.L#
#.LL.LL.LL
#.LLLL#.##
..L.L.....
#LLLLLLLL#
#.LLLLLL.L
#.#LLLL.##
"#,
                r#"
#.##.L#.##
#L###LL.L#
L.#.#..#..
#L##.##.L#
#.##.LL.LL
#.###L#.##
..#.#.....
#L######L#
#.LL###L.L
#.#L###.##
"#,
                r#"
#.#L.L#.##
#LLL#LL.L#
L.L.L..#..
#LLL.##.L#
#.LL.LL.LL
#.LL#L#.##
..L.L.....
#L#LLLL#L#
#.LLLLLL.L
#.#L#L#.##
"#,
                r#"
#.#L.L#.##
#LLL#LL.L#
L.#.L..#..
#L##.##.L#
#.#L.LL.LL
#.#L#L#.##
..L.L.....
#L#L##L#L#
#.LLLLLL.L
#.#L#L#.##
"#,
            ];

            test_stages(&stages, false);
        }
    }

    #[test]
    fn main() {
        assert_eq!(super::part1(INPUT), 37);
    }
}
#[cfg(test)]
mod part2 {
    use super::*;
    const INPUT: &'static str = r#"
L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL"#;
    #[test]
    fn count_with_sight() {
        assert_eq!(
            State::new(
                r#"
.......#.
...#.....
.#.......
.........
..#L....#
....#....
.........
#........
...#....."#
            )
            .get_occupied_neighbors(3, 4, true),
            8
        );

        assert_eq!(
            State::new(
                r#"
.............
.L.L.#.#.#.#.
............."#
            )
            .get_occupied_neighbors(1, 1, true),
            0
        );

        assert_eq!(
            State::new(
                r#"
.##.##.
#.#.#.#
##...##
...L...
##...##
#.#.#.#
.##.##.
            "#
            )
            .get_occupied_neighbors(3, 3, true),
            0
        );
    }

    #[test]
    fn stages() {
        let stages = [
            INPUT,
            r#"#.##.##.##
#######.##
#.#.#..#..
####.##.##
#.##.##.##
#.#####.##
..#.#.....
##########
#.######.#
#.#####.##"#,
            r#"#.LL.LL.L#
#LLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLL#
#.LLLLLL.L
#.LLLLL.L#
"#,
            r#"
#.L#.##.L#
#L#####.LL
L.#.#..#..
##L#.##.##
#.##.#L.##
#.#####.#L
..#.#.....
LLL####LL#
#.L#####.L
#.L####.L#
"#,
            r#"
#.L#.L#.L#
#LLLLLL.LL
L.L.L..#..
##LL.LL.L#
L.LL.LL.L#
#.LLLLL.LL
..L.L.....
LLLLLLLLL#
#.LLLLL#.L
#.L#LL#.L#
"#,
            r#"
#.L#.L#.L#
#LLLLLL.LL
L.L.L..#..
##L#.#L.L#
L.L#.#L.L#
#.L####.LL
..#.#.....
LLL###LLL#
#.LLLLL#.L
#.L#LL#.L#
"#,
            r#"
#.L#.L#.L#
#LLLLLL.LL
L.L.L..#..
##L#.#L.L#
L.L#.LL.L#
#.LLLL#.LL
..#.L.....
LLL###LLL#
#.LLLLL#.L
#.L#LL#.L#
"#,
        ];

        test_stages(&stages, true);
    }

    // #[test]
    // fn main() {
    //     assert_eq!(part2(INPUT), 26);
    // }
}
