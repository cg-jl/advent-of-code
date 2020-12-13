use itertools::{zip, Itertools};
use std::collections::HashMap;
// data + line length
pub fn parse_input(input: &str) -> (Vec<u8>, usize) {
    let mut vec = Vec::new();
    let mut line_len = 0;
    for line in input.lines().filter_map(|line| {
        let line = line.trim();
        if line.is_empty() {
            None
        } else {
            Some(line.as_bytes())
        }
    }) {
        line_len = line.len();
        vec.extend(line);
    }
    (vec, line_len)
}

#[inline]
fn count(v: &Vec<u8>) -> u32 {
    v.iter()
        .filter_map(|&x| if x == b'#' { Some(1) } else { None })
        .sum()
}

#[inline]
fn are_equal(v1: &Vec<u8>, v2: &Vec<u8>) -> bool {
    !zip(v1, v2).any(|(k, v)| v != k)
}

#[inline]
fn check_cached(i: usize, cache: &mut HashMap<usize, u32>, input: &[u8], line_len: usize) -> u32 {
    if let Some(res) = cache.get(&i) {
        return *res;
    }
    let mut sum = 0;
    let can_go_left = i > 0;
    let can_go_right = i < input.len() - 1;
    let can_go_up = i > line_len;
    let can_go_down = i < input.len() - line_len - 1;

    if can_go_down {
        sum += check_occupied(&input[i + line_len]);
        if can_go_left {
            sum += check_occupied(&input[i + line_len - 1]);
        }
        if can_go_right {
            sum += check_occupied(&input[i + line_len + 1]);
        }
    }
    if can_go_up {
        sum += check_occupied(&input[i - line_len]);
        if can_go_left {
            sum += check_occupied(&input[i - line_len - 1]);
        }
        if can_go_right {
            sum += check_occupied(&input[i - line_len + 1]);
        }
    }

    if can_go_right {
        sum += check_occupied(&input[i + 1]);
    }
    if can_go_left {
        sum += check_occupied(&input[i - 1]);
    }

    cache.insert(i, sum);

    sum
}

#[inline]
fn check_occupied(b: &u8) -> u32 {
    if b == &b'#' {
        1
    } else {
        0
    }
}

pub fn part1_input(input: &str) -> u32 {
    let (mut input, line_len) = parse_input(input);
    part1(&mut input, line_len)
}

pub fn part1(input: &mut Vec<u8>, line_len: usize) -> u32 {
    let mut other_vec = Vec::with_capacity(input.len());
    let mut checked = HashMap::new();
    while !are_equal(&input, &other_vec) {
        checked.clear();
        // push new state to other_vec
        for (i, value) in input.iter().enumerate() {
            if *value == b'.' {
                other_vec.push(*value);
                continue;
            } // i dont care
            let occupied = check_occupied(value) == 1;
            let sum = check_cached(i, &mut checked, &input, line_len);
            other_vec.push(if occupied && sum >= 4 {
                b'L'
            } else if !occupied && sum == 0 {
                b'#'
            } else {
                *value
            });
        }
        // copy other_vec to last state and empty other_vec
        input.clear();
        input.extend(&other_vec);
        other_vec.clear();
    }
    count(&input)
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn part1() {
        assert_eq!(
            part1_input(
                r#"
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
"#
            ),
            37
        );
    }
}
