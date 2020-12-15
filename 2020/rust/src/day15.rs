/*
day15::part1            time:   [55.457 us 55.558 us 55.700 us]
day15::part2            time:   [2.1921 s 2.2231 s 2.2546 s]
*/

use std::collections::HashMap;
use lexical::parse_partial;


#[inline(always)]
fn count_turns(mut input: &[u8], nth: u32) -> u32 {
    let mut map = HashMap::new();
    let mut last = 0;
    let mut current_turn = 1;
    while input.len() > 0 {
        if unsafe { *input.get_unchecked(0)} == b',' {
            input = &input[1..];
        }
        let (n, ndigits) = parse_partial::<u32, _>(input).unwrap();
        input = &input[ndigits..];
        map.insert(n, current_turn);
        current_turn += 1;
    }


    while current_turn < nth {
        if let Some(v) = map.get_mut(&last) {
            last = current_turn - *v;
            *v = current_turn;
        }
        else {
            map.insert(last, current_turn);
            last = 0;
        }
        current_turn += 1;
    }
    last
}

#[inline(always)]
pub fn part1(input: &[u8]) -> u32 {
    count_turns(input, 2020)
}

#[inline(always)]
pub fn part2(input: &[u8]) -> u32 {
    count_turns(input, 30000000)
}

#[cfg(test)]
mod tests {
    #[test]
    fn part1() {
        for (input, n) in [
            ("0,3,6", 436),
            ("1,3,2", 1),
            ("2,1,3", 10),
            ("1,2,3", 27),
            ("2,3,1", 78),
            ("3,2,1", 438),
            ("3,1,2", 1836)
        ].iter() {
            assert_eq!(super::part1(input.as_bytes()), *n);
        }
    }
}