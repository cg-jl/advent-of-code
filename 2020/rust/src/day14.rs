use fxhash::FxHasher;
use itertools::Itertools;
use std::{collections::HashMap, hash::BuildHasherDefault, iter::repeat};
fn count_1s_bin(mut x: u64) -> Vec<u8> {
    let mut vec = Vec::new();
    let mut i = 0;
    while x > 0 {
        if x & 1 == 1 {
            vec.push(i);
        }
        i += 1;
        x >>= 1;
    }
    vec
}

use lexical::parse_partial;

pub fn parse_mask(line: &mut &[u8]) -> (u64, u64) {
    *line = &line[6..];
    let res = line[..=36].iter().fold((0, 0), |(mut ones, mut xs), &val| {
        ones <<= 1;
        xs <<= 1;
        match val {
            b'X' => xs |= 1,
            b'1' => ones |= 1,
            _ => {}
        }
        (ones, xs)
    });
    *line = &line[36..];
    res
}

pub fn parse_assignment(line: &mut &[u8]) -> (u64, u64) {
    *line = &line[4..];
    let (addr, ndigits) = parse_partial::<u64, _>(&line).unwrap();
    *line = &line[ndigits + 4..];
    let (val, ndigits) = parse_partial::<u64, _>(&line).unwrap();
    *line = &line[ndigits..];
    (addr, val)
}

pub fn part1(mut line: &[u8]) -> u64 {
    let (mut xs, mut not_xs, mut first_expr) = (0u64, 0u64, 0u64);
    let mut mem = HashMap::<u64, u64, BuildHasherDefault<FxHasher>>::with_capacity_and_hasher(
        256 * 1024,
        Default::default(),
    );
    while line.len() > 1 {
        match unsafe { *line.get_unchecked(1) } {
            b'a' => {
                let (mask, next_xs) = parse_mask(&mut line);
                xs = next_xs;
                not_xs = !xs;
                first_expr = xs | (not_xs & mask);
            }
            b'e' => {
                let (k, v) = parse_assignment(&mut line);
                let val = first_expr & (not_xs | (xs & v));
                *mem.entry(k).or_default() = val;
            }
            _ => {}
        }
        if line.len() > 0 {
            line = &line[1..];
        }
    }
    mem.values().sum()
}

pub fn part2(mut line: &[u8]) -> u64 {
    let (mut mask, mut xs, mut xs_idxs) = (0, 0, Vec::new());

    let mut mem = HashMap::<u64, u64, BuildHasherDefault<FxHasher>>::with_capacity_and_hasher(
        256 * 1024,
        Default::default(),
    );

    while line.len() > 1 {
        match unsafe { *line.get_unchecked(1) } {
            b'a' => {
                let (new_mask, new_xs) = parse_mask(&mut line);
                xs_idxs = count_1s_bin(new_xs);
                xs = new_xs;
                mask = new_mask;
            }
            b'e' => {
                let (mut k, v) = parse_assignment(&mut line);
                k &= xs | !mask;
                k |= !xs & mask;

                // let mut count = 0;
                // for _ in 0..(1 << xs.count_ones()) {
                //     *mem.entry(count & xs | mask | k).or_default() = v;
                //     count = count.wrapping_add(1);
                //     count |= !xs;
                // }

                for p in repeat([0, 1].iter())
                    .take(xs_idxs.len())
                    .multi_cartesian_product()
                {
                    let mut k0 = k.clone();
                    for (i, idx) in xs_idxs.iter().enumerate() {
                        unsafe {
                            k0 &= !(1 << idx);
                            k0 |= *p.get_unchecked(i) << idx;
                        }
                    }
                    *mem.entry(k0).or_default() = v;
                }
            }

            _ => {}
        }
        if line.len() > 0 {
            line = &line[1..];
        }
    }

    mem.values().sum()
}

#[cfg(test)]
mod tests {
    #[test]
    fn part1() {
        assert_eq!(
            super::part1(
                br#"mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
mem[8] = 11
mem[7] = 101
mem[8] = 0"#
            ),
            165
        );
    }

    #[test]
    fn part2() {
        assert_eq!(
            super::part2(
                br#"mask = 000000000000000000000000000000X1001X
mem[42] = 100
mask = 00000000000000000000000000000000X0XX
mem[26] = 1"#
            ),
            208
        );
    }
}
