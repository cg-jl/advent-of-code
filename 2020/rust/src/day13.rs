use lexical::parse_partial;
use std::cmp;
use std::i32;
use std::str;
pub fn part1(input: &[u8]) -> i32 {
    let mut input = input;
    let (start, nd) = parse_partial::<i32, _>(input).unwrap();
    #[cfg(windows)]
    {
    input = &input[nd + 2..];
    }
    #[cfg(not(windows))]
    {
    input = &input[nd + 1..];
    }
    let mut better_id = 0;
    let mut min_departure = i32::MAX;
    while input.len() > 1 {
        // were at <number|x>,
        if input[0] == b',' {
            input = &input[1..];
        }
        if input[0] == b'x' {
            // skip x,
            input = &input[2..];
            continue;
        }
        let (n, ndig) = parse_partial::<i32, _>(input).unwrap();
        input = &input[ndig..];
        let departure = (n + start - 1) / n * n;
        if departure < min_departure {
            better_id = n;
            min_departure = departure;
        }
    }
    better_id * (min_departure - start)
}

use ring_algorithm::chinese_remainder_theorem;

// generic
// TODO: make it work
pub fn part2_generic(mut input: &[u8]) -> i64 {
    // fist line doesn't  count
    while input.len() > 0 && input[0] != b'\n' {
        input = &input[1..];
    }
    input = &input[1..]; // skip newline
    println!("input = {:?}", str::from_utf8(input).unwrap());

    // get the numbers and their indexes
    let mut idx = 0;
    let mut idx_vec = Vec::new();
    let mut nums_vec = Vec::new();

    while input.len() > 0 {
        if input[0] == b',' {
            input = &input[1..];
        }
        if input[0] == b'x' {
            input = &input[cmp::min(input.len(), 2)..];
        } else {
            println!("[parse] input = {:?}", str::from_utf8(input).unwrap());
            let (num, ndigits) = parse_partial::<i64, _>(input).unwrap();
            input = &input[ndigits..];
            nums_vec.push(num);
            idx_vec.push(idx);
        }
        idx += 1;
    }
    dbg!(&nums_vec, &idx_vec);
    println!();

    nums_vec.iter().product::<i64>() - chinese_remainder_theorem(&idx_vec, &nums_vec).unwrap()
}

#[cfg(test)]
mod tests {
    #[test]
    fn part1() {
        let i: &'static [u8] = include_bytes!("../inputs/13.txt");
        assert_eq!(super::part1(&i), 295);
    }

    #[test]
    fn part2() {
        for (i, must) in [
            ("\n17,x,13,19", 3417),
            ("\n67,7,59,61", 754018),
            ("\n67,x,7,59,61", 779210),
            ("\n67,7,x,59,61", 1261476),
            ("\n1789,37,47,1889", 1202161486),
        ]
        .iter()
        {
            assert_eq!(super::part2_generic(i.as_bytes()), *must);
        }
    }
}
