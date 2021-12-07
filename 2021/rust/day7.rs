use std::io;
use std::ops::RangeInclusive;

fn parse<'source>(input: &'source str) -> impl Iterator<Item = u32> + 'source {
    input.split(',').filter_map(|s| s.parse().ok())
}

fn bounds(values: &[u32]) -> RangeInclusive<u32> {
    let min = values.iter().cloned().min().unwrap();
    let max = values.iter().cloned().max().unwrap();

    min..=max
}

#[inline]
fn distance(a: u32, b: u32) -> u32 {
    if a > b {
        a - b
    } else {
        b - a
    }
}

#[inline]
fn fuel(a: u32, b: u32) -> u32 {
    let distance = distance(a, b);
    // sum of the first N elements of an arithmetic progression
    // being 1 the first element (we start consuming 1 fuel and 1 each step) and
    (distance * (1 + distance)) / 2
}

// compute the sum fuel consumptions of `values` to go into `target`
fn total_fuel_consumption(
    fuel_fn: fn(u32, u32) -> u32,
    target: u32,
    values: impl Iterator<Item = u32>,
) -> u32 {
    values.map(|x| fuel_fn(target, x)).sum()
}

fn part_one(values: &[u32]) -> u32 {
    bounds(values).map(|target| {
        total_fuel_consumption(distance, target, values.iter().cloned())
    }).min().unwrap()
}

fn part_two(values: &[u32]) -> u32 {
    bounds(values).map(|target| {
        total_fuel_consumption(fuel, target, values.iter().cloned())
    }).min().unwrap()
}

fn main() -> io::Result<()> {
    let sample_input = std::fs::read_to_string("inputs/day7.txt")?;
    let values: Vec<_> = parse(sample_input.trim()).collect();


    println!("Part 1: {}", part_one(&values));
    println!("Part 2: {}", part_two(&values));


    Ok(())
}
