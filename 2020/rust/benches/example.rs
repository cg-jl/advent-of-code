use aoc_rust::day11;
//use aoc_rust::day11_faster;
use criterion::{criterion_group, criterion_main, Criterion};
use std::fs;

use aoc_rust::day7;
fn day7(c: &mut Criterion) {
    let contents = fs::read_to_string("/home/gsus/.config/aoc_helper/2020/7.in").unwrap();
    c.bench_function("day7::part1", |b| b.iter(|| day7::part1(&contents)));
}

fn day11(c: &mut Criterion) {
    let contents = fs::read_to_string("/home/gsus/.config/aoc_helper/2020/11.in").unwrap();
    c.bench_function("day11::part1", |b| b.iter(|| day11::part1(&contents)));
}

use aoc_rust::day12;

fn day12(c: &mut Criterion) {
    let mut contents: &'static [u8] = include_bytes!("/home/gsus/.config/aoc_helper/2020/12.in");
    c.bench_function("day12::part1", |b| b.iter(|| day12::part1(&mut contents)));
    c.bench_function("day12::part2", |b| b.iter(|| day12::part2(&mut contents)));
}

// fn bench_part1(c: &mut Criterion) {
//     let contents = fs::read_to_string("/home/gsus/.config/aoc_helper/2020/11.in").unwrap();
//     c.bench_function("day11::part1", |b| b.iter(|| day11::part1(&contents)));
// }

// fn bench_fast(c: &mut Criterion) {
//     let contents = fs::read_to_string("/home/gsus/.config/aoc_helper/2020/11.in").unwrap();
//     c.bench_function("day11_faster::part1", |b| {
//         b.iter(|| day11_faster::part1_input(&contents))
//     });
// }

// criterion_group!(bench, bench_part1, bench_fast);
criterion_group!(bench, day7, day11, day12);
criterion_main!(bench);
