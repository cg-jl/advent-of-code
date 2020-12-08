use aoc_rust::day8;
use std::fs;
fn main() {
    let contents = fs::read_to_string("/home/gsus/.config/aoc_helper/2020/8.test.in").unwrap();
    println!("{}", day8::part2(&contents));
}
