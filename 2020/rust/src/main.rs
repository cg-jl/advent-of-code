use aoc_rust::day15;
fn main() {
    let contents: &'static [u8] = include_bytes!("../inputs/15.txt");
    println!("{}", day15::part2(&contents));
}
