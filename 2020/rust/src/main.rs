use aoc_rust::day14;
fn main() {
    let mut contents: &'static [u8] = include_bytes!("../../python/inputs/14.txt");
    println!("{}", day14::part2(&contents));
}
