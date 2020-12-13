use aoc_rust::day12;
fn main() {
    let mut contents: &'static [u8] = include_bytes!("/home/gsus/.config/aoc_helper/2020/12.in");
    println!("{}", day12::part1(&mut contents));
}
