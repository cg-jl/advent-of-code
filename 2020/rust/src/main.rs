use aoc_rust::day13;
fn main() {
    let mut contents: &'static [u8] = include_bytes!("/home/gsus/.config/aoc_helper/2020/13.in");
    println!("{}", day13::part2_generic(&contents));
}
