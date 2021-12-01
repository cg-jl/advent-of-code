use std::io::{self, BufRead, BufReader};

fn count_greater(iter: impl IntoIterator<Item = u32>) -> usize {
    let mut count = 0;
    let mut last = None;
    for x in iter {
        if let Some(last) = last {
            if last < x {
                count += 1;
            }
        }
        last = Some(x);
    }
    count
}

fn sum_of_triplets<'a>(items: &'a [u32]) -> impl Iterator<Item = u32> + 'a {
    (0..items.len().saturating_sub(2)).map(move |i| {
        items[i] + items[i + 1] + items[i + 2]
    })
}

fn main() -> io::Result<()> {
    let lines: Vec<u32> = std::fs::File::open("inputs/day1.txt")
        .map(BufReader::new)?
        .lines()
        .map(|line| line.map(|x| x.parse().unwrap())).collect::<Result<_, _>>()?;

    println!("Part 1:");
    println!("{}", count_greater(lines.iter().cloned()));
    println!("Part 2:");
    println!("{}", count_greater(sum_of_triplets(&lines)));
    Ok(())
}
