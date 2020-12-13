use lexical::parse_partial;
use std::f32;

#[inline(always)]
fn manhattan_distance(x: f32, y: f32) -> f32 {
    x.abs() + y.abs()
}

pub fn part1(input: &mut &[u8]) -> f32 {
    let mut x = 0f32;
    let mut y = 0f32;
    let mut angle = 0f32;
    let mut got = 0;

    while got < input.len() - 2 {
        let ch = input[got];
        let (num, ndigits) = parse_partial::<f32, _>(&input[got + 1..]).unwrap();
        got += ndigits + 2;
        match ch {
            b'N' => y -= num,
            b'S' => y += num,
            b'E' => x += num,
            b'W' => x -= num,
            b'L' => angle -= num * f32::consts::PI / 180f32,
            b'R' => angle += num * f32::consts::PI / 180f32,
            b'F' => {
                x += angle.cos() * num;
                y += angle.sin() * num;
            }
            _ => {}
        }
    }

    manhattan_distance(x, y).round()
}

#[inline(always)]
fn rotate(x: &mut f32, y: &mut f32, a: f32) {
    let s = a.sin();
    let c = a.cos();
    let _x = *x * c - *y * s;
    let _y = *x * s + *y * c;
    *x = _x;
    *y = _y;
}

pub fn part2(input: &mut &[u8]) -> f32 {
    let mut wx = 10f32;
    let mut wy = -1f32;
    let mut sx = 0f32;
    let mut sy = 0f32;
    let mut parse_got = 0;
    while parse_got < input.len() - 2 {
        let ch = input[parse_got];
        parse_got += 1;
        let (num, ndigits) = parse_partial::<f32, _>(&input[parse_got..]).unwrap();
        parse_got += ndigits + 1;

        match ch {
            b'N' => wy -= num,
            b'S' => wy += num,
            b'E' => wx += num,
            b'W' => wx -= num,
            b'R' => rotate(&mut wx, &mut wy, num * f32::consts::PI / 180f32),
            b'L' => rotate(&mut wx, &mut wy, -num * f32::consts::PI / 180f32),
            b'F' => {
                sx += wx * num;
                sy += wy * num;
            }
            _ => {}
        }
    }
    manhattan_distance(sx, sy).round()
}

#[cfg(test)]
mod tests {
    #[test]
    fn part1() {
        let mut b: &'static [u8] = include_bytes!("../inputs/day12.txt");
        assert_eq!(super::part1(&mut b), 25f32);
    }

    #[test]
    fn part2() {
        let mut b: &'static [u8] = include_bytes!("../inputs/day12.txt");
        assert_eq!(super::part2(&mut b), 286f32);
    }
}
