
use std::io::BufReader;
use std::io::BufRead;
use std::fs::File;

pub fn main() {
   println!("day1, part1 = {}", check(part1(),3318632));
   println!("day1, part2 = {}", check(part2(),4975084));
}

fn check(got : i64, expected : i64) -> i64 {
   if got != expected {
       panic!("ensure failed: got: {}, expected: {}", got, expected);
   }
   got
}

fn part1() -> i64 {
   let file = File::open("../haskell/input/day1.input").expect("day1 input");
   let mut buffer = BufReader::new(file);
   let mut acc = 0;
   loop {
        let mut line = String::new();
        let n = buffer.read_line(&mut line).expect("read_line");
        if n == 0 { break }
        let n : i64 = line.trim().parse().expect("not an int");
        acc += calc1(n);
    }
    return acc;
}

fn part2() -> i64 {
   let file = File::open("../haskell/input/day1.input").expect("day1 input");
   let mut buffer = BufReader::new(file);
   let mut acc = 0;
   loop {
        let mut line = String::new();
        let n = buffer.read_line(&mut line).expect("read_line");
        if n == 0 { break }
        let n : i64 = line.trim().parse().expect("not an int");
        acc += calc2(n);
    }
    return acc;
}

fn calc1(x : i64) -> i64 {
   x / 3 - 2
}

fn calc2(x : i64) -> i64 {
   let y = calc1(x);
   if y>0 {
      return y + calc2(y)
   } else {
      return 0
   }
}
