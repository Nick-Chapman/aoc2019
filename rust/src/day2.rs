
use std::convert::TryInto;

pub fn main() {

    let mut example1 = [1,9,10,3,2,3,11,0,99,30,40,50];
    println!("day2, part1(example1) = {}", check (part1 (&mut example1),3500));

    let mut full = read_input();
    full[1] = 12;
    full[2] = 2;
    println!("day2, part1(full)     = {}", check (part1 (&mut full),6627023));
}

fn read_input() -> [i64;169] {
[1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,9,1,19,1,9,19,23,1,23,5,27,2,27,10,31,1,6,31,35,1,6,35,39,2,9,39,43,1,6,43,47,1,47,5,51,1,51,13,55,1,55,13,59,1,59,5,63,2,63,6,67,1,5,67,71,1,71,13,75,1,10,75,79,2,79,6,83,2,9,83,87,1,5,87,91,1,91,5,95,2,9,95,99,1,6,99,103,1,9,103,107,2,9,107,111,1,111,6,115,2,9,115,119,1,119,6,123,1,123,9,127,2,127,13,131,1,131,9,135,1,10,135,139,2,139,10,143,1,143,5,147,2,147,6,151,1,151,5,155,1,2,155,159,1,6,159,0,99,2,0,14,0]
}

fn check(got : i64, expected : i64) -> i64 {
   if got != expected {
       panic!("ensure failed: got: {}, expected: {}", got, expected);
   }
   got
}

fn part1(m : &mut[i64]) -> i64 {
    let mut ip = 0;
    loop {
        //println!("Mem:{:?}",m);
        match decode(m[ip]) {
            Op::Add => {
                let a : usize = m[ip+1].try_into().unwrap();
                let b : usize = m[ip+2].try_into().unwrap();
                let t : usize = m[ip+3].try_into().unwrap();
                m[t] = m[a] + m[b];
                ip += 4;
            }
            Op::Mul => {
                let a : usize = m[ip+1].try_into().unwrap();
                let b : usize = m[ip+2].try_into().unwrap();
                let t : usize = m[ip+3].try_into().unwrap();
                m[t] = m[a] * m[b];
                ip += 4;
            }
            Op::Halt => {
                break
            }
        }
    }
    m[0]
}

fn decode(x : i64) -> Op {
    let _ = Op::Add;
    match x {
        1 => Op::Add,
        2 => Op::Mul,
        99 => Op::Halt,
        _ => panic!("unknown opcode: {}", x)
    }
}

enum Op {
    Add,
    Mul,
    Halt
}
