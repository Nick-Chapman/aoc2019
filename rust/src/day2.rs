
use std::convert::TryInto;
use std::fs;

pub fn main() {

    let mut example1 = vec![1,9,10,3,2,3,11,0,99,30,40,50];
    run (&mut example1);
    println!("day2, part1(example1)   = {}", check (example1[0],3500));

    let full = read_intcode("../haskell/input/day2.input");
    println!("day2, part1(full)       = {}", check (part1 (&full,12,2),6627023));

    println!("day2, part2(full)       = {}", check (part2 (&full),4019));
}

fn check(got : i64, expected : i64) -> i64 {
   if got != expected {
       panic!("ensure failed: got: {}, expected: {}", got, expected);
   }
   got
}

fn read_intcode(filename : &str) -> Vec<i64> {
    let contents = fs::read_to_string(filename).expect(filename);
    let mut v = Vec::new();
    for x in contents.split(",") {
        v.push(x.parse().expect("read_intcode"))
    }
    v
}

fn part1(init : &[i64], noun : i64, verb : i64) -> i64 {
    let mut m = Vec::new();
    for x in init { m.push(*x) }
    m[1] = noun;
    m[2] = verb;
    run(&mut m);
    m[0]
}

fn part2(code : &[i64]) -> i64 {
    let goal = 19690720;
    for n in 0..99 {
        for v in 0..99 {
            let res = part1(code,n,v);
            if res == goal {
                return n*100 + v
            }
        }
    }
    panic!("part2, cant find answer!");
}

fn run(m : &mut Vec<i64>) {
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
