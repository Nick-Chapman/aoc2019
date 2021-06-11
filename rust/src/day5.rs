
use std::convert::TryInto;
use std::fs;

pub fn main() {
    let full = read_intcode("../haskell/input/day5.input");
    println!("day5, part1 = {}", check (part1 (&full),2845163));
    println!("day5, part2 = {}", check (part2 (&full),9436229));
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

fn part1(code : &[i64]) -> i64 {
    let results = run(code, 1);
    //println!("Results:{:?}",results);
    *results.last().unwrap()
}

fn part2(code : &[i64]) -> i64 {
    let results = run(code, 5);
    //println!("Results:{:?}",results);
    *results.last().unwrap()
}

enum Op { Add, Mul, Input, Output, JumpNZ, JumpZ, LessThan, Equals, Halt }

fn decode_op(x : i64) -> Op {
    match x {
        1 => Op::Add,
        2 => Op::Mul,
        3 => Op::Input,
        4 => Op::Output,
        5 => Op::JumpNZ,
        6 => Op::JumpZ,
        7 => Op::LessThan,
        8 => Op::Equals,
        99 => Op::Halt,
        _ => panic!("unknown Op: {}", x)
    }
}

enum Mode { Positional, Immediate }

fn decode_mode(x : i64) -> Mode {
    match x {
        0 => Mode::Positional,
        1 => Mode::Immediate,
        _ => panic!("unknown Mode: {}", x)
    }
}

fn run(code : &[i64], the_input : i64) -> Vec<i64> {
    let mut mem = Vec::new();
    let mut ip : usize = 0;
    let mut output = Vec::new();

    for x in code { mem.push(*x) }

    loop {

        let read = |off : usize, mode : Mode| -> i64 {
            match mode {
                Mode::Positional => {
                    let off2 : usize = mem[off].try_into().expect("read");
                    return mem[off2]
                }
                Mode::Immediate => {
                    return mem[off]
                }
            }
        };

        let write = |mem : &mut Vec<i64>, off : usize, value : i64| {
            let t : usize = mem[off].try_into().expect("write");
            mem[t] = value;
        };

        let op_code = mem[ip];
        let op = decode_op(op_code % 100);
        let mode1 = decode_mode (op_code / 100 % 10);
        let mode2 = decode_mode (op_code / 1000 % 10);
        //println!("Mem:{:?}",m);
        match op {
            Op::Add => {
                let a = read(ip+1,mode1);
                let b = read(ip+2,mode2);
                let res = a + b;
                write(&mut mem, ip+3,res);
                ip += 4;
            }
            Op::Mul => {
                let a = read(ip+1,mode1);
                let b = read(ip+2,mode2);
                let res = a * b;
                write(&mut mem,ip+3,res);
                ip += 4;
            }
            Op::Input => {
                write(&mut mem,ip+1,the_input);
                ip += 2;
            }
            Op::Output => {
                let a = read(ip+1,mode1);
                output.push(a);
                ip += 2;
            }
            Op::JumpNZ => {
                let a = read(ip+1,mode1);
                if a != 0 {
                    let dest : usize = read(ip+2,mode2).try_into().expect("jumpNZ");
                    ip = dest;
                } else {
                    ip += 3;
                }
            }
            Op::JumpZ => {
                let a = read(ip+1,mode1);
                if a == 0 {
                    let dest : usize = read(ip+2,mode2).try_into().expect("jumpZ");
                    ip = dest;
                } else {
                    ip += 3;
                }
            }
            Op::LessThan => {
                let a = read(ip+1,mode1);
                let b = read(ip+2,mode2);
                let res = if a < b { 1 } else { 0 };
                write(&mut mem,ip+3,res);
                ip += 4;
            }
            Op::Equals => {
                let a = read(ip+1,mode1);
                let b = read(ip+2,mode2);
                let res = if a == b { 1 } else { 0 };
                write(&mut mem,ip+3,res);
                ip += 4;
            }
            Op::Halt => {
                break
            }
        }
    }
    output
}
