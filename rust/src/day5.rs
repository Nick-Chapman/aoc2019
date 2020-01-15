
use std::convert::TryInto;
use std::fs;

pub fn main() {
    let full = read_intcode("../input/day5.input");
    println!("day5, part1 = {}", check (part1 (&full),2845163))
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

fn part1(init : &[i64]) -> i64 {
    let mut m = Vec::new();
    for x in init { m.push(*x) }
    let results = run(&mut m, 1);
    //println!("Results:{:?}",results);
    *results.last().unwrap()
}

enum Op { Add, Mul, Input, Output, Halt }

fn decode_op(x : i64) -> Op {
    match x {
        1 => Op::Add,
        2 => Op::Mul,
        3 => Op::Input,
        4 => Op::Output,
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

fn run(mem : &mut Vec<i64>, the_input : i64) -> Vec<i64> {
    let mut ip : usize = 0;
    let mut output = Vec::new();

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
                write(mem,ip+3,res);
                ip += 4;
            }
            Op::Mul => {
                let a = read(ip+1,mode1);
                let b = read(ip+2,mode2);
                let res = a * b;
                write(mem,ip+3,res);
                ip += 4;
            }
            Op::Input => {
                write(mem,ip+1,the_input);
                ip += 2;
            }
            Op::Output => {
                let a = read(ip+1,mode1);
                output.push(a);
                ip += 2;
            }
            Op::Halt => {
                break
            }
        }
    }
    output
}
