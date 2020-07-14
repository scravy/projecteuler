use std::string::String;

fn read_roman_numeral(n: &str) -> usize {
    let mut max = 0;
    let mut sum = 0;
    for c in n.chars().rev() {
        let v = match c {
            'I' => 1,
            'V' => 5,
            'X' => 10,
            'L' => 50,
            'C' => 100,
            'D' => 500,
            'M' => 1000,
            _ => std::process::exit(2),
        };
        if v > max {
            max = v;
        }
        if v < max {
            sum -= v;
        } else {
            sum += v;
        }
    }
    sum
}

fn write_roman_numeral(mut n: usize) -> String {
    let mut s = String::new();
    loop {
        if n >= 1000 {
            n -= 1000;
            s.push_str("M");
            continue;
        }
        if n >= 900 {
            n -= 900;
            s.push_str("CM");
            continue;
        }
        if n >= 500 {
            n -= 500;
            s.push_str("D");
            continue;
        }
        if n >= 400 {
            n -= 400;
            s.push_str("CD");
            continue;
        }
        if n >= 100 {
            n -= 100;
            s.push_str("C");
            continue;
        }
        if n >= 90 {
            n -= 90;
            s.push_str("XC");
            continue;
        }
        if n >= 50 {
            n -= 50;
            s.push_str("L");
            continue;
        }
        if n >= 40 {
            n -= 40;
            s.push_str("CL");
            continue;
        }
        if n >= 10 {
            n -= 10;
            s.push_str("X");
            continue;
        }
        if n >= 9 {
            n -= 9;
            s.push_str("IX");
            continue;
        }
        if n >= 5 {
            n -= 5;
            s.push_str("V");
            continue;
        }
        if n >= 4 {
            n -= 4;
            s.push_str("IV");
            continue;
        }
        if n >= 1 {
            n -= 1;
            s.push_str("I");
            continue;
        }
        break s;
    }
}

fn main() {
    let path = match std::env::args().nth(1) {
        None => std::process::exit(1),
        Some(p) => p,
    };
    let data = match std::fs::read_to_string(&path) {
        Err(err) => {
            eprintln!("Failed reading {}: {}", path, err);
            std::process::exit(1)
        }
        Ok(d) => d,
    };
    let mut count_chars = 0;
    let mut count_chars_optimized = 0;
    for line in data.lines() {
        count_chars += line.len();
        let v = read_roman_numeral(line);
        count_chars_optimized += write_roman_numeral(v).len();
    }
    println!("{}", count_chars - count_chars_optimized);
}
