fn main() {
    let mut s = String::with_capacity(1000010);
    let mut i = 1;
    while s.len() < 1000000 {
        s.push_str(format!("{}", i).as_str());
        i += 1;
    }
    let c = s.as_str();
    let mut p = 1;
    for i in 0..7 {
        let ix = 10usize.pow(i as u32);
        let d = c[(ix - 1)..ix].chars().nth(0).unwrap();
        p *= d.to_digit(10).unwrap();
    }
    println!("{}", p);
}
