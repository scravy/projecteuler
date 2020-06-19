fn main() {
    let mut n: u64 = 600851475143;
    let mut i: u64 = 2;

    let r = loop {
        if n % i == 0 {
            n /= i;
        } else {
            i += 1;
            continue;
        }
        if n == 1 {
            break i;
        }
    };
    println!("{}", r);
}
