use std::collections::BTreeSet;

type Num = u64;

fn main() {
    let mut primes: BTreeSet<Num> = BTreeSet::new();
    let mut n: Num = 1;
    'outer: while primes.len() < 10001 {
        n += 1;
        let s = (n as f64).sqrt() as Num;
        for &p in primes.iter() {
            if p > s {
                break;
            }
            if n % p == 0 {
                continue 'outer;
            }
        }
        primes.insert(n);
    }
    println!("{}", n);
}
