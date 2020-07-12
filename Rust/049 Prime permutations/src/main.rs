use std::collections::BTreeSet;

fn primes_upto(n: i64) -> BTreeSet<i64> {
    let mut ps = BTreeSet::new();
    ps.insert(2);
    let mut i = 3;
    while i < n {
        let mut is_prime = true;
        let t = (i as f64).sqrt() as i64;
        for p in ps.iter() {
            if i % p == 0 {
                is_prime = false;
                break;
            }
            if *p > t {
                break;
            }
        }
        if is_prime {
            ps.insert(i);
        }
        i += 2;
    }
    ps
}

fn is_permutation_of(mut a: i64, mut b: i64) -> bool {
    let mut pa = [0; 10];
    let mut pb = [0; 10];
    while a > 0 {
        pa[(a % 10) as usize] += 1;
        pb[(b % 10) as usize] += 1;
        a /= 10;
        b /= 10;
    }
    pa == pb
}

fn main() {
    let ps = primes_upto(10000)
        .into_iter()
        .skip_while(|&p| p < 1000)
        .collect::<BTreeSet<_>>();

    for &p2 in ps.iter() {
        for &p3 in ps.iter().rev().take_while(|&p| p > &p2) {
            if !is_permutation_of(p2, p3) {
                continue;
            }
            let d = p3 - p2;
            let p1 = p2 - d;
            if !ps.contains(&p1) {
                continue;
            }
            if !is_permutation_of(p1, p2) {
                continue;
            }
            println!("{} {} {} {}", d, p1, p2, p3);
        }
    }
}
