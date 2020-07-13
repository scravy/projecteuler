use std::collections::BTreeSet;
use std::collections::VecDeque;
use std::option::Option;

fn primes_upto(n: usize) -> BTreeSet<usize> {
    let mut ps = BTreeSet::new();
    ps.insert(2);
    let mut i = 3;
    while i < n {
        let mut is_prime = true;
        let t = (i as f64).sqrt() as usize;
        for &p in ps.iter() {
            if i % p == 0 {
                is_prime = false;
                break;
            }
            if p > t {
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

fn comp() -> Option<usize> {
    let m = 1000000;
    let ps = primes_upto(m);

    let mut s = 0;
    let mut t: VecDeque<usize> = VecDeque::new();

    let mut it = ps.iter().peekable();
    let mut ok = false;

    // TODO: This algorithm _does_ find the correct
    // result but is itself not correct, it works for
    // m=1000000 and m=1000 but it does, for example,
    // not work for m=100.
    loop {
        let &&p = it.peek()?;
        if s + p < m {
            t.push_back(p);
            s += p;
            it.next();
        } else {
            ok = true;
            s -= t.pop_front()?;
        }
        if ok && ps.contains(&s) {
            return Some(s);
        }
    }
}

fn main() {
    let res = comp();
    println!("{}", res.unwrap());
}
