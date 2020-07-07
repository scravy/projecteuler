use std::collections::BTreeSet;

fn f<F>(r: &mut BTreeSet<i32>, n: i32, p: i32, max: i32, c: &F) -> bool
where
    F: Fn(i32) -> bool,
{
    if p == max {
        return c(n);
    }
    for i in (1..=max).rev() {
        if r.contains(&i) {
            r.remove(&i);
            if f(r, n * 10 + i, p + 1, max, c) {
                return true;
            }
            r.insert(i);
        }
    }
    false
}

fn is_prime(n: i32) -> bool {
    for i in 2..n {
        if n % i == 0 {
            return false;
        }
    }
    true
}

fn main() {
    for n in (1..=9).rev() {
        let mut r = BTreeSet::new();
        for i in 1..=n {
            r.insert(i);
        }
        if f(&mut r, 0, 0, n, &|x| {
            if is_prime(x) {
                println!("{}", x);
                return true;
            }
            false
        }) {
            break;
        }
    }
}
