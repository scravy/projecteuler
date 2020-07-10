use std::collections::BTreeSet;
use std::string::String;

fn f<F>(r: &mut BTreeSet<i32>, n: i32, p: i32, c: &F) -> bool
where
    F: Fn(i32) -> bool,
{
    if p == 9 {
        return c(n);
    }
    for i in (1..=9).rev() {
        if r.contains(&i) {
            r.remove(&i);
            if f(r, n * 10 + i, p + 1, c) {
                return true;
            }
            r.insert(i);
        }
    }
    false
}

fn is_pandigital_multiple(num: i32) -> bool {
    let mut scale = 100000000;
    while scale > 1 {
        let n1 = num / scale;
        let mut s = String::from(format!("{}", n1));
        let mut n = n1;
        while s.len() < 9 {
            n += n1;
            s.push_str(format!("{}", n).as_str());
        }
        if s.as_str() == format!("{}", num) {
            return true;
        }
        scale /= 10;
    }
    false
}

fn main() {
    let mut r = BTreeSet::new();
    for i in 1..=9 {
        r.insert(i);
    }
    f(&mut r, 0, 0, &|x| {
        if is_pandigital_multiple(x) {
            println!("{}", x);
            return true;
        }
        false
    });
}
