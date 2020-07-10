use std::collections::BTreeSet;

fn f<F>(r: &mut BTreeSet<i64>, n: i64, p: i64, c: &F) -> i64
where
    F: Fn(i64) -> i64,
{
    if p == 10 {
        return c(n);
    }
    let mut s = 0;
    for i in (0..=9).rev() {
        if r.contains(&i) {
            r.remove(&i);
            s += f(r, n * 10 + i, p + 1, c);
            r.insert(i);
        }
    }
    s
}

fn has_sub_string_divisibility_property(n: i64) -> bool {
    let d2d3d4 = n % 1000000000 / 1000000;
    if d2d3d4 % 2 != 0 {
        return false;
    }
    let d3d4d5 = n % 100000000 / 100000;
    if d3d4d5 % 3 != 0 {
        return false;
    }
    let d4d5d6 = n % 10000000 / 10000;
    if d4d5d6 % 5 != 0 {
        return false;
    }
    let d5d6d7 = n % 1000000 / 1000;
    if d5d6d7 % 7 != 0 {
        return false;
    }
    let d6d7d8 = n % 100000 / 100;
    if d6d7d8 % 11 != 0 {
        return false;
    }
    let d7d8d9 = n % 10000 / 10;
    if d7d8d9 % 13 != 0 {
        return false;
    }
    let d8d9d10 = n % 1000;
    if d8d9d10 % 17 != 0 {
        return false;
    }
    true
}

fn main() {
    let mut r = BTreeSet::new();
    for i in 0..=9 {
        r.insert(i);
    }
    let res = f(&mut r, 0, 0, &|x| {
        if has_sub_string_divisibility_property(x) {
            return x;
        }
        0
    });
    println!("{}", res);
}
