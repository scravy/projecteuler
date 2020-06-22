use std::cmp::max;
use std::collections::BTreeMap;

type Num = u32;

fn main() {
    let mut factors: BTreeMap<Num, Num> = BTreeMap::new();
    for i in 1..=20 {
        merge_into(factorize(i), &mut factors);
    }
    let mut r: Num = 1;
    for (k, v) in factors {
        r *= k.pow(v);
    }
    println!("{:?}", r);
}

fn merge_into(map: BTreeMap<Num, Num>, into: &mut BTreeMap<Num, Num>) {
    for (k, v) in map {
        let value = match into.get(&k) {
            Some(&value) => max(value, v),
            _ => v,
        };
        into.insert(k, value);
    }
}

fn factorize(mut n: Num) -> BTreeMap<Num, Num> {
    let mut factors: BTreeMap<Num, Num> = BTreeMap::new();
    let mut f = 2;
    while n > 1 {
        if n % f == 0 {
            n /= f;
            *factors.entry(f).or_insert(0) += 1;
        } else {
            f += 1;
        }
    }
    factors
}
