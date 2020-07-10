use std::collections::BTreeSet;

type Num = i64;

fn penta(n: Num) -> Num {
    n * (3 * n - 1) / 2
}

fn main() {
    let mut xs = BTreeSet::new();
    xs.insert(penta(1));
    xs.insert(penta(2));
    let mut i = 3;
    let (j, k) = 'outer: loop {
        let l = penta(i);
        for k in xs.iter() {
            let j = l - k;
            let i = k - j;
            if xs.contains(&j) && xs.contains(&i) {
                break 'outer (j, k);
            }
        }
        xs.insert(l);
        i += 1;
    };
    println!("{}", k - j);
}
