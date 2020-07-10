use std::collections::BTreeSet;

fn main() {
    let mut ps = BTreeSet::new();
    let mut i = 3;
    ps.insert(2);
    ps.insert(3);
    let res = 'outer: loop {
        i += 2;
        let mut is_prime = true;
        for p in ps.iter() {
            if i % p == 0 {
                is_prime = false;
                break;
            }
        }
        if is_prime {
            ps.insert(i);
        } else {
            for p in ps.iter() {
                let x = ((i - p) as f64 / 2.0).sqrt();
                if (x as i64) as f64 == x {
                    continue 'outer;
                }
            }
            break i;
        }
    };
    println!("{}", res);
}
