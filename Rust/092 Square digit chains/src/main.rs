use std::collections::HashMap;

fn next(mut n: usize) -> usize {
    let mut m = 0;
    while n > 0 {
        m += (n % 10).pow(2);
        n /= 10;
    }
    m
}

fn judge(map: &mut HashMap<usize, usize>, n: usize) -> usize {
    match map.get(&n) {
        Some(&v) => v,
        None => {
            let m = next(n);
            let r = judge(map, m);
            map.insert(n, r);
            r
        },
    }
}

fn main() {
    let mut map = HashMap::new();

    map.insert(1, 1);
    map.insert(89, 89);

    let mut count = 0;
    for i in 1..10000000 {
        if judge(&mut map, i) == 89 {
            count += 1;
        }
    }
    println!("{}", count);
}
