fn main() {
    let mut pmax = 0;
    let mut pmaxval = 0;
    for p in 3..=1000 {
        let mut count = 0;
        for a in 1..p {
            let a2 = a*a;
            for b in a..=((p-a)/2) {
                let c = p - a - b;
                if a2 + b*b == c*c {
                    count += 1;
                }
            }
        }
        if count > pmaxval {
            pmax = p;
            pmaxval = count;
        }
    }
    println!("{}", pmax);
}
