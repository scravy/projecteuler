fn sum(multiple: i64, threshold: i64) -> i64 {
    let mut i = multiple;
    let mut sum = 0;
    while i < threshold {
        sum += i;
        i += multiple;
    }
    sum
}

fn main() {
    let t = 1000;
    let r = sum(3, t) + sum(5, t) - sum(15, t);

    println!("{}", r);
}
