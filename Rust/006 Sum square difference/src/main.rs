type Num = u64;

fn sum(upto: Num) -> Num {
    upto * (upto + 1) / 2
}

fn sum_squares(upto: Num) -> Num {
    upto * (upto + 1) * (2 * upto + 1) / 6
}

fn main() {
    let upto = 100;
    let r = sum(upto).pow(2) - sum_squares(upto);
    println!("{}", r);
}
