fn main() {
    let mut f1 = 1u64;
    let mut f2 = 2u64;
    let mut sum = 0;

    while f1 <= 4000000 {
        if f1 % 2 == 0 {
            sum += f1;
        }
        let (f1next, f2next) = (f2, f1 + f2);
        f1 = f1next;
        f2 = f2next;
    }

    println!("{}", sum);
}
