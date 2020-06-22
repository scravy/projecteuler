fn rev(mut n: u64) -> u64 {
    let mut r = 0;
    while n > 0 {
        r = r * 10 + n % 10;
        n /= 10;
    }
    r
}

fn ispalindrome(n: u64) -> bool {
    n == rev(n)
}

fn main() {
    let mut r = 0;
    for i in (100..1000).rev() {
        for j in (i..1000).rev() {
            let n = i * j;
            if n > r {
                if ispalindrome(n) {
                    r = n;
                }
            }
        }
    }
    println!("{}", r);
}
