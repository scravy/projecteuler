fn main() {
    let path = match std::env::args().nth(1) {
        None => std::process::exit(1),
        Some(p) => p,
    };
    let data = match std::fs::read_to_string(&path) {
        Err(err) => {
            eprintln!("Failed reading {}: {}", path, err);
            std::process::exit(1)
        }
        Ok(d) => d,
    };
    let mut res = 0;
    for word in data.split(",") {
        let mut s = 0;
        for c in word.trim_start_matches('"').trim_end_matches('"').chars() {
            s += c as u32 - 'A' as u32 + 1;
        }
        let r = ((8 * s + 1) as f64).sqrt();
        if r as u32 as f64 == r {
            res += 1;
        }
    }
    println!("{}", res);
}
