use std::collections::BTreeSet;

fn main() {
    reset_signal_pipe_handler();
    let mut ps = BTreeSet::new();
    ps.insert(2);
    let mut i = 3;
    let mut x1;
    let mut x2 = BTreeSet::new();
    let mut x3 = BTreeSet::new();
    let mut x4 = BTreeSet::new();
    let res = loop {
        let mut p = i;
        let mut fs = BTreeSet::new();
        let mut it = ps.iter();
        let mut f = *it.next().expect("never fails");
        'factorization: while p > 1 {
            if p % f == 0 {
                p /= f;
                fs.insert(f);
            } else {
                f = match it.next() {
                    Some(&x) => x,
                    None => break 'factorization,
                };
            }
        }
        if fs.is_empty() {
            ps.insert(p);
        }
        x1 = x2;
        x2 = x3;
        x3 = x4;
        x4 = fs;
        if x1.len() == 4 && x2.len() == 4 && x3.len() == 4 && x4.len() == 4 {
            if x1 != x2 && x2 != x3 && x3 != x4 {
                break i - 3;
            }
        }
        i += 1;
    };
    println!("{}", res);
}

pub fn reset_signal_pipe_handler() {
    #[cfg(target_family = "unix")]
    {
        use nix::sys::signal;

        let _err = unsafe { signal::signal(signal::Signal::SIGPIPE, signal::SigHandler::SigDfl) };
    }
}
