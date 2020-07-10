type Num = u64;

fn main() {
    let mut tri: Num = 1;
    let mut tri_ix: Num = 1;
    let mut tri_diff: Num = 2;
    let mut penta: Num = 1;
    let mut penta_ix: Num = 1;
    let mut penta_diff: Num = 4;
    let mut hexa: Num = 1;
    let mut hexa_ix: Num = 1;
    let mut hexa_diff: Num = 5;

    let mut count = 0;

    let result = loop {
        if tri == penta && penta == hexa {
            println!("T_{} = P_{} = H_{} = {}", tri_ix, penta_ix, hexa_ix, tri);
            count += 1;
            if count == 3 {
                break tri;
            }
        }
        if tri <= penta && tri <= hexa {
            tri += tri_diff;
            tri_ix += 1;
            tri_diff += 1;
        } else if penta <= tri && penta <= hexa {
            penta += penta_diff;
            penta_ix += 1;
            penta_diff += 3;
        } else if hexa <= tri && hexa <= penta {
            hexa += hexa_diff;
            hexa_ix += 1;
            hexa_diff += 4;
        }
    };
    println!("{}", result);
}
