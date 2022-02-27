mod set;

fn main() {
    let deck = set::deck::new_unshuffled();

    for (ai, ca) in deck.iter().enumerate() {
        for (bi, cb) in deck[ai+1..].iter().enumerate() {
            for (_ci, cc) in deck[bi+1..].iter().enumerate() {
                if set::card::triplet(&ca, &cb, &cc) {
                    println!("Triplet found! {}  {}  {}", ca, cb, cc)
                }
            }
        }
    }

}
