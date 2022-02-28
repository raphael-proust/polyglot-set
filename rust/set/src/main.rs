mod set;
use rand;
use rand::SeedableRng;

fn main() {
    let mut rng = rand::rngs::SmallRng::seed_from_u64(8436593275u64);
    let deck = set::deck::new(&mut rng);

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
