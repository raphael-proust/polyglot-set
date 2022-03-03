mod set;
use rand;
use rand::SeedableRng;

fn main() {
    let mut rng = rand::rngs::SmallRng::seed_from_u64(8436593275u64);
    let mut table = set::table::Table::new(&mut rng);

    if table.triplet() {
        println!("Found triplet: {} {} {}", table.cards[0], table.cards[1], table.cards[2])
    }
    while table.draw() {
        println!("Draw");
        if table.triplet() {
            println!("Found triplet: {} {} {}", table.cards[0], table.cards[1], table.cards[2])
        }
    }
    while table.triplet() {
        println!("Found triplet: {} {} {}", table.cards[0], table.cards[1], table.cards[2])
    }
}
