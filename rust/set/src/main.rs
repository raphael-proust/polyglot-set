#![allow(dead_code)]
use std::fmt;

fn triplet<T: std::cmp::PartialEq> (pa: &T, pb: &T, pc: &T) -> bool {
    (pa == pb && pb == pc) || (pa != pb && pb != pc && pa != pc)
}

macro_rules! cardprop {

    ($name: ident, $p0: ident, $p1: ident, $p2: ident) => {

        #[derive(PartialEq, Copy, Clone)]
        enum $name { $p0, $p1, $p2 }
        impl fmt::Display for $name {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                match self {
                    $name::$p0 => write!(f, stringify!($p0)),
                    $name::$p1 => write!(f, stringify!($p1)),
                    $name::$p2 => write!(f, stringify!($p2)),
                }
            }
        }

    };

}

cardprop!(Shape, Wave, Pill, Diamond);
cardprop!(Fill, Empty, Partial, Full);
cardprop!(Colour, Red, Green, Purple);
cardprop!(Count, One, Two, Three);

struct Card {
    shape: Shape,
    fill: Fill,
    colour: Colour,
    count: Count,
}

fn triplet_card (ca : &Card, cb: &Card, cc: &Card) -> bool {
    triplet (&ca.shape, &cb.shape, &cc.shape)
    && triplet (&ca.fill, &cb.fill, &cc.fill)
    && triplet (&ca.colour, &cb.colour, &cc.colour)
    && triplet (&ca.count, &cb.count, &cc.count)
}

impl fmt::Display for Card {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}:{}:{}", self.shape, self.fill, self.colour, self.count)
    }
}

fn main() {
    let shapes: [Shape; 3] = [Shape::Wave, Shape::Pill, Shape::Diamond ];
    let fills: [Fill; 3] = [Fill::Empty, Fill::Partial, Fill::Full ];
    let colours: [Colour; 3] = [Colour::Red, Colour::Green, Colour::Purple ];
    let counts: [Count; 3] = [Count::One, Count::Two, Count::Three ];
    let mut all_cards: Vec<Card> = vec![];
    for shape in shapes.iter() {
        for fill in fills.iter() {
            for colour in colours.iter() {
                for count in counts.iter() {
                    all_cards.push(Card { shape: *shape, fill: *fill, colour: *colour, count: *count })
                }
            }
        }
    }

    for (ai, ca) in all_cards.iter().enumerate() {
        for (bi, cb) in all_cards[ai+1..].iter().enumerate() {
            for (_ci, cc) in all_cards[bi+1..].iter().enumerate() {
                if triplet_card(&ca, &cb, &cc) {
                    println!("Triplet found! {}  {}  {}", ca, cb, cc)
                }
            }
        }
    }

}
