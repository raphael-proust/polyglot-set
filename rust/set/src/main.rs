pub mod card_prop {
    use std::fmt;

    pub fn triplet<T: std::cmp::PartialEq> (pa: &T, pb: &T, pc: &T) -> bool {
        (pa == pb && pb == pc) || (pa != pb && pb != pc && pa != pc)
    }

    macro_rules! makeprop {

        ($name: ident, $p0: ident, $p1: ident, $p2: ident) => {

            #[derive(PartialEq, Clone)]
            pub enum $name { $p0, $p1, $p2 }
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

    makeprop!(Shape, Wave, Pill, Diamond);
    makeprop!(Fill, Empty, Partial, Full);
    makeprop!(Colour, Red, Green, Purple);
    makeprop!(Count, One, Two, Three);

}

pub mod card {
    use std::fmt;
    use crate::card_prop;

    pub struct Card {
        shape: card_prop::Shape,
        fill: card_prop::Fill,
        colour: card_prop::Colour,
        count: card_prop::Count,
    }
    impl Card {
        pub fn new(shape: card_prop::Shape, fill: card_prop::Fill, colour: card_prop::Colour, count: card_prop::Count) -> Card {
            Card { shape, fill, colour, count, }
        }
    }

    pub fn triplet (ca : &Card, cb: &Card, cc: &Card) -> bool {
        card_prop::triplet (&ca.shape, &cb.shape, &cc.shape)
            && card_prop::triplet (&ca.fill, &cb.fill, &cc.fill)
            && card_prop::triplet (&ca.colour, &cb.colour, &cc.colour)
            && card_prop::triplet (&ca.count, &cb.count, &cc.count)
    }

    impl fmt::Display for Card {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            write!(f, "{}:{}:{}:{}", self.shape, self.fill, self.colour, self.count)
        }
    }

}

fn main() {
    let shapes: [card_prop::Shape; 3] = [card_prop::Shape::Wave, card_prop::Shape::Pill, card_prop::Shape::Diamond ];
    let fills: [card_prop::Fill; 3] = [card_prop::Fill::Empty, card_prop::Fill::Partial, card_prop::Fill::Full ];
    let colours: [card_prop::Colour; 3] = [card_prop::Colour::Red, card_prop::Colour::Green, card_prop::Colour::Purple ];
    let counts: [card_prop::Count; 3] = [card_prop::Count::One, card_prop::Count::Two, card_prop::Count::Three ];
    let mut all_cards: Vec<card::Card> = vec![];
    for shape in shapes.iter() {
        for fill in fills.iter() {
            for colour in colours.iter() {
                for count in counts.iter() {
                    all_cards.push(card::Card::new( shape.clone(), fill.clone(), colour.clone(), count.clone() ))
                }
            }
        }
    }

    for (ai, ca) in all_cards.iter().enumerate() {
        for (bi, cb) in all_cards[ai+1..].iter().enumerate() {
            for (_ci, cc) in all_cards[bi+1..].iter().enumerate() {
                if card::triplet(&ca, &cb, &cc) {
                    println!("Triplet found! {}  {}  {}", ca, cb, cc)
                }
            }
        }
    }

}
