pub mod prop {
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

    pub struct Card {
        pub shape: super::prop::Shape,
        pub fill: super::prop::Fill,
        pub colour: super::prop::Colour,
        pub count: super::prop::Count,
    }

    pub fn triplet (ca : &Card, cb: &Card, cc: &Card) -> bool {
        super::prop::triplet (&ca.shape, &cb.shape, &cc.shape)
            && super::prop::triplet (&ca.fill, &cb.fill, &cc.fill)
            && super::prop::triplet (&ca.colour, &cb.colour, &cc.colour)
            && super::prop::triplet (&ca.count, &cb.count, &cc.count)
    }

    impl fmt::Display for Card {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            write!(f, "{}:{}:{}:{}", self.shape, self.fill, self.colour, self.count)
        }
    }

}

pub mod deck {
    use rand;
    use rand::prelude::SliceRandom;

    pub type Deck = Vec<super::card::Card>;
    pub fn new_unshuffled() -> Deck {
        let shapes: [super::prop::Shape; 3] = [super::prop::Shape::Wave, super::prop::Shape::Pill, super::prop::Shape::Diamond ];
        let fills: [super::prop::Fill; 3] = [super::prop::Fill::Empty, super::prop::Fill::Partial, super::prop::Fill::Full ];
        let colours: [super::prop::Colour; 3] = [super::prop::Colour::Red, super::prop::Colour::Green, super::prop::Colour::Purple ];
        let counts: [super::prop::Count; 3] = [super::prop::Count::One, super::prop::Count::Two, super::prop::Count::Three ];
        let mut all_cards: Deck = vec![];
        for shape in shapes.iter() {
            for fill in fills.iter() {
                for colour in colours.iter() {
                    for count in counts.iter() {
                        all_cards.push(super::card::Card { shape: shape.clone(), fill: fill.clone(), colour: colour.clone(), count: count.clone() })
                    }
                }
            }
        };
        return all_cards;
    }

    pub fn shuffle<R: rand::Rng>(d: &mut Deck, rng: &mut R) {
        d.shuffle(rng);
    }

    pub fn draw(d: &mut Deck) -> Option<super::card::Card> {
        return d.pop();
    }

}
