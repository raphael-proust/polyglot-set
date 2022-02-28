pub mod prop {
    use std::fmt;
    pub fn triplet<T: std::cmp::PartialEq> (pa: &T, pb: &T, pc: &T) -> bool {
        (pa == pb && pb == pc) || (pa != pb && pb != pc && pa != pc)
    }

    macro_rules! makeprop {

        ($name: ident, $p0: ident, $s0: literal, $p1: ident, $s1: literal, $p2: ident, $s2: literal) => {

            #[derive(PartialEq, Clone)]
            pub enum $name { $p0, $p1, $p2 }
            impl fmt::Display for $name {
                fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                    match self {
                        $name::$p0 => write!(f, $s0),
                        $name::$p1 => write!(f, $s1),
                        $name::$p2 => write!(f, $s2),
                    }
                }
            }

        };

    }

    makeprop!(Shape, Wave, "~", Pill, "o", Diamond, "⋄" );
    makeprop!(Fill, Empty, "░", Partial, "▒", Full, "▓");
    makeprop!(Colour, Red, "R", Green, "G", Purple, "P"  );
    makeprop!(Count, One, "⠄", Two, "⠢", Three, "⠦");
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
    use rand::prelude::SliceRandom;
    use std::convert::TryInto;

    const DECKSIZE: usize = 3*3*3*3;

    pub type Deck = [super::card::Card; DECKSIZE];

    pub fn new<R: rand::Rng>(rng: &mut R) -> Deck {
        let shapes: [super::prop::Shape; 3] = [super::prop::Shape::Wave, super::prop::Shape::Pill, super::prop::Shape::Diamond ];
        let fills: [super::prop::Fill; 3] = [super::prop::Fill::Empty, super::prop::Fill::Partial, super::prop::Fill::Full ];
        let colours: [super::prop::Colour; 3] = [super::prop::Colour::Red, super::prop::Colour::Green, super::prop::Colour::Purple ];
        let counts: [super::prop::Count; 3] = [super::prop::Count::One, super::prop::Count::Two, super::prop::Count::Three ];
        let mut v: Vec<super::card::Card> = vec![];
        for shape in shapes.iter() {
            for fill in fills.iter() {
                for colour in colours.iter() {
                    for count in counts.iter() {
                        v.push(super::card::Card { shape: shape.clone(), fill: fill.clone(), colour: colour.clone(), count: count.clone() })
                    }
                }
            }
        };
        v.shuffle(rng);
        match v.try_into() {
            Ok(a) => return a,
            Err(_) => std::process::exit(1),
        };
    }

}

pub mod table {

    pub struct Table {
        // A table is:
        // - a set of guessed triplets (all the cards in `cards[0..guessed]`),
        // - a set of already drawn cards (all cards in `cards[guessed..drawn]`),
        // - a draw pile of not yet drawn cards (all cards in `cards[drawn..]`).
        pub cards: super::deck::Deck,
        pub guessed: u8,
        pub drawn: u8,
    }

    //TODO:
    // - draw: increment drawn field (return bool)
    // - guess: finds a triplet of indices in the range [guessed..drawn] then move those cards to
    // the indices`[guessed, guessed+1, guessed+2], then increment the guessed field (return bool)

}
