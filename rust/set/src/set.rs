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
            impl $name {
                pub fn all() -> [$name; 3] {
                    return [$name::$p0, $name::$p1, $name::$p2];
                }
                pub fn third(x: &$name, y: &$name) -> $name {
                    if x == y {
                        return x.clone()
                    } else {
                        let mut has0 = false;
                        let mut has1 = false;
                        let mut has2 = false;
                        match x {
                            $name::$p0 => has0 = true,
                            $name::$p1 => has1 = true,
                            $name::$p2 => has2 = true,
                        };
                        match y {
                            $name::$p0 => has0 = true,
                            $name::$p1 => has1 = true,
                            $name::$p2 => has2 = true,
                        };
                        if !has0 { return $name::$p0 }
                        else if !has1 { return $name::$p1 }
                        else if !has2 { return $name::$p2 }
                        else { panic!("cannot find third prop for {} and {}", x, y) }
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

    pub fn third (ca: &Card, cb: &Card) -> impl Fn(&Card) -> bool {
        let shape = super::prop::Shape::third(&ca.shape, &cb.shape);
        let fill = super::prop::Fill::third(&ca.fill, &cb.fill);
        let colour = super::prop::Colour::third(&ca.colour, &cb.colour);
        let count = super::prop::Count::third(&ca.count, &cb.count);
        move |cc: &Card| cc.shape == shape && cc.fill == fill && cc.colour == colour && cc.count == count
    }

    impl fmt::Display for Card {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            write!(f, "{}:{}:{}:{}", self.shape, self.fill, self.colour, self.count)
        }
    }

}

const DECKSIZE: usize = 3*3*3*3;

pub mod deck {
    use rand::prelude::SliceRandom;
    use std::convert::TryInto;

    pub type Deck = [super::card::Card; super::DECKSIZE];

    pub fn new<R: rand::Rng>(rng: &mut R) -> Deck {
        let mut v: Vec<super::card::Card> = vec![];
        for shape in super::prop::Shape::all().iter() {
            for fill in super::prop::Fill::all().iter() {
                for colour in super::prop::Colour::all().iter() {
                    for count in super::prop::Count::all().iter() {
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
        pub guessed: usize,
        pub drawn: usize,
    }

    impl Table {
        pub fn new<R: rand::Rng>(rng: &mut R) -> Table {
            let cards: super::deck::Deck = super::deck::new(rng);
            let guessed: usize = 0;
            let drawn: usize = 12;
            return Table {cards, guessed, drawn}
        }

        pub fn draw (&mut self) -> bool {
            if self.drawn < super::DECKSIZE {
                self.drawn = self.drawn + 3;
                return true
            } else {
                return false;
            }
        }

        fn triplet_indices (&mut self) -> Option<(usize, usize, usize)> {
            for (ia, ca) in self.cards[self.guessed .. (self.drawn - 2)].iter().enumerate() {
                for (ib, cb) in self.cards[(ia+1) .. (self.drawn - 1)].iter().enumerate() {
                    let third = super::card::third(&ca, &cb);
                    for (ic, cc) in self.cards[(ib+1) .. self.drawn].iter().enumerate() {
                        if third(&cc) {
                            return Some((ia, ib, ic))
                        }
                    }
                }
            }
            return None
        }

        pub fn triplet (&mut self) -> bool {
            if self.guessed == self.drawn { return false; }
            if self.guessed + 1 == super::DECKSIZE { return false; }
            match self.triplet_indices() {
                None => return false,
                Some((indexa, indexb, indexc)) => {
                    self.cards.swap(indexa, self.guessed);
                    self.cards.swap(indexb, self.guessed+1);
                    self.cards.swap(indexc, self.guessed+2);
                    self.guessed = self.guessed + 3;
                    return true;
                },
            }
        }

    }

}
