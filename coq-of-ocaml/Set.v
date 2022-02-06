Require Import CoqOfOCaml.CoqOfOCaml.
Require Import CoqOfOCaml.Settings.

Module List.
  Fixpoint length {A : Set} (function_parameter : list A) : int :=
    match function_parameter with
    | [] => 0
    | cons _ xs => Z.add 1 (length xs)
    end.
  
  Fixpoint rev_append {A : Set} (xs : list A) (ys : list A) : list A :=
    match xs with
    | [] => ys
    | cons x xs => rev_append xs (cons x ys)
    end.
  
  Definition shuffle1 {A : Set} (xs : list A) : list A :=
    let fix aux {B : Set}
      (a : list B) (b : list B) (function_parameter : list B) : list B :=
      match function_parameter with
      | [] => rev_append a b
      | cons x xs => aux b (cons x a) xs
      end in
    aux nil nil xs.
  
  Fixpoint shuffle {A : Set} (n : int) (xs : list A) {struct n} : list A :=
    if CoqOfOCaml.Stdlib.le n 0 then
      xs
    else
      shuffle (Z.sub n 1) (shuffle1 xs).
  
  Fixpoint for_all {A : Set} (f : A -> bool) (function_parameter : list A)
    : bool :=
    match function_parameter with
    | [] => true
    | cons x xs => andb (f x) (for_all f xs)
    end.
  
  Definition iteri {A B : Set} (f : int -> A -> B) (xs : list A) : unit :=
    let fix aux (n : int) (function_parameter : list A) : unit :=
      match function_parameter with
      | [] => tt
      | cons x xs =>
        let '_ := f n x in
        aux (Z.add n 1) xs
      end in
    aux 0 xs.
  
  Fixpoint iter {A B : Set} (f : A -> B) (function_parameter : list A) : unit :=
    match function_parameter with
    | [] => tt
    | cons x xs =>
      let '_ := f x in
      iter f xs
    end.
  
  Fixpoint uniq {A : Set} (function_parameter : list A) : bool :=
    match function_parameter with
    | ([] | cons _ []) => true
    | cons c cs => andb (for_all (nequiv_decb c) cs) (uniq cs)
    end.
  
  Definition find1 {A : Set} (predicate : A -> bool) (cs : list A)
    : option (A * list A) :=
    let fix aux (negatives : list A) (function_parameter : list A)
      : option (A * list A) :=
      match function_parameter with
      | [] => None
      | cons cc cs =>
        if predicate cc then
          Some (cc, (rev_append negatives cs))
        else
          aux (cons cc negatives) cs
      end in
    aux nil cs.
  
  Definition find2 {A : Set} (predicate : A -> A -> bool) (cs : list A)
    : option ((A * A) * list A) :=
    let fix aux (negatives : list A) (function_parameter : list A)
      : option ((A * A) * list A) :=
      match function_parameter with
      | ([] | cons _ []) => None
      | cons cb cs =>
        match find1 (predicate cb) cs with
        | Some (cc, cs) => Some ((cb, cc), (rev_append negatives cs))
        | None => aux (cons cb negatives) cs
        end
      end in
    aux nil cs.
  
  Definition find3 {A : Set} (predicate : A -> A -> A -> bool) (cs : list A)
    : option ((A * A * A) * list A) :=
    let fix aux (negatives : list A) (function_parameter : list A)
      : option ((A * A * A) * list A) :=
      match function_parameter with
      | ([] | cons _ [] | cons _ (cons _ [])) => None
      | cons ca cs =>
        match find2 (predicate ca) cs with
        | Some ((cb, cc), cs) => Some ((ca, cb, cc), (rev_append negatives cs))
        | None => aux (cons ca negatives) cs
        end
      end in
    aux nil cs.
  
  Definition for_each {A B : Set} (xs : list A) (f : A -> list B) : list B :=
    let fix aux (acc : list B) (function_parameter : list A) : list B :=
      match function_parameter with
      | [] => acc
      | cons x xs => aux (rev_append (f x) acc) xs
      end in
    aux nil xs.
End List.

Module TERNARY.
  Record signature {t : Set} : Set := {
    (** Each property can take one of three values. Ternaries are abstract
      properties so we represent them as abstract values [Tn]. *)
    t := t;
    (** [all] is [[T0; T1; T2]]. *)
    all : list t;
    (** [triplet a b c] is [true] iff the values for [a], [b], and [c] are either
      all the same (e.g., all [T2]) or all different (i.e., one of them is [T0],
      one is [T1], and the other is [T2]). *)
    triplet : t -> t -> t -> bool;
  }.
End TERNARY.
Definition TERNARY := @TERNARY.signature.
Arguments TERNARY {_}.

Module Ternary.
  Inductive t : Set :=
  | T0 : t
  | T1 : t
  | T2 : t.
  
  Definition all : list t := [ T0; T1; T2 ].
  
  Definition triplet (a : t) (b : t) (c : t) : bool :=
    match (a, b, c) with
    |
      ((T0, T0, T0) | (T1, T1, T1) | (T2, T2, T2) | (T0, T1, T2) | (T0, T2, T1)
      | (T1, T2, T0) | (T1, T0, T2) | (T2, T0, T1) | (T2, T1, T0)) => true
    |
      ((T0, T0, (T1 | T2)) | (T0, (T1 | T2), T0) | ((T1 | T2), T0, T0) |
      (T1, T1, (T2 | T0)) | (T1, (T2 | T0), T1) | ((T2 | T0), T1, T1) |
      (T2, T2, (T0 | T1)) | (T2, (T0 | T1), T2) | ((T0 | T1), T2, T2)) => false
    end.
  
  Definition module :=
    {|
      TERNARY.all := all;
      TERNARY.triplet := triplet
    |}.
End Ternary.
Definition Ternary : TERNARY (t := _) := Ternary.module.

Module PROPERTY.
  Record signature {t : Set} : Set := {
    name : string;
    t := t;
    all : list t;
    triplet : t -> t -> t -> bool;
    to_string : t -> string;
  }.
End PROPERTY.
Definition PROPERTY := @PROPERTY.signature.
Arguments PROPERTY {_}.

Module NAME.
  Record signature : Set := {
    name : string;
    s0 : string;
    s1 : string;
    s2 : string;
  }.
End NAME.
Definition NAME := NAME.signature.

Module MakeProp.
  Class FArgs := {
    N : NAME;
  }.
  
  (** Inclusion of the module [Ternary] *)
  Definition t `{FArgs} := Ternary.(TERNARY.t).
  
  Definition all `{FArgs} := Ternary.(TERNARY.all).
  
  Definition triplet `{FArgs} := Ternary.(TERNARY.triplet).
  
  Definition name `{FArgs} : string := N.(NAME.name).
  
  Definition to_string `{FArgs} (function_parameter : t) : string :=
    match function_parameter with
    | Ternary.T0 => N.(NAME.s0)
    | Ternary.T1 => N.(NAME.s1)
    | Ternary.T2 => N.(NAME.s2)
    end.
  
  Definition functor `{FArgs} :=
    {|
      PROPERTY.name := name;
      PROPERTY.all := all;
      PROPERTY.triplet := triplet;
      PROPERTY.to_string := to_string
    |}.
End MakeProp.
Definition MakeProp (N : NAME) : PROPERTY (t := _) :=
  let '_ := MakeProp.Build_FArgs N in
  MakeProp.functor.

Definition Shape :=
  MakeProp
    (let name := "shape" in
    let s0 := "~" in
    let s1 := "o" in
    let s2 := "\226\139\132" in
    {|
      NAME.name := name;
      NAME.s0 := s0;
      NAME.s1 := s1;
      NAME.s2 := s2
    |}).

Definition Fill :=
  MakeProp
    (let name := "fill" in
    let s0 := "\226\150\145" in
    let s1 := "\226\150\146" in
    let s2 := "\226\150\147" in
    {|
      NAME.name := name;
      NAME.s0 := s0;
      NAME.s1 := s1;
      NAME.s2 := s2
    |}).

Definition Colour :=
  MakeProp
    (let name := "colour" in
    let s0 := "R" in
    let s1 := "G" in
    let s2 := "P" in
    {|
      NAME.name := name;
      NAME.s0 := s0;
      NAME.s1 := s1;
      NAME.s2 := s2
    |}).

Definition Count :=
  MakeProp
    (let name := "count" in
    let s0 := "\226\160\132" in
    let s1 := "\226\160\162" in
    let s2 := "\226\160\166" in
    {|
      NAME.name := name;
      NAME.s0 := s0;
      NAME.s1 := s1;
      NAME.s2 := s2
    |}).

Module Card.
  (** A card has for properties, each with a value for this property. *)
  Module t.
    Record record : Set := Build {
      shape : Shape.(PROPERTY.t);
      fill : Fill.(PROPERTY.t);
      colour : Colour.(PROPERTY.t);
      count : Count.(PROPERTY.t) }.
    Definition with_shape shape (r : record) :=
      Build shape r.(fill) r.(colour) r.(count).
    Definition with_fill fill (r : record) :=
      Build r.(shape) fill r.(colour) r.(count).
    Definition with_colour colour (r : record) :=
      Build r.(shape) r.(fill) colour r.(count).
    Definition with_count count (r : record) :=
      Build r.(shape) r.(fill) r.(colour) count.
  End t.
  Definition t := t.record.
  
  (** A simple pretty-printing for cards, we just print each property back-to-back. *)
  Definition to_string (function_parameter : t) : string :=
    let '{|
      t.shape := shape;
        t.fill := fill;
        t.colour := colour;
        t.count := count
        |} := function_parameter in
    String.append (Shape.(PROPERTY.to_string) shape)
      (String.append (Fill.(PROPERTY.to_string) fill)
        (String.append (Colour.(PROPERTY.to_string) colour)
          (Count.(PROPERTY.to_string) count))).
  
  (** [all] is the full set of all the possible cards. *)
  Definition all : list t :=
    List.for_each Shape.(PROPERTY.all)
      (fun (shape : Shape.(PROPERTY.t)) =>
        List.for_each Fill.(PROPERTY.all)
          (fun (fill : Fill.(PROPERTY.t)) =>
            List.for_each Colour.(PROPERTY.all)
              (fun (colour : Colour.(PROPERTY.t)) =>
                List.for_each Count.(PROPERTY.all)
                  (fun (count : Count.(PROPERTY.t)) =>
                    [
                      {| t.shape := shape; t.fill := fill; t.colour := colour;
                        t.count := count |}
                    ])))).
  
  (** Three cards form a valid triplet if each of their properties form a valid
      ternary triplet. *)
  Definition triplet (ca : t) (cb : t) (cc : t) : bool :=
    andb (Shape.(PROPERTY.triplet) ca.(t.shape) cb.(t.shape) cc.(t.shape))
      (andb (Fill.(PROPERTY.triplet) ca.(t.fill) cb.(t.fill) cc.(t.fill))
        (andb
          (Colour.(PROPERTY.triplet) ca.(t.colour) cb.(t.colour) cc.(t.colour))
          (Count.(PROPERTY.triplet) ca.(t.count) cb.(t.count) cc.(t.count)))).
End Card.

Module Deck.
  (** A deck is a list of cards. *)
  Definition t : Set := list Card.t.
  
  Definition size {A : Set} (t : list A) : int := List.length t.
  
  (** [triplet d] is either [None] if there are no valid triplets in the deck or
      it is [Some ((a, b, c), dd)] such that
      - [(a, b, c)] are a valid triplet,
      - [a], [b], and [c] are each cards of [d],
      - [a], [b], and [c] are pairwise distinct,
      - [dd] contains all the cards from [d] except from [a], [b], and [c]. *)
  Definition triplet (deck : list Card.t)
    : option ((Card.t * Card.t * Card.t) * list Card.t) :=
    List.find3 Card.triplet deck.
End Deck.

Module Table.
  (** A table is a draw-pile, a set of shown cards (modeled as a deck), and a list
      of foudn triplets. *)
  Module t.
    Record record : Set := Build {
      draw : Deck.t;
      shown : Deck.t;
      found : list (Card.t * Card.t * Card.t) }.
    Definition with_draw draw (r : record) :=
      Build draw r.(shown) r.(found).
    Definition with_shown shown (r : record) :=
      Build r.(draw) shown r.(found).
    Definition with_found found (r : record) :=
      Build r.(draw) r.(shown) found.
  End t.
  Definition t := t.record.
  
  Definition draw_pile (function_parameter : t) : Deck.t :=
    let '{| t.draw := draw |} := function_parameter in
    draw.
  
  Definition shown_cards (function_parameter : t) : Deck.t :=
    let '{| t.shown := shown |} := function_parameter in
    shown.
  
  Definition found_triplets (function_parameter : t)
    : list (Card.t * Card.t * Card.t) :=
    let '{| t.found := found |} := function_parameter in
    found.
  
  Definition pp (ppf : Stdlib.out_channel) (function_parameter : t) : unit :=
    let '{| t.draw := draw; t.shown := shown; t.found := found |} :=
      function_parameter in
    let '_ :=
      Stdlib.Printf.fprintf ppf
        (CamlinternalFormatBasics.Format
          (CamlinternalFormatBasics.String_literal "found: "
            (CamlinternalFormatBasics.Int CamlinternalFormatBasics.Int_d
              CamlinternalFormatBasics.No_padding
              CamlinternalFormatBasics.No_precision
              (CamlinternalFormatBasics.String_literal " triples\n"
                CamlinternalFormatBasics.End_of_format))) "found: %d triples\n")
        (List.length found) in
    let '_ :=
      Stdlib.Printf.fprintf ppf
        (CamlinternalFormatBasics.Format
          (CamlinternalFormatBasics.String_literal "draw: "
            (CamlinternalFormatBasics.Int CamlinternalFormatBasics.Int_d
              CamlinternalFormatBasics.No_padding
              CamlinternalFormatBasics.No_precision
              (CamlinternalFormatBasics.String_literal " cards\n"
                CamlinternalFormatBasics.End_of_format))) "draw: %d cards\n")
        (List.length draw) in
    List.iteri
      (fun (i : int) =>
        fun (c : Card.t) =>
          Stdlib.Printf.fprintf ppf
            (CamlinternalFormatBasics.Format
              (CamlinternalFormatBasics.String
                CamlinternalFormatBasics.No_padding
                (CamlinternalFormatBasics.String
                  CamlinternalFormatBasics.No_padding
                  CamlinternalFormatBasics.End_of_format)) "%s%s")
            (Card.to_string c)
            (if equiv_decb (Z.modulo i 3) 2 then
              "\n"
            else
              "\t")) shown.
  
  (** Draw cards (by default: three cards as per the game rules) *)
  Definition draw (op_staroptstar : option int) : t -> option t :=
    let n :=
      match op_staroptstar with
      | Some op_starsthstar => op_starsthstar
      | None => 3
      end in
    fun (table : t) =>
      let draw1 (function_parameter : t) : option t :=
        match function_parameter with
        | {| t.draw := [] |} => None
        | {| t.draw := cons c draw; t.shown := shown; t.found := found |} =>
          let r :=
            {| t.draw := draw; t.shown := cons c shown; t.found := found |} in
          Some r
        end in
      let fix aux (n : int) (table : t) : option t :=
        if CoqOfOCaml.Stdlib.le n 0 then
          Some table
        else
          match draw1 table with
          | None => None
          | Some table => aux (Z.sub n 1) table
          end in
      aux n table.
  
  (** initial table *)
  Definition init : t :=
    Stdlib.Option.get
      (draw (Some 12)
        {| t.draw := List.shuffle 10 Card.all; t.shown := nil; t.found := nil |}).
End Table.

Fixpoint play1 (table : Table.t) : sum (list string * Table.t) string :=
  match Deck.triplet table.(Table.t.shown) with
  | None =>
    match Table.draw None table with
    | None => Stdlib.Error "No triplets, Not enough cards to draw, Bye."
    | Some table =>
      Stdlib.Ok ([ "No triplets"; "Drawn three additional cards." ], table)
    end
  | Some ((ca, cb, cc), shown) =>
    let found_msg :=
      Stdlib.Printf.sprintf
        (CamlinternalFormatBasics.Format
          (CamlinternalFormatBasics.String_literal "Found: \t"
            (CamlinternalFormatBasics.String CamlinternalFormatBasics.No_padding
              (CamlinternalFormatBasics.String_literal " \t"
                (CamlinternalFormatBasics.String
                  CamlinternalFormatBasics.No_padding
                  (CamlinternalFormatBasics.String_literal " \t"
                    (CamlinternalFormatBasics.String
                      CamlinternalFormatBasics.No_padding
                      CamlinternalFormatBasics.End_of_format))))))
          "Found: \t%s \t%s \t%s") (Card.to_string ca) (Card.to_string cb)
        (Card.to_string cc) in
    let table :=
      Table.t.with_found (cons (ca, cb, cc) table.(Table.t.found))
        (Table.t.with_shown shown table) in
    let '(table_msg, table) :=
      if CoqOfOCaml.Stdlib.ge (Deck.size shown) 12 then
        ("Extraneous cards, not drawing.", table)
      else
        match Table.draw None table with
        | None => ("No more cards, can't draw.", table)
        | Some table => ("Replenished with three cards.", table)
        end in
    Stdlib.Ok ([ found_msg; table_msg ], table)
  end.

Fixpoint play (table : Table.t) : unit :=
  let '_ := CoqOfOCaml.Stdlib.read_line tt in
  match play1 table with
  | Stdlib.Error msg =>
    Stdlib.Printf.printf
      (CamlinternalFormatBasics.Format
        (CamlinternalFormatBasics.String_literal "End of game: "
          (CamlinternalFormatBasics.String CamlinternalFormatBasics.No_padding
            (CamlinternalFormatBasics.String_literal "\n\n"
              (CamlinternalFormatBasics.Alpha
                (CamlinternalFormatBasics.Char_literal "010" % char
                  CamlinternalFormatBasics.End_of_format)))))
        "End of game: %s\n\n%a\n") msg Table.pp table
  | Stdlib.Ok (msgs, table) =>
    let '_ :=
      List.iter
        (Stdlib.Printf.printf
          (CamlinternalFormatBasics.Format
            (CamlinternalFormatBasics.String CamlinternalFormatBasics.No_padding
              (CamlinternalFormatBasics.Char_literal "010" % char
                CamlinternalFormatBasics.End_of_format)) "%s\n")) msgs in
    let '_ :=
      Stdlib.Printf.printf
        (CamlinternalFormatBasics.Format
          (CamlinternalFormatBasics.String_literal "Table: "
            CamlinternalFormatBasics.End_of_format) "Table: ") in
    let '_ :=
      List.iter
        (fun (card : Card.t) =>
          Stdlib.Printf.printf
            (CamlinternalFormatBasics.Format
              (CamlinternalFormatBasics.Char_literal "009" % char
                (CamlinternalFormatBasics.String
                  CamlinternalFormatBasics.No_padding
                  CamlinternalFormatBasics.End_of_format)) "\t%s")
            (Card.to_string card)) (Table.shown_cards table) in
    let '_ :=
      Stdlib.Printf.printf
        (CamlinternalFormatBasics.Format
          (CamlinternalFormatBasics.Char_literal "010" % char
            CamlinternalFormatBasics.End_of_format) "\n") in
    play table
  end.

(** Init function; without side-effects in Coq *)
Definition init_module : unit :=
  let table := Table.init in
  let '_ :=
    Stdlib.Printf.printf
      (CamlinternalFormatBasics.Format
        (CamlinternalFormatBasics.String_literal "Table: "
          CamlinternalFormatBasics.End_of_format) "Table: ") in
  let '_ :=
    List.iter
      (fun (card : Card.t) =>
        Stdlib.Printf.printf
          (CamlinternalFormatBasics.Format
            (CamlinternalFormatBasics.Char_literal "009" % char
              (CamlinternalFormatBasics.String
                CamlinternalFormatBasics.No_padding
                CamlinternalFormatBasics.End_of_format)) "\t%s")
          (Card.to_string card)) (Table.shown_cards table) in
  play table.
