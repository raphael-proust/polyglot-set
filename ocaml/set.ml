module List = struct

  let rec length = function
    | [] -> 0
    | _ :: xs -> 1 + length xs

  let rec rev_append xs ys =
    match xs with
    | [] -> ys
    | x :: xs -> rev_append xs (x :: ys)

  let shuffle1 xs =
    let rec aux a b = function
      | [] -> rev_append a b
      | x :: xs -> aux b (x :: a) xs
    in
    aux [] [] xs

  let[@coq_struct "n"] rec shuffle n xs =
    if n <= 0 then xs else shuffle (n - 1) (shuffle1 xs)

  let rec for_all f = function
    | [] -> true
    | x :: xs -> f x && for_all f xs

  let iteri f xs =
    let rec aux n = function
      | [] -> ()
      | x :: xs -> f n x; aux (n+1) xs
    in
    aux 0 xs

  let rec iter f = function [] -> () | x :: xs -> f x; iter f xs

  let rec uniq = function
    | [] | [ _ ] -> true
    | c :: cs -> for_all ((<>) c) cs && uniq cs

  let find1 predicate cs =
    let rec aux negatives = function
      | [] -> None
      | cc :: cs ->
          if predicate cc then
            Some (cc, rev_append negatives cs)
          else
            aux (cc :: negatives) cs
    in
    aux [] cs

  let find2 predicate cs =
    let rec aux negatives = function
      | [] | [ _ ] -> None
      | cb :: cs ->
          match find1 (predicate cb) cs with
          | Some (cc, cs) -> Some ((cb, cc), rev_append negatives cs)
          | None -> aux (cb :: negatives) cs
    in
    aux [] cs

  let find3 predicate cs =
    let rec aux negatives = function
      | [] | [_] | [_; _] -> None
      | ca :: cs ->
          match find2 (predicate ca) cs with
          | Some ((cb, cc), cs) -> Some ((ca, cb, cc), rev_append negatives cs)
          | None -> aux (ca :: negatives) cs
    in
    aux [] cs

  let for_each xs f =
    let rec aux acc = function
      | [] -> acc
      | x :: xs -> aux (rev_append (f x) acc) xs
    in
    aux [] xs

end

module type TERNARY = sig

  (** Each property can take one of three values. Ternaries are abstract
      properties so we represent them as abstract values [Tn]. *)
  type t = T0 | T1 | T2

  (** [all] is [[T0; T1; T2]]. *)
  val all : t list

  (** [triplet a b c] is [true] iff the values for [a], [b], and [c] are either
      all the same (e.g., all [T2]) or all different (i.e., one of them is [T0],
      one is [T1], and the other is [T2]). *)
  val triplet : t -> t -> t -> bool
end

module Ternary
: TERNARY
= struct
  type t = T0 | T1 | T2
  let all : t list = [ T0; T1; T2 ]
  let triplet a b c =
    match (a, b, c) with
      | T0, T0, T0 | T1, T1, T1 | T2, T2, T2
      | T0, T1, T2 | T0, T2, T1
      | T1, T2, T0 | T1, T0, T2
      | T2, T0, T1 | T2, T1, T0
          -> true
      | T0, T0, (T1 | T2) | T0, (T1 | T2), T0 | (T1 | T2), T0, T0
      | T1, T1, (T2 | T0) | T1, (T2 | T0), T1 | (T2 | T0), T1, T1
      | T2, T2, (T0 | T1) | T2, (T0 | T1), T2 | (T0 | T1), T2, T2
          -> false

  (* NOTE: things to prove in coq:
    - forall a, b : t, exists c, triplet (a, b, c)
     - forall a, b, c : t, triplet (a, b, c) => triplet (a, c, b)
     - forall a, b, c : t, triplet (a, b, c) => triplet (c, a, b)
     - forall a, b, c : t, triplet a b c, forall d <> c, not triplet a b d
     *)
end

module type PROPERTY = sig
  val name : string
  include TERNARY
  val to_string : t -> string
end

module type NAME = sig val name : string val s0 : string val s1 : string val s2 : string end

module MakeProp (N : NAME)
: PROPERTY
= struct
   include Ternary
   let name = N.name
   let to_string = function | T0 -> N.s0 | T1 -> N.s1 | T2 -> N.s2
end

(** These are the 4 properties of the game Set. We currently use simple strings
    to visually represent the different values. *)

module Shape = MakeProp (struct let name = "shape" let s0 = "~" let s1 = "o" let s2 = "⋄" end)
module Fill = MakeProp (struct let name = "fill" let s0 = "░" let s1 = "▒" let s2 =  "▓" end)
module Colour = MakeProp (struct let name = "colour" let s0 = "R" let s1 ="G" let s2 =  "P" end)
module Count = MakeProp (struct let name = "count" let s0 = "⠄" let s1 ="⠢" let s2 =  "⠦" end)

module Card = struct

  (** A card has for properties, each with a value for this property. *)
  type t = {
    shape : Shape.t;
    fill : Fill.t;
    colour : Colour.t;
    count : Count.t;
  }

  (** A simple pretty-printing for cards, we just print each property back-to-back. *)
  let to_string { shape; fill; colour; count } =
    Shape.to_string shape ^ Fill.to_string fill ^ Colour.to_string colour ^ Count.to_string count

  (** [all] is the full set of all the possible cards. *)
  let all =
    List.for_each Shape.all @@ fun shape ->
      List.for_each Fill.all @@ fun fill ->
        List.for_each Colour.all @@ fun colour ->
          List.for_each Count.all @@ fun count ->
            [ { shape; fill; colour; count } ]

(*   let () = assert (List.uniq all) *)
(*   let () = assert (List.length all = 3 * 3 * 3 * 3) *)

  (** Three cards form a valid triplet if each of their properties form a valid
      ternary triplet. *)
  let triplet ca cb cc =
    Shape.triplet ca.shape cb.shape cc.shape
    && Fill.triplet ca.fill cb.fill cc.fill
    && Colour.triplet ca.colour cb.colour cc.colour
    && Count.triplet ca.count cb.count cc.count

      end

module Deck = struct

  (** A deck is a list of cards. *)
  type t = Card.t list

  let size t = List.length t

  (** [triplet d] is either [None] if there are no valid triplets in the deck or
      it is [Some ((a, b, c), dd)] such that
      - [(a, b, c)] are a valid triplet,
      - [a], [b], and [c] are each cards of [d],
      - [a], [b], and [c] are pairwise distinct,
      - [dd] contains all the cards from [d] except from [a], [b], and [c]. *)
  let triplet deck = List.find3 Card.triplet deck

end

module Table = struct

  (** A table is a draw-pile, a set of shown cards (modeled as a deck), and a list
      of foudn triplets. *)
  type t = {
    draw: Deck.t;
    shown : Deck.t;
    found : (Card.t * Card.t * Card.t) list;
  }

  let draw_pile { draw; _ } = draw
  let shown_cards { shown; _ } = shown
  let found_triplets { found; _ } = found

  let pp ppf { draw; shown; found } =
    Printf.fprintf ppf "found: %d triples\n" (List.length found);
    Printf.fprintf ppf "draw: %d cards\n" (List.length draw);
    List.iteri (fun i c -> Printf.fprintf ppf "%s%s" (Card.to_string c) (if i mod 3 = 2 then "\n" else "\t")) shown

    (** Draw cards (by default: three cards as per the game rules) *)
  let draw ?(n = 3) table =
    let draw1 = function
      | { draw = []; _ } -> None
      | { draw = c :: draw; shown; found } ->
          let r = { draw; shown = c :: shown; found } in
(*           assert (List.uniq (List.rev_append r.draw r.shown)); *)
          Some r
          in
    let rec aux n table =
      if n <= 0 then Some table else
        match draw1 table with
        | None -> None
        | Some table -> aux (n - 1) table
    in
    aux n table

  (** initial table *)
  let init = Option.get @@ draw ~n:12 { draw = List.shuffle 10 Card.all; shown = []; found = [] }

end

let rec play1 (table : Table.t) : ((string list * Table.t), string) result =
(*   assert (Deck.size table.shown mod 3 = 0); *)
  match Deck.triplet table.shown with
  | None -> begin
      match Table.draw table with
      | None -> Error "No triplets, Not enough cards to draw, Bye."
      | Some table ->
          Ok (["No triplets"; "Drawn three additional cards."], table)
  end
  | Some ((ca, cb, cc), shown) -> begin
      let found_msg = Printf.sprintf "Found: \t%s \t%s \t%s" (Card.to_string ca) (Card.to_string cb) (Card.to_string cc) in
      let table = { table with shown; found = (ca, cb, cc) :: table.found } in
      let (table_msg, table) =
        if Deck.size shown >= 12 then
          ("Extraneous cards, not drawing.", table)
        else begin
          match Table.draw table with
          | None -> ("No more cards, can't draw.", table)
          | Some table -> ("Replenished with three cards.", table)
        end
      in
      Ok ([found_msg; table_msg], table)
  end

let rec play (table : Table.t) =
  let (_ : string) = read_line () in
  match play1 table with
  | Error msg ->
      Printf.printf "End of game: %s\n\n%a\n" msg Table.pp table
  | Ok (msgs, table) ->
      List.iter (Printf.printf "%s\n") msgs;
      Printf.printf "Table: ";
      List.iter (fun card -> Printf.printf "\t%s" (Card.to_string card)) (Table.shown_cards table);
      Printf.printf "\n";
      play table

let () =
  let table = Table.init in
  Printf.printf "Table: ";
  List.iter (fun card -> Printf.printf "\t%s" (Card.to_string card)) (Table.shown_cards table);
  play table
