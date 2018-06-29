(* Wojciech Ostrowski *)

type t = Empty | Node of t * (int * int) * t * int * int

let height = function
  | Node (_, _, _, h, _) -> h
  | Empty -> 0

(* podaje liczbe elementow w zbiorze *)
let set_elems = function
  | Node (_, _, _, _, e) -> e
  | Empty -> 0

let empty = Empty

let is_empty x = x = Empty

(* add_control x y z = min (x+y+z) max_int; 
   x,y,z sa nieujemne                      *)
let add_control x y z =
  if x > max_int - y - z then max_int
  else x + y + z

(* liczba liczb nalezacych do przedzialu (x,y) *)
let inter_count x y =
  if x > 0 then y - x + 1
  else if y >= 0 then
    if x = min_int then max_int
    else add_control (-x) y 1
  else if y = -1 then
    if x = min_int then max_int
    else (-x)
  else abs (x - y) + 1

let make l (a, b) r = 
  Node (l, (a, b), r, max (height l) (height r) + 1, 
        add_control (set_elems l) (set_elems r) (inter_count a b))

let bal l (x, y) r =
  let hl = height l in
  let hr = height r in 
  if hl > hr + 2 then
    match l with
    | Node (ll, (la, lb), lr, _, _) ->
        if height ll >= height lr then make ll (la, lb) (make lr (x, y) r)
        else
          (match lr with
          | Node (lrl, (lra, lrb), lrr, _, _) ->
              make (make ll (la, lb) lrl) (lra, lrb) (make lrr (x, y) r)
          | Empty -> assert false)
    | Empty -> assert false
  else if hr > hl + 2 then
    match r with
    | Node (rl, (ra, rb), rr, _, _) ->
        if height rr >= height rl then make (make l (x, y) rl) (ra, rb) rr
        else
          (match rl with
          | Node (rll, (rla, rlb), rlr, _, _) ->
              make (make l (x, y) rll) (rla, rlb) (make rlr (ra, rb) rr)
          | Empty -> assert false)
    | Empty -> assert false
  else Node (l, (x,y), r, max hl hr + 1, 
             add_control (set_elems l) (set_elems r) (inter_count x y))

(* dodaje do zbioru przedzial, ktorego elementy sa mniejsze od 
   najmniejszego elementu tego zbioru                         *)
let leftmost_add (x, y) set =
  let rec loop (x, y) = function
  | Empty -> Node (Empty, (x, y), Empty, 1, inter_count x y)
  | Node (l, (a, b), r, _, _) -> bal (loop (x, y) l) (a, b) r
  in loop (x, y) set

(* dodaje do zbioru przedzial, ktorego elementy sa wieksze od
   najwiekszego elementu tego zbioru                         *)
let rightmost_add (x, y) set = 
  let rec loop (x, y) = function
  | Empty -> Node (Empty, (x, y), Empty, 1, inter_count x y)
  | Node (l, (a, b), r, _, _) -> bal l (a, b) (loop (x, y) r)
  in loop (x, y) set

let rec join s1 (x, y) s2 = match (s1, s2) with
  | (Empty, _) -> leftmost_add (x, y) s2
  | (_, Empty) -> rightmost_add (x, y) s1
  | (Node (l1, (a1, b1), r1, h1, e1), Node (l2, (a2, b2), r2, h2, e2)) ->
    if h1 > h2 + 2 then 
      bal l1 (a1, b1) (join r1 (x, y) s2)
    else if h2 > h1 + 2 then
      bal (join s1 (x, y) l2) (a2, b2) r2
    else Node 
      (s1, (x, y), s2, max h1 h2 + 1, add_control e1 e2 (inter_count x y))

let split x set = 
  let rec loop x = function
  | Empty -> (Empty, false, Empty)
  | Node (l, (a, b), r, _, _) ->
    if x = a && x = b then
      (l, true, r)
    else if x = a then
      (l, true, leftmost_add (a + 1, b) r)
    else if x = b then
      (rightmost_add (a, b - 1) l, true, r)
    else if x > a && x < b then
      (rightmost_add (a, x - 1) l, true, leftmost_add (x + 1, b) r)
    else if x < a then
      let (ll, pres, rl) = loop x l in (ll, pres, join rl (a, b) r)
    else
      let (lr, pres, rr) = loop x r in (join l (a, b) lr, pres, rr)        
  in loop x set

let rec min_elt = function
  | Node (Empty, (a, b), _, _, _) -> (a, b)
  | Node (l, _, _, _, _) -> min_elt l
  | Empty -> failwith "empty set"

let rec remove_min_elt = function
  | Node (Empty, _, r, _, _) -> r
  | Node (l, (a, b), r, _, _) -> bal (remove_min_elt l) (a, b) r
  | Empty -> failwith "empty set"

let rec max_elt = function
  | Node (_, (a, b), Empty, _, _) -> (a, b)
  | Node (_, _, r, _, _) -> max_elt r
  | Empty -> failwith "empty set"

let rec remove_max_elt = function
  | Node (l, _, Empty, _, _) -> l
  | Node (l, (a, b), r, _, _) -> bal l (a, b) (remove_max_elt r)
  | Empty -> failwith "empty set" 

(* laczy 2 zbiory w jeden *)
let merge s1 s2 =
  match (s1, s2) with
  | (Empty, _) -> s2
  | (_, Empty) -> s1
  | _ ->
      let (a, b) = min_elt s2 in
      bal s1 (a, b) (remove_min_elt s2)

(* add najpierw korzysta z procedury split, ktora wyznacza lx i ry, ktore sa
   odpowiednio : zbiorem liczb mniejszych od x i zbiorem wiekszych od y.
   Nastepnie add laczy lx i ry z przedzialem (x,y)                          *)
let add (x, y) set =
  let (lx, _, _) = split x set
  and (_, _, ry) = split y set in 
    (* ry_add dodaje (x,y) do ry; jezeli najmniejszy element w ry
       jest rowny y + 1, to max przedzial w ry jest rozszerzany  *)
    let ry_add ry =
      let minry = min_elt ry in
        if fst minry > y + 1 then
          leftmost_add (x, y) ry
        else 
          leftmost_add (x, snd minry) (remove_min_elt ry)
  (* procedura analogiczna do ry_add *)  
  and lx_add lx =
      let maxlx = max_elt lx in
        if snd maxlx < x - 1 then
	  rightmost_add (x, y) lx
        else 
          rightmost_add (fst maxlx, y) (remove_max_elt lx)
    in
      match (lx, ry) with
        | (Empty, Empty) -> Node (Empty, (x, y), Empty, 1, inter_count x y)
        | (Empty, _) -> ry_add ry
        | (_, Empty) -> lx_add lx
        | _ ->
          let maxlx = max_elt lx
          and minry = min_elt ry in
            if snd maxlx = x - 1 && fst minry = y + 1 then
              join (remove_max_elt lx) (fst maxlx, snd minry) (remove_min_elt ry)
            else if fst minry = y + 1 then
              join lx (x, snd minry) (remove_min_elt ry)
            else
              merge (lx_add lx) ry
	    
(* znajduje, za pomoca split, zbiory zawieracjace liczby mniejsze od x
   oraz wieksze od y, a nastepnie je laczy                            *)
let rec remove (x,y) set =
  let (lx, _, _) = split x set
  and (_, _, ry) = split y set in
  match (lx, ry) with
    | (Empty, _) -> ry
    | (_, Empty) -> lx
    | _ -> merge lx ry

(* sprawdza, czy x nalezy do zbioru za pomoca split *)
let mem x set = 
  let (_, pres, _) = split x set
  in pres

let iter f set = 
  let rec loop = function
    | Empty -> ()
    | Node (l, (a, b), r, _, _) -> 
      loop l; f (a, b); loop r
  in loop set

let fold f set acc =
  let rec loop acc = function 
    | Empty -> acc
    | Node (l, (a, b), r, _, _) ->
      loop (f (a, b) (loop acc l)) r
  in loop acc set

let elements set =
  let rec loop acc = function
    | Empty -> acc
    | Node (l, (a, b), r, _, _) ->
      loop ((a, b)::loop acc r) l
  in loop [] set

(* korzysta z procedury split, zeby wyznaczyc 
   liczbe elementow mniejszych niz x         *)
let below x set = 
  let (l, pres, _) = split x set in 
    if pres && set_elems l < max_int then set_elems l + 1
    else set_elems l
