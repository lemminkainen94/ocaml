(* Wojciech Ostrowski *)
(* Kolejka jest implementowana jako kopiec binarny, w którym
   w każdym węźle przechowywany jest priorytet oraz długość
   prawej ścieżki. W poniższych procedurach używam następujących
   skrótów :
   - l, r - lewe i prawe poddrzewo kopca reprezentującego
     kolejkę q
   - nlp - długość prawej ścieżki (skrót od null path length)
   - p - priorytet                                             *)
type 'a queue = Null | Node of 'a queue * 'a * int * 'a queue;;

let empty = Null;;

let is_empty (q : 'a queue) =
  match q with
    | Null -> true
    | _ -> false;;

exception Empty;;

(* Rekurencyjna procedura join łączy kolejki q1 i q2. Jeśli
   obydwie kolejki są niepuste, to kolejka wynikowa ma w
   korzeniu min p1 p2. Jeżeli p1 < p2, to lewym poddrzewem 
   kolejki wynikowej zostaje lewe poddrzewo q1, natomiast 
   prawym poddrzewem kolejki wynikowej zostaje wynik połączenia
   q2 oraz prawego poddrzewa q1. Następnie, jeśli prawa ścieżka
   w lewym poddrzewie jest krótsza niż w prawym poddrzewie, to
   następuje zamiana poddrzew.*)
let rec join (q1 : 'a queue) (q2 : 'a queue) =
  match (q1, q2) with
    | (Null, _) -> q2
    | (_, Null) -> q1
    | (Node (l1, p1, npl1, r1), Node (l2, p2, npl2, r2)) ->
      if p1 < p2 then
	if l1 = Null then
	  Node (q2, p1, 0, Null)
	else
	  let q3 = join q2 r1 in
	    let Node (_, _, nlpl, _) = l1
	    and Node (_, _, nlp3, _) = q3 in
	      if nlpl > nlp3 then
	        Node (l1, p1, nlp3 + 1, q3)
	      else
	        Node (q3, p1, nlpl + 1, l1)
      else
	if l2 = Null then
	  Node (q1, p2, 0, Null)
	else
	  let q3 = join q1 r2 in
	    let Node (_, _, nlpl, _) = l2
	    and Node (_, _, nlp3, _) = q3 in
	      if nlpl > nlp3 then
	        Node (l2, p2, nlp3 + 1, q3)
	      else
	        Node (q3, p2, nlpl + 1, l2);;

(* Procedura delete_min usuwa element o najmniejszym priorytecie,
   znajdujący się w korzeniu, a następnie używa procedury join
   do połączenia prawego i lewego drzewa w jedną kolejkę.      *)
let delete_min (q : 'a queue) =
  match q with
    | Null -> raise Empty
    | Node (l, p, d, r) -> (p, join l r);;

(* Procedura add dodaje element e do kolejki priorytetowej q,
   tworząc jednoelementową kolejkę qe i łącząc ją z kolejką q
   przy pomocy procedury join.                                 *)
let add (e : 'a) (q : 'a queue) =
  let qe = Node (Null, e, 0, Null)
  in join qe q;;

