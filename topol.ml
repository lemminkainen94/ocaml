(* Wojciech Ostrowski *)

open PMap;;

exception Cykliczne

(* z danej listy budowany jest graf g, przechowywany jako mapa;
   do kazdego wierzcholka dowiazana jest para (n,l) gdzie l to
   lista sasiadow, a n to liczba ze zbioru {0,1,2}, gdzie:
   0 - nie odwiedzony wierzcholek
   1 - aktualnie przetwarzany wierzcholek
   2 - przetworzony wierzcholek                                *)
let topol lista =
  let wynikowa = ref []
  and g = ref (List.fold_left (fun a (v, l) -> add v (0, l) a) empty lista) in
  (* search wybiera kolejne elementy z danej listy do przeszukiwania wglab *)
  let rec search lista = 
    let rec dfs_visit v =
      if mem v (!g) then
        let (n, l) = find v (!g) in
          match n with
	    | 0 -> 
	        g := add v (1, l) !g;
                List.iter (dfs_visit) l;
	        wynikowa := (v::(!wynikowa));
	        g := add v (2, l) !g
	    | 1 -> raise Cykliczne
	    | 2 -> ()
      else begin
	wynikowa := (v::(!wynikowa));
        g := add v (2, []) !g
      end 
    in match lista with
      | [] -> ()
      | (v, _)::t ->
        dfs_visit v;
        search t
  in search lista;
  !wynikowa
