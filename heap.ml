exception Code_inexistant

type tree =
      | Leaf of int
      | Node of tree * tree

type t = (int*int) list
let empty = []

let is_singleton t = 
  match t with 
   [_] -> true
  | [] -> false
  | _::_ -> false

let is_empty t = 
  t = empty

let add e t = 
  let e1 = fst(e) in 
  let e2 = snd(e) in 
  let rec insert (f,s) l = 
    match l with
    [] -> (f,s)::[]
    | (a,b)::ll -> if f < a then (f,s)::l
                else (a,b)::(insert (f,s)ll)
  in 
  insert (e1,e2) t        
   
let sort_tree t =
  List.sort (fun (a1,b1) (a2,b2) -> if a1 < a2 then -1 else 1 ) t

let find_min t = 
  match sort_tree t with
    [] -> failwith "la liste est vide" 
    | x::ll -> x

(*affiche le 1er element des tuples de la liste (sert comme test)*) 
let rec aff_1etuple l =
  match l with
  |[] -> Printf.printf "\n"
  | (a,b)::ll -> Printf.printf"%d\n" a;
              aff_1etuple ll

let remove_min t = 
  let tab = sort_tree t in
  match tab with
  [] -> failwith "la liste est vide"
  | x::ll -> (x,ll)

let char_freq c = 
  let rec loop tab = 
    try
    match input_char c with
      x -> begin 
        tab.(int_of_char x) <- tab.(int_of_char x) + 1;
        loop tab
      end 
    with
    End_of_file -> tab
  in 
  loop (Array.make 256 0)

let collection tab = 
  let rec loop l t i = 
    if i < 256 then 
      if tab.(i) != 0 then loop (add (tab.(i), i) l) t (i+1)
      else loop l t (i+1)
    else 
      List.sort compare l
  in 
  loop [] tab 0

let occ_feuille t = 
  let f = fun (occ, y) -> (occ, Leaf y) in
  List.map f t

let create_tree c = 
  let cf =  occ_feuille c in 
   let rec loop t =
    match t with 
    [] -> failwith "le fichier donné est vide"
    |[x] -> t
    | _::_ ->   let (n1,t1),ccf = remove_min t in 
                let (n2,t2),cff = remove_min ccf in
                loop (add (n1 + n2, Node (t1, t2)) cff)
  in 
  loop cf

let affiche_1element_arbre ah = 
  match ah with 
  [] -> failwith "erreur"
  | (a,_)::_ -> Printf.printf"le dernier entier de l'arbre de huffman (nbre de caracteres total):  %d\n" a

let arbre de =
  match de with 
  [] -> failwith "erreur"
  | (_,b)::_ -> b

let caractere de = 
  match de with 
  [] -> failwith "erreur"
  | (a,_)::_ -> a

let code_arbre tree =
  let tab = Array.make 256 [] in
  let rec loop t acc tab =
    match t with
    | Leaf (n) -> tab.(n) <- List.rev acc
    | Node (g,d) -> loop g (0::acc) tab;
                    loop d (1::acc) tab
  in
  loop tree [] tab;
  tab

let nbre_de_Zeros_en_plus tab_occ code = 
  let rec loop tab_occ code bits_compress indice =
    if indice < 256 then 
      loop tab_occ code (bits_compress+ tab_occ.(indice)*List.length (code.(indice))) (indice+1)
    else 
      (8 - (bits_compress mod 8))
  in 
  loop tab_occ code 0 0

let affiche_tab tab =
  Printf.printf"\n";
  let rec loop x t = 
    if x < 256 then
    match t.(x) with
    [] -> loop (x+1) t
    | l -> Printf.printf "%d : " x;
          List.iter (Printf.printf"%n") l ;
          Printf.printf "\n";
          loop (x+1) t
    else Printf.printf"\n"
  in 
  loop 0 tab

let affiche_arbre tree =
  let rec loop t = 
    match t with
    | Leaf(n) -> Printf.printf "%d\n" n
    | Node(g,d) -> begin 
                  Printf.printf "noeud\n";
                  loop g;
                  loop d
                  end
  in 
  loop tree

let return_code c tab = 
  tab.(int_of_char (c))

let return_char code tab =
  let rec bis code tab i= 
    if i >= Array.length tab then
      raise Code_inexistant
    else 
      if (List.compare_lengths code (tab.(i))=0) then
        let blist= List.map2 (fun x y -> x-y) code tab.(i) in
          let add x y= x+y in
          if (List.fold_left add 0 blist)=0 then
            char_of_int i 
          else bis code tab (i+1)
      else bis code tab (i+1)
  in bis code tab 0


(*************************************************************
*                                                            *
*                JEUX DE TESTS HEAP.ML                       *
*                                                            *
**************************************************************)
let test () = 
  (*test si les fcts marchent mais on pourra enlever ça*)
  let a = is_singleton(empty) in 
  let b = is_empty(empty) in 
  Printf.printf "empty singleton ? : %b ; empty vide ? : %b\n" a b ;
  let c = is_singleton(add (6,7) []) in 
  Printf.printf"c est un singleton : %b\n" c ;
  let d = is_singleton(add (6,9) (add(4,7) [])) in 
  Printf.printf"d est un singleton : %b\n" d ; 
  (*ce qui nous interesse vraiment*)
  let f1 = open_in "test2.txt" in 
  let p = collection(char_freq f1) in 
  let x = create_tree p in
  Printf.printf "tuple avec arbre final est un singleton ? : %b\n" (is_singleton x);
  affiche_1element_arbre x;
  let v = arbre x in 
  (*affiche_arbre v;*)
  let t1 = code_arbre v in
  affiche_tab (t1);
  List.iter (Printf.printf"%d") (return_code 's' t1);