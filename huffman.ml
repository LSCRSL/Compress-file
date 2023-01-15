(*************************************************************
*                                                            *
*                 CODE AFFICHAGE STATISTIQUES                *
*                                                            *
**************************************************************)

let stats prev_file new_file time_comp time_huff = 
  let size f =
    let stat_f = Unix.stat f in
    stat_f.st_size
  in
  let prev_size = size prev_file in
  let new_size = size new_file in
  let rate = (float_of_int new_size)/.(float_of_int prev_size)*.100.0 in 
  Printf.printf "\n   Statistiques sur l'algorithme de compression:
      - Temps d'exécution de l'algorithme de compression: %fs
      - Temps d'exécution de l'algorithme de huffman: %fs
      - Taille de l'ancien fichier: %do
      - Taille du nouveau fichier compressé: %do
      - Taux de compression: %f%%\n\n"  time_comp time_huff prev_size new_size rate

(*************************************************************
*                                                            *
*                     CODE COMPRESSION                       *
*                                                            *
**************************************************************)

(* Fonction de sérialisation de l'arbre *)
let serialisation tree os =
  (*1ere partie de l'arbre ie sa structure*)
  let rec structure tree  os = 
  match tree with 
    Heap.Node(g,d) -> begin
              Bs.write_bit os 1;
              structure g os;
              structure d os 
              end
    |Heap.Leaf(n) -> Bs.write_bit os 0;
  in 

  (*2eme partie de l'arbre ie la suite de caracteres*)
  let rec caracteres tree os = 
  match tree with
    Heap.Node(g,d) -> begin
                      caracteres g os;
                      caracteres d os
                      end
    |Heap.Leaf(n) -> Bs.write_byte os n;
  in
  structure tree os;
  caracteres tree os
  

let write_bits prev_file new_file codehf nbre_bits =
  let rec write_bits_bis prev_file new_file codehf = 
  try 
    match input_char prev_file with 
      |x -> 
      let list_bits = Heap.return_code x codehf in
      let rec octet list_bits = 
        match list_bits with 
          |[] -> write_bits_bis prev_file new_file codehf
          | x::ll -> Bs.write_bit new_file x;
                    octet ll

      in
      octet list_bits
    with
      End_of_file -> Bs.finalize new_file
  in 
  write_bits_bis prev_file new_file codehf

(* Fonction de compression avec en argument f le nom du fichier et s un drapeau pour l'affichage des statistiques *)
let compress f s =
  let debut_comp= Sys.time() in (* CODE STATS: on démarre l'horloge pour l'algo de compression *)
  (* On crée le nom du fichier compressé, on le crée et on l'ouvre*)
  let new_file = f ^ ".hf" in
  let oc = open_out new_file in
  (* On ouvre le fichier à compresser*)
  let ic = open_in f in
  (* On crée l'arbre de huffman*)
  let debut_huff = Sys.time() in (* CODE STATS: on démarre l'horloge pour l'algo de Huffman*)
  let freq = Heap.char_freq ic in 
  let col = Heap.collection( freq ) in 
  (* on crée notre tuple et on obtient (nbre de caractere, arbre), *)
  let ct= Heap.create_tree col in
  (* on récupère notre arbre*, le nb de noeuds, le nb de caractere*)
  let tree = Heap.arbre ct in 
  (* on récupère les codes de nos caracteres à partir de l'arbre*)
  let codehf = Heap.code_arbre tree in
  let fin_huff = Sys.time() in (* CODE STATS: on arrête l'horloge pour l'algo de Huffman*)
  (*on ferme notre fichier*)
  close_in ic;
  (*on le reouvre pour revenir au debut*)
  let ic2 = open_in f in
  (* On écrit dans le fichier *)
  (*On crée le ofstream nécessaire pour la fonction write_bits*)
  let os= Bs.of_out_channel oc in
  (* serialisation de l'arbre *)
  serialisation tree os;
  (* ecriture du texte compressé *)
  write_bits ic2 os codehf [];
  (* On ferme le in_channel, le ofstream et le out_channel *)
  close_in ic2;
  close_out oc;
  Printf.printf "Le fichier %s a bien été compressé dans le fichier %s.hf.\n" f f;
  let fin_comp= Sys.time() in  (* CODE STATS: on arrête l'horloge pour l'algo de compression*)
  let time_compression = fin_comp -. debut_comp in
  let time_huff = fin_huff -. debut_huff in
  if s=1 then 
    stats f (f^".hf") time_compression time_huff


(*************************************************************
*                                                            *
*                    CODE DECOMPRESSION                      *
*                                                            *
**************************************************************)
(*on cree l'arbre*)
let get_arbre is =
  let rec read_struct is=  
    try
      match Bs.read_bit is with 
      |0 -> Heap.Leaf(-1)
      |1 -> let g = read_struct is in 
            let d = read_struct is in 
            Heap.Node(g, d)
      |_ -> failwith "Erreur"
    with 
      Bs.End_of_stream -> failwith "Erreur dans la compression du fichier"
  in 
  let arbre = read_struct is in 

  let rec lec_caracteres is tree = 
    match tree with 
    |Heap.Leaf(n) -> Heap.Leaf(Bs.read_byte is)       
    |Heap.Node(n1, n2) -> let g = lec_caracteres is n1 in 
                          let d = lec_caracteres is n2 in 
                          Heap.Node(g,d)
  in 
  lec_caracteres is arbre 

(*on regarde si le code compresse appartient au code du caractere*)
(*on compare les codes*)
let code_egaux liste1 liste2 = 
  let rec loop liste1 liste2 = 
     match liste1,liste2 with 
      | ([], []) | ([], _) -> true
      | (x1::l1, x2::l2) -> if x1 == x2 then (loop l1 l2)
                             else 
                                false
      |(_, []) -> false
  in 
  loop liste1 liste2

(*parcours 1 et 2 renvoient une liste avec les caracteres possibles associes au code*)
(* 1: si on a pas encore selectionne de caracteres possibles*)
let parcours1 listecode t = 
  let rec parcours x lcode t liste = 
        if x < 256 then
          match t.(x) with
          [] -> parcours (x+1) lcode t liste (*cas ou le caractere n'est pas dans le fichier*)
          | l -> if code_egaux lcode l then (*le code est inclus donc on l'ajoute à la liste des possibles caracteres*)
                    parcours (x+1) lcode t (x::liste)
                else
                    parcours (x+1) lcode t liste (*pas le meme code*)    
        else 
          liste
  in 
  parcours 0 listecode t []

(*2: si on connait deja les caracteres possibles*)
let parcours2 listecode t pos_carac =
  let rec loop listecode t pos_carac nv_carac =  
    match pos_carac with 
    | [] -> nv_carac
    | x::ll -> if code_egaux t.(x) listecode then 
                loop listecode t ll (x::nv_carac)
              else 
                loop listecode t ll nv_carac
  in 
  loop listecode t pos_carac []

(*fonction qui regarde si on cherche un caractere parmis tous ceux de notre arbre 
   ou si l'on doit en trouver un dans une liste plus restreinte*)
let trouver_carac_associe is liste_code codehf liste_carac = 
  if liste_carac == [] then parcours1 liste_code codehf 
  else parcours2 liste_code codehf liste_carac

(* fonction qui renvoie vrai si la list ne contient qu'un élément *)
let is_singleton t = 
  match t with 
    [_] -> true
    | [] -> false
    | _::_ -> false

(* Fonction qui écrit le texte décompressé dans le nouveau fichier *)
let rec read is liste_compress codehf oc liste_carac = (* sous-fonction pour régler le soucis de stack overflow *)
    match Bs.read_bit is with 
    | x ->let liste_code = List.rev (x::List.rev (liste_compress)) in
          let liste_des_caracs = trouver_carac_associe is liste_code codehf liste_carac in 
          if is_singleton liste_des_caracs then 
            begin 
            Bs.write_byte oc (List.hd liste_des_caracs);
            read is [] codehf oc []
            end
          else 
              read is liste_code codehf oc liste_des_caracs

let write_txt is codehf oc = 
  try
    read is [] codehf oc []
  with
    Bs.End_of_stream -> Bs.finalize oc

(* Fonction de décompression *)
let decompress f = 
  let inc= open_in f in
  let is= Bs.of_in_channel inc in
  (* On crée le nom du fichier décompressé, on le crée et on l'ouvre*)
  let new_file = 
    let list=(String.split_on_char '.' f) in
    match list with
    name::ext::_-> 
    name^"_decompresse."^ext
    |f::_-> f^"_decompresse"
    |[]-> failwith "Error"
  in
  let oc = open_out new_file in
  let os= Bs.of_out_channel oc in
  (* on récupère l'arbre *)
  let tree = get_arbre is in 
  (*on récupère les codes de nos caracteres à partir de l'arbre*)
  let codehf = Heap.code_arbre tree in
  (* On lit dans le fichier compressé et on écrit dans le fichier décompressé*)
  write_txt is codehf os;
  (* On ferme le in-channel et le out_channel *)
  close_in inc;
  close_out oc;
  Printf.printf "Le fichier %s a bien été décompressé dans le fichier %s.\n" f new_file