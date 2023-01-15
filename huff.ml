(* Ci-dessous le code de la gestion de la ligne de commande *)
let ligne_de_commande() =
  if Array.length Sys.argv < 2 then
    Printf.printf "Nombre de paramètres sur la ligne de commande invalide. Veuillez recommencer en entrant dans la ligne de commande 'huff' suivi de l'option souhaitée.\n"
  else 
    (* Rend vrai si le fichier existe *)
    let file_ok f = 
      try (Sys.file_exists f)
      with
        Sys_error f -> false
    in
    (* Affiche l'aide dans le terminal *)
    let affichage_aide ()= 
      Printf.printf "\n                                 ** AIDE **\n
    Les options proposées par ce programme sont les suivantes:
      — huff fichier : pour compresser le fichier donné en argument et obtenir un fichier fichier.hf
      — huff fichier.hf : pour décompresser le fichier donné en argument et obtenir un fichier fichier
      — huff --stats fichier : pour compresser le fichier et aussi afficher des statistiques sur ce dernier
      — huff --help : pour afficher un message d’aide sur les différentes options\n\n"
    in
    (* Execute l'action demandée si les conditions sont vérifiées *)
    let match_file f s= (* avec f le nom du fichier, s un drapeau indiquant s'il faut afficher les statistiques *)
      (* Cas fichier.hf *)
      let cas_hf nom =
        if not (file_ok nom) then
          Printf.printf "Le fichier \" %s \" n'existe pas\n" nom
        else 
          if s=1 then begin(* Cas decompression et affichage des stats *)
            Printf.printf "Il n'est pas possible de décompresser un fichier en affichant les statistiques, consulter \"huff --help\" pour avoir la liste des options possibles.\n"             
          end
          else begin (* Cas decompression *)
            Printf.printf "Decompression du fichier en cours...\n";
            Huffman.decompress nom
          end
      in
      (* Cas fichier *)
      let cas_f nom =
        if not (file_ok nom) then
          Printf.printf "Le fichier \" %s \" n'existe pas\n" nom
        else 
          if s=0 then begin (* Cas compression *)
            Printf.printf "Compression en cours...\n";
            Huffman.compress nom 0
          end
          else begin (* Cas compression et affichage des stats *)
            Printf.printf "Compression en cours...\n";
            Huffman.compress nom 1
          end
      in
      match f with
          name::["hf"] -> cas_hf name
          | name::ext::["hf"] -> let nom= name^"."^ext^".hf" in
                                  cas_hf nom
          | name::[] -> cas_f name
          | name::ext::[] -> let nom = name^"."^ext in
                              cas_f nom
          |_ -> Printf.printf "Syntaxe invalide. Pour avoir de l'aide, entrer \"huff --help\".\n"
    in
    let h = Sys.argv.(1) in
    (* Verifie que la commande huff ainsi qu'une option et/ou un nom de fichier sont donnés dans la ligne de commandes *)
    if h<>"huff" || Array.length Sys.argv = 2 then
      Printf.printf "Veuillez entrer dans la ligne de commande \"huff\" suivi de l'option souhaitée.\nPour avoir de l'aide, entrer \"huff --help\".\n"
    (* Cas huff et option/fichier *)
    else if Array.length Sys.argv = 3 then
      let f= Sys.argv.(2) in
      (* Affichage de l'aide *)
      if f="--help" then
        affichage_aide ()
      (* Verifie si l'utilisateur a voulu entrer une option *)
      else if f.[0]='-' && f.[1]='-' then
            Printf.printf "L'option n'a pas été reconnue, pour avoir de l'aide, entrer \"huff --help\".\n"
            else 
              let file_hf = String.split_on_char '.' f in
              match_file file_hf 0 (* Compression/Decompression du fichier *)
      (* Cas huff et option et fichier *)
      else
        let o = Sys.argv.(2) in
        let f = Sys.argv.(3) in
        if o <> "--stats" then
          Printf.printf "L'option n'a pas été reconnue, pour avoir de l'aide, entrer \"huff --help\".\n"
        else
          let file_hf = String.split_on_char '.' f in
          match_file file_hf 1 (* Decompression du fichier et affichage des statistiques*)

let () = 
  ligne_de_commande()