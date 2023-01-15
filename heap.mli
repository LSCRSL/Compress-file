exception Code_inexistant
(** Exception levée par return_char si le code n'existe pas dans le tableau descodes **)

type t = (int * int) list

(*type tab = int array*)

type tree =
      | Leaf of int
      | Node of tree * tree
(** The type of heaps. Elements are ordered using generic comparison. **)

val empty : t
(** [empty] is the empty heap. *)

val add : (int*int) -> t -> t
(** [add e h] add element [e] to [h]. *)

val find_min :  t -> (int*int)
(** [find_min h] returns the smallest elements of [h] w.r.t to 
    the generic comparison [<] *)

val sort_tree : (int * 'a) list -> (int * 'a) list
(** [sort_tree] trie la liste par rapport au premier element des tuples (dans notre cas l'occurrence)*)

val remove_min :  t -> (int*int) *  t
(** [remove_min h] returns the pair of the smallest elements of [h] w.r.t to 
    the generic comparison [<] and [h] where that element has been removed. *)

val char_freq : in_channel -> int array
(** [char_freq c] renvoie un tableau des fréquences des caractères présents dans le fichier lu ainsi que le nombre de caractères lus**)

val is_singleton :  t -> bool
(** [is_singleton h] returns [true] if [h] contains one element *)

val is_empty : t -> bool
(** [is_empty h] returns [true] if [h] contains zero element *)

val collection : int array -> (int*int) list
(** [collection] qui cree une collection a partir d'un array (l'array est celui créé par la fct char_freq)*)

val occ_feuille : ('a*int) list -> ('a* tree) list
(** [occ_feuille] qui transforme l'entier representant un caractere en feuille*)

val create_tree : (int*int) list -> (int*tree) list
(**[create_tree] cree l'arbre de huffman a patir d'une collection*)

val code_arbre : tree -> int list array
(** [code_arbre] permet d'obtenir le code de chaque caractere en parcourant l'abre de huffman et les codes (sont des listes de booléens, true=1 et false=0)
et sont dans un tableau (ie : tab(caractere en ASCII) = [code] ) *)

val arbre : ('a * 'b) list -> 'b
(** [arbre] renvoie l'arbre uniquement (2e element du tuple)*)

val caractere : ('a* 'b) list -> 'a
(** [caractere] renvoie le nbre de caracteres uniquement (1er element du tuple)*)

val return_code : char -> 'a array -> 'a 
(** [return_code] renvoie le code du caractere donne en argument par rapport au tableau defini par l'arbre de Huffman*)

val return_char : int list -> int list array -> char
(** [return_char] renvoie le caractere dont le code est donne en argument par rapport au tableau defini par l'arbre de Huffman*)

val nbre_de_Zeros_en_plus : int array -> 'a list array -> int
(** [nbre_de_Zeros_en_plus] renvoie le nbre de bits a 0 a rajouter à la fin du fichier*) 

val test : unit -> unit

val aff_1etuple : (int * 'a) list -> unit
(** [aff_1etuple] affiche les occurrences (a ete utile pour voir si l'ordre etait correct 
    (ie les caracteres sont rangés par ordre croissant suivant leur occurrence ))*)

val affiche_1element_arbre : (int*'a) list -> unit
(** [affiche_1element_arbre] affiche le nbre de caracteres dans le text*)

val affiche_arbre : tree -> unit 
(** [affiche_arbre] affiche les feuilles de notre arbre c'est à dire les caracteres en ASCII *)

val affiche_tab : int list array -> unit
(** [affiche_tab] affiche chaque caractere suivi de son code trouvé grace à l'arbre de huffman *)
