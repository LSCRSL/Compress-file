# Projet IPF

## Membres
   - Elisa Ciocarlan
   - Lisa Ceresola

## Fichiers

Fichiers de code:
- huff.ml
- huffman.ml
- heap.mli et heap.ml
- bs.mli et bs.ml
Fichiers de test (format txt):
- test.txt contenant le mot "satisfaisant"
- test2.txt contenant des carctères très variés
- test3.txt de taille moyenne contenant un poème
- Amis.txt de grande taille contenant une oeuvre littéraire
- testvide.txt, un texte vide
Rapport (format pdf):
- Rapport.pdf


## Comment éxécuter ce programme
Dans la ligne de commande, entrer "dune build" pour compiler les fichiers puis éxécuter à l'aide de la commande "./huff.exe huff --help" par exemple pour avoir accès aux différentes options proposées par ce programme.

## Répartition des fonctions dans les différents fichiers

### huff.ml (fichier principal) (gestion principale par Elisa)
- ligne de commande

### huffman.ml (travail partagé)
Code affichage des statistiques:
- stats
Code de compression:
- serialisation
- write_bits
- compress   

Code de décompression:
- get_arbre
- code_egaux
- parcours1 
- parcours2 
- trouver_carac_associe
- is_singleton 
- write_txt 
- decompress

### heap.ml et heap.mli (gestion principale par Lisa)
- exception Code_inexistant
- type t = (int * int) list
- type tree =
      | Leaf of int
      | Node of tree * tree
- val empty : t
- val add : (int*int) -> t -> t
- val find_min :  t -> (int*int)
- val sort_tree : (int * 'a) list -> (int * 'a) list
- val remove_min :  t -> (int*int) *  t
- val char_freq : in_channel -> int array
- val is_singleton :  t -> bool
- val is_empty : t -> bool
- val collection : int array -> (int*int) list
- val occ_feuille : ('a*int) list -> ('a* tree) list
- val create_tree : (int*int) list -> (int*tree) list
- val code_arbre : tree -> int list array
- val arbre : ('a * 'b) list -> 'b
- val caractere : ('a* 'b) list -> 'a
- val return_code : char -> 'a array -> 'a 
- val return_char : int list -> int list array -> char
- val nbre_de_Zeros_en_plus : int array -> 'a list array -> int
- val test : unit -> unit
- val aff_1etuple : (int * 'a) list -> unit
- val affiche_1element_arbre : (int*'a) list -> unit
- val affiche_arbre : tree -> unit 
- val affiche_tab : int list array -> unit

### bs.ml et bs.mli (fichier fourni)
- type istream
- exception Invalid_stream
- exception End_of_stream
- val of_in_channel : in_channel -> istream
- val read_bit : istream -> int
- val read_n_bits : istream -> int -> int
- val read_byte : istream -> int
- val read_short : istream -> int
- val read_int : istream -> int
- type ostream
- val of_out_channel : out_channel -> ostream
- val write_bit : ostream -> int -> unit
- val write_n_bits : ostream -> int -> int -> unit
- val write_byte : ostream -> int -> unit
- val write_short : ostream -> int -> unit
- val write_int : ostream -> int -> unit
- val finalize : ostream -> unit