open Memoizer
open Timing
open Base

type base = Base.base;;
type dna = Base.dna;;

(* slow lcs *)
let rec slow_lcs ((s1,s2) : dna * dna) : dna =
  match (s1,s2) with 
      ([], _) -> []
    | (_, []) -> []
    | (x :: xs, y :: ys) ->
      if Base.eq x y then
	x :: slow_lcs (xs, ys)
      else
	Base.longer_dna_of (slow_lcs (s1, ys)) (slow_lcs (xs, s2))
;;

(* A potentially useful module *)
module DnaPairOrder : Map.OrderedType with type t = dna * dna =
struct
    type t = dna * dna

    let rec compare_dna x' y' : int = 
        match x',y' with 
          [],[] -> 0
        | [], xs -> -1
        | xs, [] -> 1
        | x::xs, y::ys -> 
	  (match Base.compare x y with
	      0 -> compare_dna xs ys
            | other -> other)
	    

    (* implements a lexicographic ordering: 
     * compare the second components only if first components are equal *)
    let compare (a, b) (c, d) =
      match compare_dna a c with
	  0 -> compare_dna b d
        | other -> other
     
end;;

module LCSMemoizer = Memoizer(Map.Make(DnaPairOrder))

(* Task 4.4 *)

(* implement fast_lcs using your automatic memoizer functor! 
 * doing so will of course require proper creation of modules and
 * use of functors *)

let lcs (recurse: dna*dna -> dna) ((s1,s2) : dna*dna) : dna =
 match (s1,s2) with 
      ([], _) -> []
    | (_, []) -> []
    | (x :: xs, y :: ys) ->
      if Base.eq x y then
	x :: recurse (xs, ys)
      else
	Base.longer_dna_of (recurse (s1, ys)) (recurse (xs, s2))


let fast_lcs (ds : dna * dna) : dna =  LCSMemoizer.memo lcs ds;;


(* Task 4.5 *)


(* main function/testing *)

let print_header () =
  print_string "---------LCS(N) -------------\n";
  print_string "    N     Slow     Fast     \n";
  print_string "-----------------------------\n"
;;
(* Implement some experiment that shows performance difference
 * between slow_lcs and fast_lcs. (Print your results.)     
 * Explain in a brief comment what your experiment shows.        *)

let print_row n slow fast  =
  let space () = print_string "   " in
  let print f = Printf.printf "%6.4f" f in
  let print_slow slow =
    match slow with
    |None -> print_string "  -   "
    |Some f -> print f in

  if n < 10 then print_string " ";
  if n < 100 then print_string " ";

  print_int n; space ();
  print_slow slow; space();
  print fast; space ();
  print_newline()
;;

let experiment ((s1,s2):dna * dna) : unit =
  let slow ds = if (List.length s1) > 16 then None else Some (time_fun slow_lcs ds) in
  let fast ds = time_fun fast_lcs ds in   
  print_row (List.length s1) (slow (s1,s2)) (fast (s1,s2)) 
;;

let main () =
  (* change these numbers if you want depending on the speed of your machine *)
  (* on my machine slow_fib starts taking visible time at input 30 *)
  let trials = [(dna_from_string "A" , dna_from_string "A");
		(dna_from_string "AC" , dna_from_string "AT");
		(dna_from_string "CCTA" , dna_from_string "CCAT");
		(dna_from_string "GTCAATGC" , dna_from_string "GCACCTGA");
		(dna_from_string "TTTTTTTAGCGATGTT" , dna_from_string "ACGGCATTGTACATTG");
		(dna_from_string "TAGGTCATACCGTGTAGCCTTAAATAGATGTT" , 
                 dna_from_string "GTTGGCGCGAGGGGATATATATTTGTTGCATG");
		(dna_from_string "AAAATAAAAATTAAAATTTGGGTGAGTAGGATGAGAGAGGATTCCCCC" , 
		 dna_from_string "ACAAACGAGCTAGCTGAGATAGCTCGAAGAGGGGGTGACTGACTTCAT");
		(dna_from_string "TTTAGGAGAGAGAGATCGATCGACTACGACTAGCTAGCATCGATCGAAGGGGGGGGGGGGGGGGGGGG",
                 dna_from_string "GGGGGGGGGGGGGGGGATACGATACGATTGGGTGTGGGTCGACGACGAGGGGCTTTTTATATCTTTTA");
		(dna_from_string "ACGGCATTGTACATGGGGGGAATATATATAGGGCTAGCTAGCTACGAGGAGCAGCTAGCTACGTACGATCGAGGATGATGCGAGGAGGAGGACTTCTCTT",
                 dna_from_string "CTTCATTAGCATACGATGCTGAGATAGCAGAGAGTCGAGACGTCGTCGAGCTACTGACTGACGAAGGGCTACGTAAAAAAAAAAGCTCGATAGATTATTG");
	       ] in
  print_header();
  List.iter experiment trials
;;

main ();;


