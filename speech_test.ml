open Extracter
open Ingester
open Pervasives
module WordBank = Map.Make(String) 

module Int = struct
  type t = int
  let compare = Pervasives.compare
end

module WordId = Map.Make(Int)

module Parser = struct
  let rec make_word_bank map ls =
    match ls with
    | [] -> map
    | hd::tl -> make_word_bank (WordBank.add hd hd map) tl
  
  let rec make_word_id map ls =
     match ls with
     | [] -> map
     | hd::tl -> make_word_id (WordId.add (List.length tl) hd map) tl
end

let noun_files = Extracter.read "noun"
let change_to_noun = Extracter.change "noun"
let noun_list = Extracter.combine noun_files
let move_back_noun = Extracter.change ".."

let noun_bank = Parser.make_word_bank WordBank.empty noun_list
let noun_id = Parser.make_word_id WordId.empty noun_list

let adj_files = Extracter.read "adj"
let change_to_adj = Extracter.change "adj"
let adj_list = Extracter.combine adj_files
let move_back_adj = Extracter.change ".."

let adj_bank = Parser.make_word_bank WordBank.empty adj_list
let adj_id = Parser.make_word_id WordId.empty adj_list

let adv_files = Extracter.read "adv"
let change_to_adv = Extracter.change "adv"
let adv_list = Extracter.combine adv_files
let move_back_adv = Extracter.change ".."

let adv_bank = Parser.make_word_bank WordBank.empty adv_list
let adv_id = Parser.make_word_id WordId.empty adv_list

let verb_files = Extracter.read "verb"

let change_to_verb = Extracter.change "verb"
let verb_list = Extracter.combine verb_files
let move_back_verb = Extracter.change ".."

let verb_bank = Parser.make_word_bank WordBank.empty verb_list
let verb_id = Parser.make_word_id WordId.empty verb_list

(*let ingest = List.flatten (List.rev (Ingester.read_all "passage1.txt"))*)
let ingest_file = Ingester.ingest "passage1.txt"
let clean word = (Str.global_replace (Str.regexp "[^a-zA-Z]+") "" (word))

let rec strip_adj ls =
    match ls with 
    | hd::tl -> if ((WordBank.mem (clean (hd)) (adj_bank)) = true) && (((List.length tl) mod 3) = 0) then
                    "__ad"::strip_adj tl
                else 
                    hd::strip_adj tl
    | [] -> []

let rec strip_verb ls =
    match ls with
   | hd::tl -> if ((WordBank.mem (clean (hd)) (verb_bank)) = true) && (((List.length tl) mod 3) = 0) then
                     "__v"::strip_verb tl
               else
                     hd::strip_verb tl
   | [] -> []

let rec strip_noun ls =
    match ls with
   | hd::tl -> if ((WordBank.mem (clean (hd)) (noun_bank)) = true) && (((List.length tl) mod 3) = 0) then
                     "__n"::strip_noun tl
               else
                     hd::strip_noun tl
   | [] -> []

let rec strip_adv ls =
    match ls with
    | hd::tl -> if ((WordBank.mem (clean (hd)) (adv_bank)) = true) && (((List.length tl) mod 3) = 0) then
                          "__av"::strip_adv tl
                     else
                          hd::strip_adv tl
    | [] -> []

let init = Random.self_init()
let adj_rand = adj_list |> List.length |> Random.int

let x = print_int adj_rand; print_string "\n"
let y = print_int (List.length adj_list)

(* prints out file contents before board prep *)
let prints = List.iter ( fun x -> print_string x; print_string " ") ingest_file

let is_verb word = if (WordBank.mem word (verb_bank) = true) then print_string "TRUE" else print_string "FALSE"

let list_no_adj = strip_adj ingest_file
let list_no_noun = strip_noun list_no_adj
let list_no_verb = strip_verb list_no_noun
let list_no_adverb = strip_adv list_no_verb
let line_break = print_string "\n"; print_string "\n"

let init = Random.self_init()
let adj_rand = Random.int (List.length adj_list)

let x = print_int adj_rand; print_string "\n"
let y = print_int (List.length adj_list)

let choose_adj = WordId.find (Random.int (List.length adj_list)) adj_id 
let word = print_string "\n"; print_string choose_adj 


let rec insert_adj ls = 
  match ls with 
  | hd::tl -> if hd = "__ad" then 
                (WordId.find (Random.int (List.length adj_list)) adj_id)::insert_adj tl                               
              else 
                hd::insert_adj tl
  |[]->[]

let rec insert_noun ls = 
  match ls with 
  | hd::tl -> if hd = "__n" then 
                (WordId.find (Random.int (List.length noun_list)) noun_id)::insert_noun tl                               
              else 
                hd::insert_noun tl
  |[]->[]


let rec insert_adverb ls = 
  match ls with 
  | hd::tl -> if hd = "__av" then 
                (WordId.find (Random.int (List.length adv_list)) adv_id)::insert_adverb tl                               
              else 
                hd::insert_adverb tl
  |[]->[]

let rec insert_verb ls = 
  match ls with 
  | hd::tl -> if hd = "__v" then 
                (WordId.find (Random.int (List.length verb_list)) verb_id)::insert_verb tl                               
              else 
                hd::insert_verb tl
  |[]->[]

let empty =  List.iter (fun x -> print_string x; print_string " ") list_no_adverb

let with_adj = insert_adj list_no_adverb
let break = print_string "\n"; print_string "\n"

let with_noun  = insert_noun with_adj
let break = print_string "\n"; print_string "\n"

let with_adverb  = insert_adverb  with_noun
let break = print_string "\n"; print_string "\n"

let with_verb  = insert_verb with_adverb
let break = print_string "\n"; print_string "\n"

let () = List.iter (fun x -> print_string x; print_string " ") with_verb
