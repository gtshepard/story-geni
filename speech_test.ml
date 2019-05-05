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

  
let sys_args = List.rev (Array.to_list (Sys.argv))

let noun_files = Extracter.read "noun"
let change_to_noun = Extracter.change "noun"
let noun_list = Extracter.combine noun_files
let move_back_noun = Extracter.change ".."

(* make word stores for nouns*)
let noun_bank = Parser.make_word_bank WordBank.empty noun_list
let noun_id = Parser.make_word_id WordId.empty noun_list

let adj_files = Extracter.read "adj"
let change_to_adj = Extracter.change "adj"
let adj_list = Extracter.combine adj_files
let move_back_adj = Extracter.change ".."

(* make word stores for adj*)
let adj_bank = Parser.make_word_bank WordBank.empty adj_list
let adj_id = Parser.make_word_id WordId.empty adj_list

let adv_files = Extracter.read "adv"
let change_to_adv = Extracter.change "adv"
let adv_list = Extracter.combine adv_files
let move_back_adv = Extracter.change ".."

(* make word stores for adv*)
let adv_bank = Parser.make_word_bank WordBank.empty adv_list
let adv_id = Parser.make_word_id WordId.empty adv_list

let verb_files = Extracter.read "verb"

let change_to_verb = Extracter.change "verb"
let verb_list = Extracter.combine verb_files
let move_back_verb = Extracter.change ".."

(* make word stores for verb*)
let verb_bank = Parser.make_word_bank WordBank.empty verb_list
let verb_id = Parser.make_word_id WordId.empty verb_list

let file_chooser = 
    if List.length (List.rev (Array.to_list (Sys.argv))) = 1 then 
      Ingester.ingest List.hd
    else 
      Ingester.ingest "passage1.txt"

(*
let remove_first ls =
 67     match ls with
 68    | hd::tl -> tl
 69    | []-> []
 70 
 71 let arg_list = remove_first (Array.to_list (Sys.argv))
 72 let file_output =
 73     if (List.length arg_list) = 0 then
 74         List.rev (List.map classify (read_all "MiniMakefile"))
 75     else
 76         List.rev (List.map classify (read_all (List.hd arg_list)))

*)
(*(let ingest_file = Ingester.ingest "passage1.txt"*)
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

(* prints out file contents before board prep *)
let prints = List.iter ( fun x -> print_string x; print_string " ") ingest_file
let line_breaks = print_string "\n";print_string "\n"

let stripped_text_list = strip_adv (strip_verb (strip_noun (strip_adj ingest_file)))

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

let input speech = print_string "\n"; Printf.printf speech;read_line()

let rec fill_in_adj ls = 
  match ls with 
  | hd::tl -> if hd = "__ad" then 
                (input ("adjective: "))::fill_in_adj tl
              else 
                hd::fill_in_adj tl
  |[]->[]

let rec fill_in_verb ls = 
  match ls with 
  | hd::tl -> if hd = "__v" then 
                (input ("verb: "))::fill_in_verb tl
              else 
                hd::fill_in_verb tl
  |[]->[]

let rec fill_in_noun ls = 
  match ls with 
  | hd::tl -> if hd = "__n" then 
                (input ("noun: "))::fill_in_noun tl
              else 
                hd::fill_in_noun tl
  |[]->[]

let rec fill_in_adverb ls = 
  match ls with 
  | hd::tl -> if hd = "__av" then 
                (input ("adverb: "))::fill_in_adverb tl
              else 
                hd::fill_in_adverb tl
  |[]->[]

let stripped = List.iter (fun x -> print_string x; print_string " ") stripped_text_list

let fill = fill_in_adverb (fill_in_noun (fill_in_verb (fill_in_adj (stripped_text_list))))

let break = print_string "\n";print_string "I FILLED THIS IN ABOVE"; print_string "\n"
let insert = insert_adverb (insert_noun (insert_verb (insert_adj stripped_text_list)))
let () = List.iter (fun x -> print_string x; print_string " ") fill; print_string "\n"; List.iter (fun x -> print_string x; print_string " ") insert

