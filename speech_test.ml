open Extracter
open Ingester

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

let ingest = List.flatten (List.rev (Ingester.read_all "passage1.txt"))

let clean word = (Str.global_replace (Str.regexp "[^a-zA-Z]+") "" (word))

let rec strip_adj ls =  
    match ls with 
    | hd::tl -> if ((WordBank.mem (clean (hd)) (adj_bank)) = true) then
                    "__ad"::strip_adj tl
                else 
                    hd::strip_adj tl
    | [] -> []


let rec strip_verb ls =
    match ls with
   | hd::tl -> if ((WordBank.mem (clean (hd)) (verb_bank)) = true) then
                     "__v"::strip_verb tl
               else
                     hd::strip_verb tl
   | [] -> []

let rec strip_noun ls =
    match ls with
   | hd::tl -> if ((WordBank.mem (clean (hd)) (noun_bank)) = true) then
                     "__n"::strip_noun tl
               else
                     hd::strip_noun tl
   | [] -> []

let rec strip_adv ls =
    match ls with
    | hd::tl -> if ((WordBank.mem (clean (hd)) (adv_bank)) = true) then
                          "__av"::strip_adv tl
                     else
                          hd::strip_adv tl
    | [] -> []


(*let abstract = strip_speech ("__ad") (adj_bank) (ls)*)
let prints = List.iter ( fun x -> print_string x;print_string " ") ingest

(*
let rec is_adj ls =
  match ls with 
  | hd::tl -> begin match hd with 
              | hd2::tl2 -> if ((WordBank.mem (hd2) (adj_bank)) = true) then
                               "_ad"::(is_adj (hd2) (tl2))
                             else 
                                hd2::(is_adj (hd2) (tl2))
              | [] -> []
              end 
  | [] -> []

*)


let is_verb word = if (WordBank.mem word (verb_bank) = true) then print_string "TRUE" else print_string "FALSE"

 

(*
 * clean string before check
 * iterate through list. do 4 interations or do 1 
 *
 *
 *
 * *)


(* List.iter (fun x -> List.iter (fun x -> print_string x; print_string "\n") x) ingest*)

(*what if we are placing replacing a word with punctuation*)
(* *)
(* 
 * read passage.txt (story) using regex to remove punctuation 
 * check the word in each word bank. 
 * if no match record word in the list,
 * if match put _speech sign in the new list
 * repeat. do this for at most n/4 words (where n is the number of words in the passage)
 * once the entire passage has gone through. output the newly constructed list into a file with " " as delimiting character
 * 
 * read story_board file in to a list.
 * if not _speech sign, write to a new list 
 * if _speech sign, generate random number between 1 and WordID for part of speech place newly retrived word in new list
 * do this until end of story_board file 
 *
 * output the list into a file, a story has been generated 
 * *)

let list_no_adj = strip_adj ingest
let list_no_noun = strip_noun list_no_adj
let list_no_verb = strip_verb list_no_noun
let list_no_adverb = strip_adv list_no_verb

let line_break = print_string "\n"; print_string "\n"
let () = List.iter (fun x -> print_string x; print_string " ") list_no_adverb
  (*List.iter (fun x -> print_string x; print_string "\n") verb_list*)
