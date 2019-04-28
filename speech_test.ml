open Extracter

module Str = struct
  type t = string
  let compare = Pervasives.compare
end

module WordBank = Map.Make(Str) 

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
let () = List.iter (fun x -> print_string x; print_string "\n") verb_list
