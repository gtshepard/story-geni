open Pervasives 

let get_line ic =
  try
    Some (input_line ic)
  with
  | End_of_file -> None

(* returns all lines from the file as a list of strings, arranged in the reverse order *)
let read_all filename =
  let ic = open_in filename in

  let rec read acc =
    match get_line ic with
    | Some line -> read (line :: acc)
    | None ->
        close_in ic; (* close input channel *)
        acc (* return accumulator *)
  in
  read []

let rec combine = function 
   | hd::tl -> (read_all hd)@(combine tl)
   | [] -> []

let make_dir_file = Sys.command "ls */ -d >> dir_names.txt"

let dir = Array.to_list (Sys.readdir "adj")
let curr_dir = Sys.chdir "adj"
let adj = combine dir
  
module Str = struct
  type t = string
  let compare = Pervasives.compare
end

module WordBank = Map.Make(Str)

let rec make_word_bank map ls = 
   match ls with 
   |[] -> map
   |hd::tl -> make_word_bank (WordBank.add hd hd map) tl

let mapper = make_word_bank WordBank.empty adj
let () = WordBank.iter (fun x y -> print_string x; print_string " "; print_string y; print_string " \n") mapper
