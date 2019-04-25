open Pervasives 

module Extracter = struct
    
  (*retrieves a line from a file*)
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

  (*reads in all fie names for a given dir into a list*)
  let read dir = Array.to_list (Sys.readdir dir)
  
  (* change to specified  dir to read files *)
  let change dir = Sys.chdir dir

  (* combines contents of all filenames in the list*)
  let rec combine = function 
      | hd::tl -> (read_all hd)@(combine tl)
      | [] -> []
end

(*generates a file with all subdirs in  name/  format  *)
let make_dir_file = Sys.command "ls */ -d >> dir_names.txt"

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

(*
module Parser = struct
    
  let rec make_word_bank map ls = 
     match ls with 
    | [] -> map
    | hd::tl -> make_word_bank (WordBank.add hd hd map) tl

  let word_bank = make_word_bank WordBank.empty adj

  let rec make_word_id map ls = 
    match ls with 
    | [] -> map
    | hd::tl -> make_word_id (WordId.add (List.length tl) hd map) tl

  let word_id = make_word_id WordId.empty adj

  let print_word_id = WordId.iter (fun x y -> print_int x; print_string " "; print_string y; print_string " \n") word_id 
   
end
*)

let files = Extracter.read "noun"
let chnage_dir = Extracter.change "noun"
let noun_list = Extracter.combine files

let () = List.iter (fun x -> print_string x) noun_list
  (*let () = WordId.iter (fun x y -> print_int x; print_string " "; print_string y; print_string " \n") word_id*)
  (*WordBank.iter (fun x y -> print_string x; print_string " "; print_string y; print_string " \n") word_bank*)

