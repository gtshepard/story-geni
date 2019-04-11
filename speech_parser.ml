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

(* Read in names of files in directory. store them in a list. iter trhogh the list and call read_all. and each call to read all creates a list. add them to a list. after this you will iter thorugh this list and cons each elsit into 1. then put the list in a map *)
(* m*)

let argv = Array.to_list (Sys.argv)  

let rec build_adj ls = 
   match ls with 
   | hd::tl -> (read_all hd)@(build_adj tl)
   | [] -> []

let adj = build_adj argv

let () = List.iter (fun x -> print_string x; print_string " \n") (adj)
