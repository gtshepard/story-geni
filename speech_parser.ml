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

(*let argv = Array.to_list (Sys.argv) *)

let rec combine = function 
   | hd::tl -> (read_all hd)@(combine tl)
   | [] -> []

(*let adj = combine argv*)
(* read all file names in from directory *)

let dir = Array.to_list (Sys.readdir "adj")
let curr_dir = Sys.chdir "adj"
let adj = combine dir

let () = adj |> List.iter (fun x -> print_string x; print_string "\n")
