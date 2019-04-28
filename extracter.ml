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
(*let make_dir_file = Sys.command "ls */ -d >> dir_names.txt"*)
