open Pervasives
open Str
module Ingester = struct

  let get_line ic =
    try
      Some (input_line ic)
    with
      | End_of_file -> None
  
  let rx_space = Str.regexp "[ \t]+"
  let tokenize line = Str.split rx_space line 

  let read_all filename =
    let ic = open_in filename in
      let rec read acc =
        match get_line ic with
        | Some line -> read ((tokenize line) :: acc)
        | None -> close_in ic; acc
        in
          read []
 (* 
  * a line is read. 
  * we break up the line by spaces (tokenize) use Ocaml Str
  * that returns a list of words 
  *
  * we clean every word when we check it against the map, that way there is never punctuation lost 
  *
  * punctuation must be removed for word checking?
  * how do we perserve punctuation when the story is rebuilt?
  * 
  * *)

end

let ingest = List.rev (Ingester.read_all "passage.txt")
(*remember the Str module needs Str.cma when comppile ocamlc and Str.cmxa when compiled with ocamlopt it goes before the file name consider adding to make file*) 
let () = List.iter (fun x -> List.iter (fun x -> print_string x; print_string "\n") x) ingest
