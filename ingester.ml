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

  let ingest file = List.flatten (List.rev (read_all file))
end

