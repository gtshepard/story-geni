open Extracter

let noun_files = Extracter.read "noun"
let change_dir = Extracter.change "noun"
let noun_list = Extracter.combine noun_files

let () = List.iter (fun x -> print_string x; print_string "\n") noun_list
