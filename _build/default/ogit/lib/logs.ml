type commit = {
    parents : Digest.t list;
    date : float;
    message : string;
    content : Digest.t
}
  
(* 
la date au format HH:MM:SS-JJ/MM/AAAA
ex: 16:08:40-07/11/2022 *)


let date_add date = 
    if date < 10 then string_of_int (0) ^ string_of_int (date) else string_of_int (date) 


let date_fm _d = 
    let final = Unix.gmtime _d in 
    let s = date_add (final.tm_hour) ^ ":" ^ date_add (final.tm_min) ^ ":" ^ date_add (final.tm_sec) ^ "-" ^ date_add (final.tm_mday) ^ "/" ^ date_add (final.tm_mon + 1) ^ "/" ^ string_of_int (final.tm_year + 1900) 
    in s




















(* 

let set_head _l = failwith "TODO"

let get_head () = failwith "TODO" 

let make_commit _s  _h =  failwith "TODO"

let init_commit () = failwith "TODO"

let store_commit _c = failwith "TODO"

let read_commit _h = failwith "TODO" 

*)