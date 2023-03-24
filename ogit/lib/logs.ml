type commit = {
    parents : Digest.t list;
    date : float;
    message : string;
    content : Digest.t
}
  
let date_add date = 
    if date < 10 then string_of_int (0) ^ string_of_int (date) else string_of_int (date) 


let date_fm _d = 
    let final = Unix.gmtime _d in 
    let s = date_add (final.tm_hour) ^ ":" ^ date_add (final.tm_min) ^ ":" ^ date_add (final.tm_sec) ^ "-" ^ date_add (final.tm_mday) ^ "/" ^ date_add (final.tm_mon + 1) ^ "/" ^ string_of_int (final.tm_year + 1900) 
    in s




let lecture_fichier filename = 
    let ic = open_in filename in 
    let res = ref "" in 
    try 
        while true do 
            res := !res ^ input_line ic ^ "\n";
        done;
        close_in ic;
        assert false 
    with 
    | End_of_file -> !res 

    

let write_to_file content file = 
    let oc = open_out file in 
    output_string oc content;
    close_out oc 



let effacerLeContenu filePath = Sys.command (">" ^ filePath) (* delete the content in the file with the filePath*)


let set_head _l =  
    
    let repertoire = ".ogit/HEAD" in 
    let s = effacerLeContenu repertoire in 
    if s == 0 then (* the command was executed *)
    let rec iter res liste = match liste with 
    | [] -> res 
    | head :: tail -> iter (res ^ head) tail 
    in 
    write_to_file (Digest.to_hex (iter "" _l)) (repertoire) (* write a readable hash to the file HASH *)
    
    else () 


    
    








(* 
let get_head () = failwith "TODO" 

let make_commit _s  _h =  failwith "TODO"

let init_commit () = failwith "TODO"

let store_commit _c = failwith "TODO"

let read_commit _h = failwith "TODO" 

*)