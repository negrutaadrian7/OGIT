type t = 
| Text of string 
| Directory of (string * bool * Digest.t * t) list

let test my_string = if my_string == true then "d" else "t" 

let rec liste liste_a_analyser = match liste_a_analyser with
  | [] -> []
  | (name, dir, hash1, _) :: tail -> [name ^ ";" ^ (test dir) ^ ";" ^ Digest.to_hex hash1] @ liste tail (* string list *)

let produce_string liste_finale = String.concat "\n" (liste liste_finale)

let hash objet = match objet with 
  | Text s ->  Digest.string s
  | Directory x -> Digest.string (produce_string x)


(** teste si un objet, identifié par son hash, est présent dans .ogit/objects/ **)
let is_known objet  = 
  if Sys.file_exists (".ogit/objects/" ^ Digest.to_hex objet ) then true else false 

(** écrit l'objet dans un fichier dans .ogit/objects/ renvoie le hash de cet objet **)

let creation_fichier objet =  Sys.command ("touch .ogit/objects/" ^ Digest.to_hex(hash objet)) (* return the exit code *)

(* Creation du contenu d'un objet pour etre stocke dans un fichier *)

(* Ecriture dans le fichier *)
let write_to_file content file = 
  let oc = open_out file in 
  output_string oc content;
  close_out oc 

let produce_content objet = match objet with 
  | Text s -> if creation_fichier (Text s) == 0 then write_to_file s (".ogit/objects/" ^ Digest.to_hex (hash (Text s)))  
  | Directory [] -> if creation_fichier (Directory []) == 0 then write_to_file (produce_string ([])) (".ogit/objects/" ^ Digest.to_hex (hash (Directory [])))  
  | Directory x -> if creation_fichier (Directory x ) == 0 then write_to_file (produce_string (x)) (".ogit/objects/" ^ Digest.to_hex (hash (Directory x)))    

let best_work obj = 
  match obj with 
  | Text s -> begin produce_content (Text s); (Text s) end
  | Directory [] -> begin produce_content (Directory []); (Directory []) end
  | Directory x -> begin produce_content (Directory x); (Directory x) end 


(* doit renvoyer un Digest.t *)
let store_object _obj = match _obj with 
  | Text s -> hash (best_work (Text s))
  | Directory [] -> hash (best_work (Directory []))
  | Directory x ->  hash (best_work(Directory x))


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




(* attend un Digest.t *)
let read_text_object hash_obj  = 
  lecture_fichier (".ogit/objects/" ^ Digest.to_hex (hash_obj)) (* le hash est donne *)



(*==============================================================================================================*)


(* Efface le contenu qui commence avec un point  *)
let filter_content _array = 
  let x = ref 0 in 
  let arr = Array.make (Array.length _array) "" in 
  
  for i = 0 to (Array.length _array) - 1 do 
    let actual = _array.(i) in 
    if (actual.[0] <> '.') then begin arr.(!x) <- actual; x := !x + 1 end 
  done;

  arr (* fait retourner l'array sans les fichier ou dossier qui commence par un "." *)
  



let store_work_directory () = 
  
  let rec iter pattern = 
    let content = filter_content (Sys.readdir pattern) in 
    let liste_finale = ref [] in 
    
    for i = 0 to (Array.length content) - 1 do 
      if (Sys.is_directory (pattern ^ content.(i)) == false) then (* les fichier text *)
        liste_finale := (content.(i), false, Digest.to_hex (store_object (Text (read_text_object (pattern ^ content.(i))))), Text (lecture_fichier (pattern ^ content.(i)))) :: !liste_finale
      else
        liste_finale := (content.(i), true, Digest.to_hex (store_object (Directory (iter (pattern ^ content.(i) ^ "/")))), Text "") :: !liste_finale 
      done;
      List.rev (!liste_finale) (* On a besoin pour afficher l'hierarchie des le debut *)
    
    in  
    (store_object (Directory (iter (".")))) 


    
let produce_cont filename = 
  let res = ref [] in
  let openfile = open_in filename in 
  try
    while true do 
     
      res := input_line openfile :: !res
    done; 
  close_in openfile;
  assert false
  with 
  | End_of_file -> !res


let spliting element = String.split_on_char ';' element 


let rec read_directory_object hash_objet = (* we return a type t that means that we'll put in the final at the end point of this function all the content of the file in Directory constructor *)
  let openfile = open_in (".ogit/objects/" ^ hash_objet) in 
  let res = ref [] in (* resultat *)
  
  let triplets liste = 
    match liste with 
    | [name;dir;hash] -> let (dir_or_not, content) = if dir = "t" then (false, Text (read_text_object (Digest.from_hex (hash)))) else (true, read_directory_object (Digest.from_hex (hash))) in (name, dir_or_not, hash, content) 
    | _ -> invalid_arg "" in 
  
  try
    while true; do
      res := (triplets (spliting (input_line openfile))) :: !res 
    done;
    Directory (List.rev !res) 
  with 
  |End_of_file -> close_in openfile; Directory (List.rev !res) 
  
 


























  





