(* 1 Patterns *)

(* Must Do *)
(*ATTENTION We must be able to run your functions separately without encountering any errors*)
let build_line n str = 
	if n <= 0 then 
		invalid_arg "n <= 0"
	else 
		let rec build n str =  (* n se vide pour avoir le nombre correcte de caracteres*)
			if n = 1 then 
				str 
			else 
				str ^ build (n-1) str
		in 
 		build n str;; 

let square n str =
	if n <= 0 then 
	invalid_arg "n <= 0" 
	else  
		let rec main_square i str = (* i est dans tous les cas <= n *)
			if i = n then 
				(build_line n str) ^ "\n"
			else 
				(build_line n str) ^ "\n" ^ main_square (i+1) str
		in 
		print_string(main_square 1 str);;

let square2 n (str1, str2) = 
	if n <= 0 then 
		invalid_arg "n <= 0"
	else 
		let rec main_square2 (str1, str2) i = (* i est dans tous les cas <= n *)
			if i = n then 
				build_line n (str1 ^ str2) ^ "\n"
			else 
				(build_line n (str1 ^ str2)) ^ "\n" ^ (main_square2 (str2, str1) (i+1))
		in 
	print_string(main_square2 (str1, str2) 1);;

let triangle n str =  (* Meme logique que pour le square main en remplacant le n par i dans l'appelle de build_line *)
	if n <= 0 then 
		invalid_arg "n <= 0"
	else 
		let rec main_triangle i str = 
			if i = n then 
				(build_line i str) ^ "\n"
			else 
				(build_line i str) ^ "\n" ^ main_triangle (i+1) str
		in 
		print_string(main_triangle 1 str);;
		
(* BONUS *) 
let pyramide n strs =
  if n <= 0 then
    invalid_arg "n <=0 "
  else
    let center   pos (str1, str2) =
    (*str1 n-pos fois puis str2 2pos fois puis str1 n-pos fois*)
      (build_line (n-pos) str1) ^ (build_line (2*pos) str2) ^ (build_line (n-pos)
                                                                 str1)^"\n"
    in
    let rec main_pyramide (str1, str2)  i =
      match i with
          _ when i = n -> build_line n str2 ^ "\n" 
        |_ when i = (n-1) ->  center i strs
        |_-> center i (str1,str2) ^ main_pyramide  (str1,str2) (i+1)
    in
    print_string(main_pyramide strs 1);;

pyramide 5 (".", "*");;
