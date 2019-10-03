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