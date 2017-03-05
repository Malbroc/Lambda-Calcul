(* -----------------------TYPE TERME --------------------- *)

type terme =
	Var of string 
	|Abs of string * terme
	|App of terme * terme
;;


(* -------------------TERMES BOOLEENS -------------------- *)

let vrai =
	Abs("x",
		Abs("y",
			Var("x")));;

let faux =
	Abs("x",
		Abs("y",
			Var("y")));;

let non = 
	Abs("x",
		App(App(Var("x"),
				faux),
			vrai));;

let et =
	Abs("x",
		Abs("y",
			App(App(Var("x"),
					Var("y")),
				faux)));;

let ou =
	Abs("x",
		Abs("y",
			App(App(Var("x"),
					vrai),
				Var("y"))));;


(* ---------------------ENTIERS DE CHURCH------------------*)

let zero =
	Abs("s",
		Abs("z",
			Var("z")));;

let suivant =
	Abs("n",
		Abs("s",
			Abs("z",
				App(Var("s"),
						App(App(Var("n"), Var("s")),
								Var("z"))))));;

let plus =
	Abs("x",
		Abs("y",
			App(App(Var("x"),suivant),Var("y"))));;

let multiplication =
	Abs("x",
		Abs("y",
			Abs("z",
				App(Var("x"), App(Var("y"),Var("z"))))));;


(* ------------------------AFFICHAGE----------------------- *)

let rec string_of_terme t = match t with
	|Var(n)->n
	|Abs(n,t1)->"(Lambda "^n^"."^string_of_terme(t1)^")"
	|App(t1,t2)->"("^string_of_terme(t1)^" "^string_of_terme(t2)^")"
;;


(* -------------------OPERATIONS LISTES--------------------- *)

(* x est dans l *)
let rec element x l = match l with
	|[]->false
	|e::[]->e=x
	|e::r->e=x||element x r
;;

(* l1 - l2 *)
let rec diff l1 l2 = match l1 with
	|[]->[]
	|e::[]->if element e l2 then [] else [e]
	|e::r->if element e l2 then diff r l2 else e::diff r l2
;;

(* l1 U l2 *)
let rec union l1 l2 = match l1 with
	|[]->l2
	|e::[]->if element e l2 then l2 else e::l2
	|e::r->if element e l2 then union r l2 else e::union r l2
;;


(* ---------------------OCCURENCES LIBRES-------------------- *)


let rec libre1 t l = match t with
	|Var(n)->n::l
	|Abs(n,t1)->diff (libre1 t1 l) [n]
	|App(t1,t2)->union (libre1 t1 l) (libre1 t2 l)
;;

let libre t = libre1 t [];;


(* ------------------------SUBSTITUTION---------------------- *)


let alphabet = ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j";"k";"l";"m";"n";"o";"p";"q";"r";"s";"t";"u";"v";"w";"x";"y";"z"];;

let rec choisir_lettre alphabet interdit = match alphabet with
	|[]->failwith "ERROR liste vide"
	|e::[]->if (element e interdit) then failwith "No letter left" else e
	|e::r->if (element e interdit) then choisir_lettre r interdit else e
;; 

let rec alpha t v w= match t with
	|Var(n)->if (n=v) then Var(w) else t
	|Abs(n,t1)->if (n=v) then t else alpha t1 v w
	|App(t1,t2)->App(alpha t1 v w, alpha t2 v w)
;;
	 

let rec substitution a x b = match a with
	|Var(v)->if (v=x) then b
						else a (* Var(v) *)
	|App(t1,t2)->App(substitution t1 x b, substitution t2 x b)
	|Abs(v,t1)->if (v=x) then a (* Abs(v,t1) *)
							else 
								if (element v (libre b)) then 
									let w=choisir_lettre alphabet (union (union [v] (libre a)) (libre b)) in
									let tt1= alpha t1 v w in
									Abs(w, substitution tt1 x b)
								else Abs(v, substitution t1 x b)
;;


(* ---------------------FORME NORMALE------------------------ *)

let rec forme_normale t = match t with
	|Var(v)->true
	|Abs(v,t1)->forme_normale t1
	|App(Abs(v,t1),t2)->false
	|App(t1,t2)->forme_normale t1 && forme_normale t2
;;


(* ------------------------OUTER_LEFT------------------------ *)

(* Pre-condition : t contient au moins un redex *)
let rec outer_left t = match t with
	|Var(v)->Var(v) (* dead-code *)
	|Abs(v,t1)->Abs(v, outer_left t1);
	|App(Abs(v,t1),t2)->substitution t1 v t2
	|App(t1,t2)->if (forme_normale t1) then
									App(t1,outer_left t2)
							else App(outer_left t1, t2)
;;


(* ------------------------REDUCTION------------------------- *)

let rec reduction t =
	if (forme_normale t) then
		t
	else
		reduction(outer_left t)
;;


(* --------------------------TESTS--------------------------- *)

let test =
	Abs("x",
		Abs("y",
			App(Var("x"),
					Abs("u",
						Abs("v",
							App(Var("v"), Var("y")))))));;

let tset =
	Abs("u",
		Var("u"));;

let test2=
	App(test,tset);;

let test3=
	App(test2,
		Abs("z",
			Abs("w",
				Var("z"))));;


(*print_string(string_of_terme test3^"\n");;
print_string(string_of_terme (reduction test3)^"\n");;*)

let trois=
	App(
		App(plus,
				App(suivant,App(suivant, zero))),
		App(suivant, zero));;

let deux=
	App(suivant,App(suivant, zero));;

let six=
	App(
		App(multiplication,(reduction deux)),(reduction trois));;
		

(*print_string(string_of_terme (reduction trois)^"\n");;
print_string(string_of_terme (reduction deux)^"\n");;
print_string(string_of_terme (reduction six)^"\n");;*)


