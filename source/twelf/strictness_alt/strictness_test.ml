open Strictness_alt
open Printf
open Lfabsyn

(* test case *)
let idtp1 = Const "tp1";;
let idtp2 = Const "tp2";;
let idtp3 = Const "tp3";;
let idtp4 = Const "tp4";;

let tp1 = IdType idtp1;;
let tp2 = IdType idtp2;;
let tp3 = IdType idtp3;;
let tp4 = IdType idtp4;;

let v0 = Var ("v0", tp1);;
let v1 = Var ("v1", tp1);;
let v2 = Var ("v2", tp2);;
let v3 = Var ("v3", tp3);;
let v4 = Var ("v4", tp4);;



let tycon = Const "tycon1";;
let tycon' = Const "tycon2";;

(* testtp1 = [v1:tp1].[v2:tp2].[v3:tp3]. tycon ({v4:tp4}.v1 v4 v4)(v2) *)
let tm1 = AbsTerm (v4 , tp4 , AppTerm (v1, [IdTerm v4; IdTerm v4]));;
let tm2 = IdTerm v2;;
let tmlist = [tm1; tm2];;
let testtp = PiType (v1, tp1, PiType(v2, tp2, (PiType (v3, tp3, AppType (tycon, tmlist)))));;
let test = find_strict_vars_neg testtp IdSet.empty;;
let () = printf "Test case 1: [v1:tp1].[v2:tp2].[v3:tp3]. tycon ({v4:tp4}.(v1) v4 v4)(v2)\n";;
printf "\tStricts: ";;
printlist (tolist (snd test));
printf "\n\n";;

(* testtp2 = [v1:tp1].[v2:tp2].[v3:tp3]. tycon ({v4:tp4}.v1 v4)(v2) *)
let tm1 = AbsTerm (v4 , tp4 , AppTerm (v1, [IdTerm v4]));;
let tm2 = IdTerm v2;;
let tmlist = [tm1; tm2];;
let testtp = PiType (v1, tp1, PiType(v2, tp2, (PiType (v3, tp3, AppType (tycon, tmlist)))));;
let test = find_strict_vars_neg testtp IdSet.empty;;
let () = printf "Test case 2: [v1:tp1].[v2:tp2].[v3:tp3]. tycon ({v4:tp4}.v1 v4)(v2)\n";;
printf "\tStricts: ";;
printlist (tolist (snd test));
printf "\n\n";;


(* testtp3 = [v1:tp1].[v2:tp2].[v5: tp5]. tycon' ({v6}. v5 v6)(v2) *)
let tm0 = AbsTerm (v0, tp1 , AppTerm (v1, [IdTerm v0]));;
let tmlist = [tm0];;
let tp2 = AppType (tycon, tmlist);;
let v2 = Var ("v2", tp2);;


let tm1 = AbsTerm (v4, tp4 , AppTerm (v2, [IdTerm v4]));;
let tm2 = IdTerm v1;;
let tmlist' = [tm2; tm1];;
let tp5 = AppType (tycon, tmlist');;

let v5 = Var ("v5", tp1);;
let v6 = Var ("v6", tp1);;
let v7 = Var ("v7", tp2);;
let tm3 = AbsTerm (v6 , tp1 , AppTerm (v5, [IdTerm v6]));;
let tm4= IdTerm v7;;
let tmlist'' = [tm3; tm4];;

let testtp = PiType (v1, tp1, PiType(v2, tp2, PiType(v5, tp5, AppType (tycon', tmlist''))));;
let test = find_strict_vars_neg testtp IdSet.empty;;
let () = printf "Test case 3: testtp3 = [v1:tp1].[v2:tp2].[v5: tp5]. tycon' ({v6}. v5 v6)(v2), where v2 appears strict in tp5, 
					v1 appears strict in tp2\n";;
printf "\tStricts: ";;
printlist (tolist (snd test));;
printf "\n\n";;



(* testtp4 = [v1:tp1].[v2:tp2].[v3:tp3]. tycon ({v4:tp4}.v1 v4)(v2) *)
let () = printf "Test case 4:  ";;




(*nat: type*)
let nat_tp = IdType (Const "nat");;
let nat_nat_tp = ImpType (nat_tp, nat_tp);;

let zero_id = Var ("zero", nat_tp);;
let succ_id = Var ("succ", nat_nat_tp);;

(*z: nat*)
let zero_tm = IdTerm zero_id;;
(*s : nat -> nat*)
let succ_tm = IdTerm succ_id;;

(*natFun : (nat -> nat -> nat) -> type*)
let natFun_tp = ImpType((ImpType (nat_tp, ImpType (nat_tp, nat_tp))),
                                (IdType (Const "natFun")));;

(*list : type*)
let list_tp = IdType (Const "list");;

(*nil : list*)
let nil_tm = IdTerm (Var ("nil", list_tp));;


let nat_list_list_tp = ImpType (nat_tp, ImpType (list_tp,list_tp));;
(*cons : nat -> list -> list*)
let cons_id = Var("cons", nat_list_list_tp);;

(*mapFun: (nat -> nat) -> list -> list -> type*)
let mapFun_tp = ImpType((ImpType (nat_nat_tp, ImpType (list_tp,list_tp))),
                                 (IdType (Const "mapFun")));;

(*b : (nat -> nat) -> type*)
let b = Const "b";;
let w1 = Var("w1", nat_nat_tp);;
let w2 = Var ("w2", ImpType(nat_nat_tp, (ImpType (nat_tp, nat_tp))));;

let c = Const "c";;
let d = Const "d";;
let xx = Var ("X", nat_nat_tp);;
let zz = Var ("Z", (AppType (b, [IdTerm xx])));;
let y = Var ("y", nat_tp);;
let w = Var ("w", nat_nat_tp);;
let sndtm =  AbsTerm(w, nat_nat_tp, (AbsTerm(y, nat_tp, 
				(AppTerm(xx, [AppTerm (w, [IdTerm y])])))));; 
let typeofyy = PiType(zz, AppType(b, [IdTerm xx]), AppType (c, 
		[AbsTerm(y, nat_tp, IdTerm y); sndtm; IdTerm zz]))
let yy = Var ("Y", typeofyy);;

let f = PiType(xx, nat_nat_tp, (PiType(yy, typeofyy, (AppType(d, 
			[AbsTerm(y, nat_tp,IdTerm y); sndtm; IdTerm yy])))));;

printf "%s\n" (string_of_typ f);;
let test = find_strict_vars_neg f IdSet.empty;;
printf "\tStricts: ";;
printlist (tolist (snd test));;
printf "\n\n";;


let () = printf "Test case 5 and 6:  \n";;

let nat_nat_nat_tp = ImpType (nat_tp, ImpType (nat_tp, nat_tp));;

let x = Var ("x", nat_tp);;
let y = Var ("y", nat_tp);;
let xx = Var("X", nat_nat_nat_tp);;

let t1 = PiType(xx, nat_nat_nat_tp, AppType(c, [AbsTerm(x, nat_tp, (AbsTerm(y, nat_tp, AppTerm(xx, [IdTerm y]))))]));;
printf "%s\n" (string_of_typ t1);;
let test = find_strict_vars_neg t1 IdSet.empty;;
printf "\tStricts: ";;
printlist (tolist (snd test));;
printf "\n\n";;


let z = Var ("z", nat_tp);;
let t2 = PiType(xx, nat_nat_nat_tp, AppType(c, [AbsTerm(x, nat_tp, (AbsTerm(y, nat_tp, AppTerm(xx, [IdTerm y; IdTerm z]))))]));;
printf "%s\n" (string_of_typ t2);;
let test = find_strict_vars_neg t2 IdSet.empty;;
printf "\tStricts: ";;
printlist (tolist (snd test));;
printf "\n\n";;


let () = printf "Test case 7:  \n";;
let ff = Var ("F", nat_nat_tp);;
let mapNil = PiType(ff, nat_nat_tp, AppType (c, [AbsTerm (x, nat_tp, 
							(AppTerm(ff, [IdTerm x]))); nil_tm; nil_tm]));;

printf "%s\n" (string_of_typ mapNil);;
let test = find_strict_vars_neg mapNil IdSet.empty;;
printf "\tStricts: ";;
printlist (tolist (snd test));;
printf "\n\n";;


let () = printf "Test case 8:  \n";;
let ff = Var ("F", nat_nat_tp);;
let l1 = Var ("l1", list_tp);;
let l2 = Var ("l2", list_tp);;
let xx = Var ("X", nat_tp);;
let fstt = AppType (c, [(AbsTerm (x, nat_tp, 
							(AppTerm(ff, [IdTerm x])))); IdTerm l1; IdTerm l2]);;

let sndt = AppType (c, [(AbsTerm (x, nat_tp, (AppTerm(ff, [IdTerm x])))); 
						(AppTerm (cons_id, [IdTerm xx; IdTerm l1])); 
						(AppTerm (cons_id, [(AppTerm(ff, [IdTerm xx])); IdTerm l2]))]);;

let mapCons = PiType(ff, nat_nat_tp, (PiType (l1, list_tp, (PiType (l2, list_tp,  
					(PiType(xx, nat_tp, (ImpType(fstt, sndt)))))))));; 



printf "%s\n" (string_of_typ mapCons);;
let test = find_strict_vars_neg mapCons IdSet.empty;;
printf "\tStricts: ";;
printlist (tolist (snd test));;
printf "\n\n";;

let gm = IdSet.add ff (IdSet.add l1 (IdSet.add l2 (IdSet.singleton xx)));;
let test = find_strict_vars_term (AppTerm (cons_id, [IdTerm xx; IdTerm l1])) gm IdSet.empty;;
printf "\tStricts in term: ";;
printlist (tolist (test));;
printf "\n\n";;




