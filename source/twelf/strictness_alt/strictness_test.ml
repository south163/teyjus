open Strictness_alt
open Printf

(* test case *)
let idtp1 = Lfabsyn.Const "tp1";;
let idtp2 = Lfabsyn.Const "tp2";;
let idtp3 = Lfabsyn.Const "tp3";;
let idtp4 = Lfabsyn.Const "tp4";;


let tp1 = Lfabsyn.IdType idtp1;;
let tp2 = Lfabsyn.IdType idtp2;;
let tp3 = Lfabsyn.IdType idtp3;;
let tp4 = Lfabsyn.IdType idtp4;;


let v1 = Lfabsyn.Var ("v1", tp1);;
let v2 = Lfabsyn.Var ("v2", tp2);;
let v3 = Lfabsyn.Var ("v3", tp3);;
let v4 = Lfabsyn.Var ("v4", tp4);;



let tycon = Lfabsyn.Const "tycon1";;
let tycon' = Lfabsyn.Const "tycon2";;

(* testtp1 = [v1:tp1].[v2:tp2].[v3:tp3]. tycon ({v4:tp4}.v1 v4 v4)(v2) *)
let tm1 = Lfabsyn.AbsTerm (v4 , tp4 , Lfabsyn.AppTerm (v1, [Lfabsyn.IdTerm v4; Lfabsyn.IdTerm v4]));;
let tm2 = Lfabsyn.IdTerm v2;;
let tmlist = [tm1; tm2];;
let testtp = Lfabsyn.PiType (v1, tp1, Lfabsyn.PiType(v2, tp2, (Lfabsyn.PiType (v3, tp3, Lfabsyn.AppType (tycon, tmlist)))));;
let test = find_strict_vars_neg testtp IdSet.empty;;
let () = printf "Test case 1: [v1:tp1].[v2:tp2].[v3:tp3]. tycon ({v4:tp4}.(v1) v4 v4)(v2)\n";;
printf "\tStricts: ";;
printlist (tolist (snd test));
printf "\n\n";;

(* testtp2 = [v1:tp1].[v2:tp2].[v3:tp3]. tycon ({v4:tp4}.v1 v4)(v2) *)
let tm1 = Lfabsyn.AbsTerm (v4 , tp4 , Lfabsyn.AppTerm (v1, [Lfabsyn.IdTerm v4]));;
let tm2 = Lfabsyn.IdTerm v2;;
let tmlist = [tm1; tm2];;
let testtp = Lfabsyn.PiType (v1, tp1, Lfabsyn.PiType(v2, tp2, (Lfabsyn.PiType (v3, tp3, Lfabsyn.AppType (tycon, tmlist)))));;
let test = find_strict_vars_neg testtp IdSet.empty;;
let () = printf "Test case 2: [v1:tp1].[v2:tp2].[v3:tp3]. tycon ({v4:tp4}.v1 v4)(v2)\n";;
printf "\tStricts: ";;
printlist (tolist (snd test));
printf "\n\n";;


(* testtp3 = [v1:tp1].[v2:tp2].[v5: tp5]. tycon' ({v6}. v5 v6)(v2) *)
let tm1 = Lfabsyn.AbsTerm (v4, tp4 , Lfabsyn.AppTerm (v2, [Lfabsyn.IdTerm v4]));;
let tm2 = Lfabsyn.IdTerm v1;;
let tmlist = [tm2; tm1];;
let tp5 = Lfabsyn.AppType (tycon, tmlist);;

let v5 = Lfabsyn.Var ("v5", tp1);;
let v6 = Lfabsyn.Var ("v6", tp1);;
let v7 = Lfabsyn.Var ("v7", tp2);;
let tm3 = Lfabsyn.AbsTerm (v6 , tp1 , Lfabsyn.AppTerm (v5, [Lfabsyn.IdTerm v6]));;
let tm4= Lfabsyn.IdTerm v7;;
let tmlist' = [tm3; tm4];;

let testtp = Lfabsyn.PiType (v1, tp1, Lfabsyn.PiType(v2, tp2, Lfabsyn.PiType(v5, tp5, Lfabsyn.AppType (tycon', tmlist'))));;
let test = find_strict_vars_neg testtp IdSet.empty;;
let () = printf "Test case 3: testtp3 = [v1:tp1].[v2:tp2].[v5: tp5]. tycon' ({v6}. v5 v6)(v2), where v2 appears strict in tp5\n";;
printf "\tStricts: ";;
printlist (tolist (snd test));;
printf "\n\n";;









