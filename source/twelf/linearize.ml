(** linearization of translated clauses (restricted Lambda Prolog) **)

(* makeApp: takes a head term `h' and a list of argument terms `a1', `a2', ..., `an' 
            and returns an application term `(((h a1) a2) ... an)'
*)
let makeApp h args =
  List.fold_left (fun t a -> Absyn.ApplicationTerm(Absyn.CurriedApplication(t,a),Errormsg.none))
                 h
                 args
(* makeImp: given terms for left and right side of implication, 
   construct an application term representing (imp left right). *)
let makeImp l r =
  makeApp (Absyn.ConstantTerm(Pervasive.implConstant, [], Errormsg.none)) [l;r]

(* makeAll: given a typesymbol and a body, create an universall 
   quantification over that symbol with the supplied term as the 
   body. *)
let makeAll tysymb body =
  makeApp (Absyn.ConstantTerm(Pervasive.allConstant, [], Errormsg.none))
          [Absyn.AbstractionTerm(Absyn.NestedAbstraction(tysymb, body), Errormsg.none)]
    
(* makeEqn : Absyn.atypesymbol -> Absyn.atypesymbol -> Absyn.aterm
   given two variables x and y, produces the term form of equality for 
   these variables (x = y) *)
let makeEqn x y =
  makeApp (Absyn.ConstantTerm(Pervasive.eqConstant, [], Errormsg.none))
          [Absyn.makeBoundVarTerm x Errormsg.none;
           Absyn.makeBoundVarTerm y Errormsg.none]

    
(* finalize : Absyn.atypesymbol list -> Absyn.aterm list -> 
              Absyn.aterm -> Absyn.aterm
   given (1) a list of newly introduced variables, (2) a list of terms 
         representing the equalies on variables to be added, and (3) 
         the base term to be extended
   returns the final modified term representing a clause.

   eg. 
   finalize (x_1,...,x_n) ((y_1=z_1),...,(y_m=z_m)) t 
   will be the term
   all x_1 ... all x_n (y_1 = z_1) -> ... -> (y_m = z_m) -> t *)
let finalize newvars eqns t =
  List.fold_left (fun tm tysymb -> makeAll tysymb tm)
                 (List.fold_left (fun tm eq -> makeImp eq tm) t eqns)
                 newvars

(* lh_t : Absyn.atypesymbol Table.SymbolTable.t -> Absyn.atypesymbol Table.SymbolTable.t -> 
          Absyn.atypesymbol Table.SymbolTable.t -> Absyn.atypesymbol list ->
          Absyn.aterm -> (Absyn.atypesymbol Table.SymbolTable.t * Absyn.atypesymbol
          list * Absyn.aterm * Absyn.aterm list)
   given (1) the universally quantified variables, (2) the subset of
          (1) which have already been seen, (3) the other variables in
          scope, (4) the new variables that have been introduced thus
          far, and (5) the term to be modified such that each variable
          only appears once
   returns (1) an updated list of the universally quantified vraiables
          which have been seen, (2) an updated list of any new
          variables that have been introduced, (3) the modified term,
          and (4) a list of terms representing the equalities on
          variables required by the transformation. *)



(* lf_c : Absyn.atypesymbol Table.SymbolTable.t -> Absyn.aterm -> Absyn.aterm
   given (1) the universally quantified variables in scope and (2) a 
         term representation of a clause to be transformed,
   returns the modified term. *)



(* linearize : Absyn.aterm -> Absyn.aterm 
   given a term representation of a clause applies the linear head
   transformation and returns the resulting term. *)

let linearize t = t
