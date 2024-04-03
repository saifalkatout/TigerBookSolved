type id = string

datatype binop = Plus | Minus | Times | Div

datatype stm = CompoundStm of stm * stm
	     | AssignStm of id * exp
	     | PrintStm of exp list

     and exp = IdExp of id
	     | NumExp of int
             | OpExp of exp * binop * exp
             | EseqExp of stm * exp
val prog = 
 CompoundStm(AssignStm("a", OpExp(NumExp 1, Plus, NumExp 1)), PrintStm [NumExp 1])


(* Program 1 *)
fun maxargs (CompoundStm(left, right)) = Int.max(maxargs(left), maxargs(right))
  | maxargs (AssignStm(_, exp)) = expargs(exp)
  | maxargs (PrintStm(exps)) = Int.max(length(exps), prnt(exps))

and expargs (EseqExp(stm, exp)) = Int.max(maxargs(stm), expargs(exp))
  | expargs (OpExp(left, _, right)) = Int.max(expargs(left), expargs(right))
  | expargs (_) = 0

and prnt ([]) = 0
  | prnt (e::t) = Int.max(expargs(e), prnt(t))


(* Program 2 *)

fun interpStm(CompoundStm(l, r)) = interpStm(l) + interpStm(r)
| interpStm(AssignStm(id ,exp)) = (id, interpExp(exp))
| interStm(PrintStm(exps)) = interpExp

and interpExp()