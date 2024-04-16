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

type id = string;

datatype binop = Plus | Minus | Times | Div;

datatype stm = CompoundStm of stm * stm
	     | AssignStm of id * exp
	     | PrintStm of exp list

     and exp = IdExp of id
	     | NumExp of int
             | OpExp of exp * binop * exp
             | EseqExp of stm * exp;

val prog = CompoundStm(AssignStm("a",OpExp (NumExp 5,Times,NumExp 5)), PrintStm [IdExp "a"]);

type table = (id * int) list 

fun update (tb,id,int) = (id,int) :: tb

fun lookup (tb:table,id:id) : int =
    case tb of 
	nil => 0
      | ((id',v) :: tb') =>
	if id = id' then v
	else lookup (tb',id)




fun print_op (b:binop) : unit = 
    case b of 
	Plus => print "+"
      | Minus => print "-"
      | Times => print "*"
      | Div => print "/"

fun print_exp (e:exp) : unit =
    case e of
	IdExp id => print id
      | NumExp n => print (Int.toString n)
      | OpExp (e1,b,e2) => (print_exp e1; print_op b; print_exp e2)
      | EseqExp (_,e1) => print_exp e1

fun interpExp(tb: table, NumExp(v)) = v
  | interpExp(tb: table, EseqExp(s, e)) = (interpStm(tb, s); interpExp(tb, e))
  | interpExp(tb: table, OpExp(v1, pp, v2)) =
    let
        val leftVal = interpExp(tb, v1);
        val rightVal = interpExp(tb, v2);
    in
        case pp of
            Plus => leftVal + rightVal
          | Minus => leftVal - rightVal
          | Times => leftVal * rightVal
          | Div => if rightVal <> 0 then leftVal div rightVal else 0
    end
  | interpExp(tb: table, IdExp(v)) = lookup(tb, v)

and interpStm(tb:table, CompoundStm(l, r)) = interpStm(tb, l) + interpStm(tb, r)
| interpStm(tb:table, AssignStm(id ,exp)) = interpExp(tb, exp)
| interpStm(tb:table, PrintStm(exp::exps)) = (print_exp(exp); 0);

interpStm([], prog);