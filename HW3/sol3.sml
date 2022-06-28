
datatype pattern = Wildcard 
                 | Variable of string 
                 | UnitP 
                 | ConstP of int 
                 | TupleP of pattern list
                 | ConstructorP of string * pattern

datatype valu = Const of int 
              | Unit 
              | Tuple of valu list 
              | Constructor of string * valu


fun check_pat(pat) =
  let fun distinct(xs) = 
    if null xs
    then true
    else let val x = hd xs in
          if List.exists (fn(y) => (x=y)) (tl xs)
          then false
          else distinct(tl xs)
         end
  in
    case pat of
         TupleP(pl) => distinct(foldl(fn(x,a)=>x::a) [] pl)
       | ConstructorP(_,p) => check_pat(p)
       | _ => true
  end

fun match(v, p) =
  case (v,p) of
       (_, Wildcard) => SOME([])
     | (_, Variable(s)) => SOME([(s,v)])
     | (Unit, UnitP) => SOME([])
     | (Const(a), ConstP(b)) => if a = b then SOME([]) else NONE
     | (Tuple(vs), TupleP(ps)) => if length(vs) = length(ps)
                                  then let val xs = ListPair.zip(vs, ps) in
                                          let val ys = List.filter(fn x => match(x) <> NONE) xs in
                                            let fun aux(l, acc) =
                                                case l of
                                                  [] => acc
                                                | x::xs' => aux(xs', acc @ valOf(match(x))) 
                                            in
                                                SOME(aux(ys, []))
                                            end
                                          end
                                      end
                                  else NONE

        
     | (Constructor(s1,v), ConstructorP(s2,p)) => if s1 = s2 then match(v,p)
                                                  else NONE
     | (_, _) => NONE





type name = string

datatype RSP = ROCK
             | SCISSORS
             | PAPER

datatype 'a strategy = Cons of 'a * (unit -> 'a strategy)

datatype tournament = PLAYER of name * (RSP strategy ref)
                    | MATCH of tournament * tournament

fun onlyOne(one:RSP) = Cons(one, fn() => onlyOne(one))
fun alterTwo(one:RSP, two:RSP) = Cons(one, fn() => alterTwo(two, one))
fun alterThree(one:RSP, two:RSP, three:RSP) = Cons(one, fn() => alterThree(two,three, one))

val r = onlyOne(ROCK)
val s = onlyOne(SCISSORS)
val p = onlyOne(PAPER)
val rp = alterTwo(ROCK, PAPER)
val sr = alterTwo(SCISSORS, ROCK)
val ps = alterTwo(PAPER, SCISSORS)
val srp = alterThree(SCISSORS, ROCK, PAPER)

fun next(strategyRef) =
  let val Cons(rsp, func) = !strategyRef in
      strategyRef := func();
      rsp
  end

fun whosWinner(t) =
  let fun win(p1 as PLAYER(n1, s1), p2 as PLAYER(n2, s2)) =
    case (next(s1), next(s2)) of
         (ROCK, ROCK) => win(p1, p2)
       | (ROCK, SCISSORS) => p1
       | (ROCK, PAPER) => p2
       | (SCISSORS, ROCK) => p2
       | (SCISSORS, SCISSORS) => win(p1, p2)
       | (SCISSORS, PAPER) => p1
       | (PAPER, ROCK) => p1
       | (PAPER, SCISSORS) => p2
       | (PAPER, PAPER) => win(p1, p2)
   in
     case t of
         PLAYER(_,_) => t
        |MATCH(p1 as PLAYER(_,_),p2 as PLAYER(_,_)) => win(p1,p2)
        |MATCH(m1,m2) => win(whosWinner(m1), whosWinner(m2))
      end


