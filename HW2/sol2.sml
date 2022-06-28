datatype expr = 
      NUM of int
    | PLUS of expr * expr
    | MINUS of expr * expr

datatype formula = 
      TRUE
    | FALSE
    | NOT of formula
    | ANDALSO of formula * formula
    | ORELSE of formula * formula
    | IMPLY of formula * formula
    | LESS of expr * expr

fun eval(x) =
  case x of
       TRUE => true
     | FALSE => false
     | NOT(a) => not (eval (a))
     | ANDALSO(a,b) => eval(a) andalso eval(b)
     | ORELSE(a,b) => eval(a) orelse eval(b)
     | IMPLY(a,b) => if eval(a) then eval(b) else true
     | LESS(a,b) => 
         let fun calc(n) =
            case n of
              NUM(i) => i
            | PLUS(i,j) => calc(i) + calc(j)
            | MINUS(i,j) => calc(i) - calc(j)
          in
            if calc(a) < calc(b) then true else false
         end



type name = string

datatype metro = STATION of name
                |AREA of name *metro
                |CONNECT of metro * metro

fun checkMetro(x) =
  let fun check(name, metro) =
    case metro of
         STATION(n) => name = n 
       | AREA(n, CONNECT(m1,m2)) => if check(n, m1) 
                                    then if check(n, m2)
                                         then true
                                         else check(name, m2)
                                    else if check(name, m1)
                                         then if check(n, m2)
                                              then true
                                              else check(name, m2)
                                         else false
                                          
       | AREA(n, m) => check(n,m) orelse check(name, m)
       | CONNECT(m1, m2) => check(name, m1) andalso check(name,m2)

  in
    case x of
        AREA(n, m) => check(n, m)
       |_ => false
  end

val t1 = AREA("a", STATION "a")
val t2 = AREA("a", AREA("a", STATION "a"))
val t3 = AREA("a", AREA("b", CONNECT(STATION "a", STATION "b")))
val t4 = AREA("a", CONNECT(STATION "a", AREA("b", STATION "a")))

val t5 = AREA("a", STATION "b")
val t6 = AREA("a", AREA("a", STATION "b"))
val t7 = AREA("a", AREA("b", CONNECT(STATION "a", STATION "c")))
val t8 = AREA("a", CONNECT(STATION "b", AREA("b", STATION "a")))
val t9 = AREA("a", CONNECT(STATION "a", AREA("b", STATION "c")))

datatype 'a lazyList = nullList
                     | cons of 'a * (unit -> 'a lazyList)


fun seq(first, last) =
  if first = last
  then cons(last, fn() => nullList)
  else cons(first, fn() => seq(first+1, last))


fun infSeq(first) =
    cons(first, fn() => infSeq(first+1))

(*fun infSeq2(firsst) =
  let fun append() =
      infSeq2(first+1)
  in
    cons(first, append())
  end*)

fun firstN(lazyListVal, n) =
  let fun aux(llist ,cnt, acc) =
      if cnt = n
      then acc
      else 
       case llist of
            nullList => acc
          | cons(x, xs) => aux(xs(), cnt+1, acc @ [x])
  in
    aux(lazyListVal, 0, []) 
  end
    
fun Nth(lazyListVal, n) =
  let fun aux(llist, cnt, acc) =
    if cnt = n
    then SOME(acc)
    else 
      case llist of
           nullList => NONE
         | cons(x, xs) => aux(xs(), cnt+1, x)
  in
    aux(lazyListVal, 0, 0)
  end

fun filterMultiples(lazyListVal, n) =
    case lazyListVal of
         nullList => nullList
       | cons(x, xs) => if x mod n = 0
                        then filterMultiples(xs(), n)
                        else cons(x, fn() => filterMultiples(xs(), n))

fun primes() =
  let fun sieve(lazyListVal) =
      case lazyListVal of
           nullList => nullList
         | cons(x, xs) => cons(x, fn() => filterMultiples(sieve(xs()), x))
  in
    sieve(infSeq(2))
  end


