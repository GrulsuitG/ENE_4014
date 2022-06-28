datatype exp = Constant of int | Negate of exp
          | Add of exp * exp
          | Multiply of exp * exp

fun tosc f e m =
  let fun cotc(f,ee) =
    case ee of
        Constant(i) => if f(i) then 1 else 0
       |Negate(e1) => cotc(f, e1)
       |Add(e1,e2) => cotc(f,e1) + cotc(f,e2)
       |Multiply(e1,e2) => cotc(f,e1) + cotc(f,e2)
  in
    cotc(f, e) >= m
  end


val s = tosc(fn x => x mod 2 = 0) 
val e = Multiply( Add( Constant(2), Constant(1) ), Constant(4) )
