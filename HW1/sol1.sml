fun merge(xs: int list, ys: int list) =
  if null xs
  then 
    if null ys
    then []
    else ys
  else
    if null ys
    then xs
    else 
      if hd xs < hd ys
      then hd xs :: merge(tl xs, ys)
      else hd ys :: merge(xs, tl ys)


fun reverse(xs : int list) = 
  let val x = []
    fun parse(xs,x) =
      if null xs
      then x
      else parse(tl xs, hd xs::x) 
  in
    parse(xs, x)
  end 
    
fun pi(a: int, b: int, f:(int->int)) =
  if a>b
  then 1
  else pi(a+1,b,f)*f(a)

fun digits(x: int) = 
let val xs =[]
  fun parse(x, xs) = 
    if x div 10 = 0
    then x mod 10 :: xs
    else parse(x div 10, (x mod 10)::xs)
  in
    parse(x, xs)
  end

fun additivePersistence(x: int) =
  let val n =0
    fun sum(xs) =
      if null xs
      then 0
      else sum(tl xs) + hd xs
    fun time(x, n) =
      if x div 10 = 0
      then n
      else time(sum(digits(x)), n+1)
  in
    time(x, n)
  end

fun digitalRoot(x: int) =
  let fun sum(xs) = 
        if null xs
        then 0
        else sum(tl xs) + hd xs
    fun root(x) =
      if x div 10 =0
      then x mod 10
      else root(sum(digits(x)))
  in
    root(x)
  end
  


  

  
