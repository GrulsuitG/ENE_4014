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
  

