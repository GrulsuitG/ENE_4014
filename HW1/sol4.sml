fun digits(x: int) = 
let val xs =[]
  fun parse(x, xs) = 
    if x div 10 = 0
    then x mod 10 :: xs
    else parse(x div 10, (x mod 10)::xs)
  in
    parse(x, xs)
  end
