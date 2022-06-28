fun reverse(xs : int list) = 
  let val x = []
    fun parse(xs,x) =
      if null xs
      then x
      else parse(tl xs, hd xs::x) 
  in
    parse(xs, x)
  end 
    
  

  
