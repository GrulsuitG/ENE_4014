fun pi(a: int, b: int, f:(int->int)) =
  if a>b
  then 1
  else pi(a+1,b,f)*f(a)

