
let rec tiles n =
  if n < 3 && n >= 0 then 1 
  else if n = 3 then 2 
  else tiles (n-1) + red_tiles (n-3);  
  and red_tiles n = 
    let rec red_tiles' x count = 
      if x = 0 then count 
      else red_tiles' (x-1) (count+tiles(x-1)) 
    in red_tiles' n 1;;