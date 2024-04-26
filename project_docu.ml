(**[n_k_element n k] *)
let rec n_k_element n k =
  if n = k || k = 0 || n = 0 then 1 
  else n_k_element (n-1) (k-1) + n_k_element (n-1) k;;

let rec build_line l p = 
  print_string (string_of_int (n_k_element l p)^" ");
  if l > p then build_line l (p+1)
            else print_endline "";;

let rec build_triangle n =
  if n > 0 then build_triangle(n-1)
           else print_endline "";
  build_line n 0;;

      

(**[tiles n] 
    *)
let rec tiles n =
  if n < 3 && n >= 0 then 1 
  else if n = 3 then 2 
  else tiles (n-1) + red_tiles (n-3);  
  and red_tiles n = 
    let rec red_tiles' x count = 
      if x = 0 then count 
      else red_tiles' (x-1) (count+tiles(x-1)) 
    in red_tiles' n 1;;