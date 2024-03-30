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

  