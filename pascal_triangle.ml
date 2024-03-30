let rec n_k_element n k =
  if n = k || n = 0 || k = 0 then 1
  else n_k_element (n-1)(k-1) + n_k_element (n-1)k ;;

let int k = 0;;

let rec build_triangle n =
  print_int (n_k_element n k) 
