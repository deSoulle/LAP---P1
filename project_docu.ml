(**@author Afonso Sousa, 65548 
   @author Joana Santos, 65542*)

(*/ PASCAL TRAINGLE \*)

(**[n_k_element n k] returns the value given the line and position in a pascal triangle;
    works recursively. [n = k], [n = 0], [k = 0] are the border positions of the triangle, value is 1;
    determines the value in the given position by calculating the values before it;  
    in the pascal triangle a number is definied by the sum of the two number above it;
    "n_k_element (n-1) (k-1)" is the number in the line above one position before;
    "n_k_element (n-1) k" is the number directly above;
    the two numbers used in the sum are also calculated through the same process; 
    *)
let rec n_k_element n k =
  if n = k || k = 0 || n = 0 then 1 
  else n_k_element (n-1) (k-1) + n_k_element (n-1) k;;

(** [build_line l p] prints the [l]-line of the triangle;
    this command is always called with [p = 0] so the first number printed is the first in the line;
    after printing the number, checks if there are still more positions to print in the line (l > p);
    TRUE:calls itself with [p + 1] to print the next position in the line;
    FALSE: terminates the recursive by printing a new line so the next layer of the triangle is printed below;
    *)
let rec build_line l p = 
  print_string (string_of_int (n_k_element l p)^" ");
  if l > p then build_line l (p+1)
  else print_endline "";;

(**[build_triangle n] builds a pascal triangle with [n]-lines;
    given [n] as the number of lines, the first if recalls the function all the way to [n = 0];
    when [n = 0] prints an empty line and proceeds to build the top layer of the triangle;
    then the following lines are built (this is done so the triangle is built from to the top to the bottom)
    *)
let rec build_triangle n =
  if n > 0 then build_triangle(n-1)
  else print_endline "";
  build_line n 0;;


(*############################################################################*)   

(* # TILES # *)

(**[tiles n] calculates the number of combinations of red and black tiles given a [n] number of tiles ;
    RULE: red tiles need to be in 3-tile blocks;
    so if [0 <= n < 3] there can't be any red tiles -> 1 combination (all black);
    if [n = 3] either all black or all red tiles;
    

    
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