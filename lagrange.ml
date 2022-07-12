let rec get_ith list i = match (list,i) with
  | ((x::xs),0) -> x
  | ((x::xs),i) -> get_ith xs  (i-1) ;;

let rec length list = match list with
  | [] -> 0
  | x::xs -> (length xs) + 1

let lagrange_term arg j k list_x= (arg -. (get_ith list_x k)) /. ((get_ith list_x j) -. (get_ith list_x k));;             

let rec lagrange_poly arg j list_x product iteration =
  
  if j == iteration then lagrange_poly arg j list_x product (iteration+1) 
  else
  if iteration == length list_x then product
  else
    lagrange_poly arg j list_x (product *. (lagrange_term arg j iteration list_x) ) (iteration + 1) ;;

let rec separate_x dot_list =
  match dot_list with
  | [] -> []
  | dot::dots -> (
      match dot with 
      | (x,y) -> [x] @ (separate_x dots)
    )
    
let rec separate_y dot_list =
  match dot_list with
  | [] -> []
  | dot::dots -> (
      match dot with 
      | (x,y) -> [y] @ (separate_y dots)
    )    
    
    
let rec sum_of_products iteration y_list x_list sum arg=
  if iteration == length x_list then sum
  else sum_of_products (iteration + 1) y_list x_list (sum +. ((get_ith y_list iteration) *. (lagrange_poly arg iteration x_list 1. 0))) arg
  
         
let rec lagrange dots arg =
  sum_of_products 0 (separate_y dots) (separate_x dots) 0. arg
  
  
  
  
  
  
  
  
  
  
  
  

             
