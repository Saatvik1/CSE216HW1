(* This is an OCaml editor.
   Enter your program here and send it to the toplevel using the "Eval code"
   button or [Ctrl-e]. *)

(*Question 1*)


let rec pow = fun x n -> 
  if n == 0 then 1 else (x * (pow (x) (n-1)));;


let rec float_pow = fun x n -> 
  if n == 0.0 then 1.0 else (x *. (float_pow (x) (n-.1.0)));; 


(*Question 2*)

let rec compress list =
  match list with 
  | [] -> []
  | h::(hot :: _ as huh) -> 
      if h = hot then compress huh else h::compress huh
  | list -> list
;;

  
(*Question 3*)
  
let rec remove_if (list: 'a list) (input : ('a -> bool)) = 
  match list with 
  | [] -> []
  | h::t -> if (input h) = false then h::(remove_if t input) else remove_if t input
;;


(*Question 4*)
  
let rec slice list x y = match list with 
  | [] -> []
  | h::t -> 
      if x > y then [] else 
      if x > 0 then slice (t) (x-1) (y-1) else 
      if (x==0 && y>0) then h :: slice (t) (x) (y-1) else 
      if y < 0 then [] else []
;;


(*Question 5*)  

let rec equivs (f: 'a -> 'a -> bool ) (list: 'a list ) = 
  let rec deleteInList list3 list4 = match list3 with 
    | [] -> list4 
    | h3::t3 -> 
        begin
          let temp =   (remove_if list4 ((=)h3)) in
          deleteInList t3 temp;
        end
  in 
  let rec deleteExtra list1 item1 = match list1 with 
    |[] -> []
    | h2::t2 -> if item1 == h2 then deleteExtra t2 item1 else h2::deleteExtra t2 item1 
  in 
  let rec helper (item: 'a) (list2: 'a list) = match list2 with 
    | [] -> []
    | h1::t1 -> if f item h1 then (item::(h1::[])@(helper item t1)) else helper item t1 
  in 
  match list with
  | [] -> [] 
  | h::t -> 
      begin
        let subtractor = (h :: (deleteExtra(helper (h) (t)) (h))) in 
        (subtractor :: []) @ equivs (f) (deleteInList subtractor list); 
      end
;;
      



(*Question 6*)

let rec goldbachpair = fun x -> 
  let increment = 2 in 
  
  let z = 2
  in

  let rec findPrimNum y = if (y mod 2 == 0 || y mod 3 == 0|| y mod 5 ==0 || y mod 7 == 0) && (y!= 2 && y != 3 && y!=5 && y!= 7) 
    then findPrimNum (y+1) else y 

  in 
      
  let rec isPrimeNum q i = 
    
    if(i!=q && q mod i == 0) then false else 
    if (i + 1 == q) then true else 
      
      isPrimeNum q (i+1); 
      
      
      
  in
  
  let rec repeat l = 
    let value = findPrimNum l in

    if isPrimeNum (x - (value)) 2 then begin
      print_int value ;
      if (((x - value)) > value) then (value ,(x - (value)) ) 
      else ((x - (value)) , value )
           
    end
    else 
      begin
  
        increment = 2;
        l = z+1; 
        z = l;
        repeat (l+1) ;
      end
  in
  
  repeat z
;;

(*Question 7*)


let rec equiv_on f g list = 
  let result_f item = f item in
  let result_g item = g item in 

  let rec comparison list_in = match list_in with 
    |[] -> true
    | h::t -> if result_f h = result_g h then comparison t else false
  in 

  comparison list
;;



(*Question 8*)

let rec pairwisefilter cmp lst = match lst with 
  |[] -> []
  | h::h2::t -> cmp h h2 :: pairwisefilter cmp t
  | h::[] -> [h]
;;


(*Question 9*)

let rec polynomial list = fun f ->
  match list with 
  |[]->0 
  |h::t -> (fst h * (pow f (snd h))) + (polynomial t) f
;;

(*Question 10*)

let rec powerset list =

  let rec map f apply = match apply with 
    |[] -> []
    |h1::t2 -> (f h1) :: (map f t2)
  in 

  match list with
  |[] -> [[]]
  |h::t -> 
      begin 
        (powerset t) @ (map (fun x -> h::x) (powerset t)) 
      end
;;
  

      

  
  
  




