let rec list_car ch = 
  match ch with
  | "" -> []
  | ch -> (String.get ch 0 ) :: (list_car (String.sub ch 1 ( (String.length ch)-1) ) )  
;;

let nbr_lgn lst =
  let rec nbr_lgn_aux lst cpp =
    match lst with 
      [] -> cpp
    | x::s -> if x = '|' then
          nbr_lgn_aux s cpp+1
        else
          nbr_lgn_aux s cpp
  in
  nbr_lgn_aux lst 1
;;

let nbr_cl lst =
  let rec nbr_cl_aux lst cpp =
    match lst with 
      [] -> cpp
    | x::s -> if x = '#' || x = ' ' then
          nbr_cl_aux s cpp+1
        else
          nbr_cl_aux [] cpp
  in
  nbr_cl_aux lst 0
;; 


let lst_pos_of_players lst =
  let lst1 = list_car lst in
  let rec lst_pos_of_players_aux lst1 lst_cord_of_players cord  = 
    match lst1 with 
      [] -> (List.rev lst_cord_of_players)
    | x::s -> if x = '#' then 
          lst_pos_of_players_aux s lst_cord_of_players (fst cord+1,snd cord) 
        else if x = ' ' then 
          lst_pos_of_players_aux s lst_cord_of_players (fst cord+1,snd cord) 
        else if x = '|' then 
          lst_pos_of_players_aux s lst_cord_of_players (0, snd cord+1) 
        else 
          lst_pos_of_players_aux s ((int_of_string (Char.escaped x),cord)::lst_cord_of_players) (fst cord+1, snd cord) 
  in
  lst_pos_of_players_aux lst1 [] (0,0) 
;;

let rec initialise_grid lst grid =
  match lst with 
    [] -> ()
  | x :: s -> if (fst x) <> -1 && (fst x) <> -2 then
        (grid.((snd (snd x))).((fst (snd x))) <- (fst x);
         initialise_grid s grid)
      else 
        initialise_grid s grid
;;


let str_to_map str =
  let lst = list_car str in
  let nbl = nbr_lgn lst in
  let nbc = nbr_cl lst in
  let lst_players = lst_pos_of_players str in 
  let grid = Array.make_matrix nbl nbc 0 in 
  for i = 0 to nbl - 1 do
    for j = 0 to nbc - 1 do
      if i = 0 || i = nbl - 1 then  
        grid.(i).(j) <- -6
      else if j = 0 || j = nbc - 1 then 
        grid.(i).(j) <- -6
      else 
        grid.(i).(j) <- -1
    done;
  done;
  initialise_grid lst_players grid;
  (grid,(nbl,nbc))
;;

type attaque = {dmg : int; range : int; pa : int} ;;
type c_player = {force : int ; vie : int ; pa : int ; pm : int ; attaque : attaque} ;;


let print_perso p car_player = 
  let l = String.split_on_char ';' p in
  let rec print_perso_aux l i p1 p2=
    if i < List.length l then
      begin
        if (i mod 2) == 0 then
          begin
            let truc = String.split_on_char '|' (List.nth l i) in
            Hashtbl.add car_player p1 {force =int_of_string  (List.nth truc 0); vie =int_of_string  (List.nth truc 1); pa =int_of_string  (List.nth truc 2); pm =int_of_string  (List.nth truc 3) ; attaque = {dmg =int_of_string  (List.nth truc 5); range =int_of_string  (List.nth truc 6); pa =int_of_string  (List.nth truc 7)}}; 
            print_perso_aux l (i+1) (p1+1) p2
          end
        else
          begin
            let truc = String.split_on_char '|' (List.nth l i) in
            Hashtbl.add car_player p2 {force = int_of_string (List.nth truc 0); vie = int_of_string (List.nth truc 1); pa = int_of_string (List.nth truc 2); pm = int_of_string (List.nth truc 3) ; attaque = {dmg =int_of_string  (List.nth truc 5); range =int_of_string  (List.nth truc 6); pa =int_of_string  (List.nth truc 7)}}; 
            print_perso_aux l (i+1) p1 (p2+1)
          end
      end
  in 
  print_perso_aux l 0 0 3
;;

(*let lst = list_car "####|#1 #|#2 #|####";;
lst_pos_of_players lst;;
str_to_map "####|#1 #|#2 #|#  #|####";; *)