let initialize_grid nbc nbl = 
  let grid = Array.make_matrix nbl nbc 0 in
  for i = 0 to nbl-1 do
    for j = 0 to nbc-1 do 
      if i = 0 || i = nbl - 1 then  
        grid.(i).(j) <- 0
      else if j = 0 || j = nbc - 1 then 
        grid.(i).(j) <- 0
      else 
        grid.(i).(j) <- (-1)
    done
  done ; 
  grid
;;

let lst_map_to_car map nbc nbl lst_car = 
  for i = 0 to nbl - 1 do 
    for j = 0 to nbc - 1 do
      if map.(i).(j) = 0 then
        lst_car := "#" :: !lst_car
      else if map.(i).(j) = -1 then
        lst_car := " " :: !lst_car 
      else 
        lst_car := (string_of_int map.(i).(j)) :: !lst_car
    done; 
    lst_car := "|" :: !lst_car;
  done;
  lst_car := List.rev !lst_car
;;

type attaque = {dmg : int; range : int; pa : int} ;;
type c_player = {force : int ; vie : int ; pa : int ; pm : int ; attaque : attaque} ;;

let initialisation_car_player c_players nb_p = 
  for i = 1 to nb_p do
    Hashtbl.add c_players i {force = 50; vie = 50; pa = 10; pm = 5 ; attaque = {dmg = 10; range = 5; pa = 1}};
  done
;; 

let car_of_players_to_car car_player player lst_car =
  lst_car := string_of_int (Hashtbl.find car_player player).force :: !lst_car;
  lst_car := "|" :: !lst_car;
  lst_car := string_of_int (Hashtbl.find car_player player).vie :: !lst_car;
  lst_car := "|" :: !lst_car;
  lst_car := string_of_int (Hashtbl.find car_player player).pa :: !lst_car;
  lst_car := "|" :: !lst_car;
  lst_car := string_of_int (Hashtbl.find car_player player).pm :: !lst_car;
  lst_car := "|" :: !lst_car;
  lst_car := string_of_int (Hashtbl.find car_player player).attaque.dmg :: !lst_car;
  lst_car := "|" :: !lst_car;
  lst_car := string_of_int (Hashtbl.find car_player player).attaque.range :: !lst_car;
  lst_car := "|" :: !lst_car;
  lst_car := string_of_int (Hashtbl.find car_player player).attaque.pa :: !lst_car;
  lst_car := List.rev !lst_car
;;


let rec display_lst_car lst = 
  match lst with 
  |[] -> print_string "\n"
  |x :: s -> print_string x;
      display_lst_car s
;;

(*let lst_act_of_ply_to_car lst =
    
;;*)

(*let () =
  let lst_car = ref [] in 
  let lst_car1 = ref [] in 
  let c_player = Hashtbl.create 6 in
  initialisation_car_player c_player 6;
  let map = initialize_grid 4 4 in
  lst_map_to_car map 4 4 lst_car;
  car_of_players_to_car c_player 2 lst_car1;
  display_lst_car !lst_car;
  display_lst_car !lst_car1;
;;*)