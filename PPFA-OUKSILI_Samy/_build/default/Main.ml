open Client
open IA
open Protocole

let initialize_grid nbc nbl = 
  let grid = Array.make_matrix nbl nbc 0 in
  for i = 0 to nbl-1 do
    for j = 0 to nbc-1 do 
      if i = 0 || i = nbl - 1 then  
        grid.(i).(j) <- -6
      else if j = 0 || j = nbc - 1 then 
        grid.(i).(j) <- -6
      else 
        grid.(i).(j) <- (-1)
    done
  done ; 
  grid
;;

let initialisation_player  nb_p = 
  let players = Hashtbl.create nb_p in
  for i = 0 to nb_p-1 do
    Hashtbl.add players i(i+1);
  done;
  players
;;


let initialisation_pos_player p_players nb_p = 
  for i = 0 to nb_p-1 do
    Hashtbl.add p_players i (0,0);
  done;
;;

type attaque = {dmg : int; range : int; pa : int} ;;
type c_player = {force : int ; vie : int ; pa : int ; pm : int ; attaque : attaque} ;;

let initialisation_car_player c_players nb_p = 
  for i = 1 to nb_p do
    Hashtbl.add c_players i {force = 50; vie = 50; pa = 10; pm = 5 ; attaque = {dmg = 10; range = 5; pa = 5}};
  done
;; 

let alter_pm_player hst_players player valeur =
  Hashtbl.replace hst_players player {force = ((Hashtbl.find hst_players player).force) ; vie = ((Hashtbl.find hst_players player).vie) ; pa = ((Hashtbl.find hst_players player).pa) ; pm = (((Hashtbl.find hst_players player).pm) - (valeur)) ; attaque = ((Hashtbl.find hst_players player).attaque)}
;;


let rec place_alea_player player grid hash_pos_player nbc nbl = 
  Random.self_init ();
  let x = Random.int nbc in
  let y = Random.int nbl in 
  if x > 0 && y > 0 && x < nbc - 1 && y < nbl - 1 && grid.(x).(y) = -1 then
    (grid.(x).(y) <- player; 
     Hashtbl.replace hash_pos_player grid.(x).(y) (x,y))
  else
    place_alea_player player grid hash_pos_player nbc nbl
;;

let initialise_alea_player_in_grid grid pos_players nb_p  nbc nbl=
  let hash_player = initialisation_player nb_p in
  initialisation_pos_player pos_players nb_p;
  for i = 0 to (Hashtbl.length hash_player) - 1 do
    place_alea_player (Hashtbl.find hash_player i) grid pos_players  nbc nbl;
  done
;;


let display_grid grid nbc nbl =
  for i = 0 to nbl - 1 do
    for j = 0 to nbc - 1 do 
      match grid.(i).(j) with
      | -1 -> print_string " ";
      | -6 -> print_string "#";
      | _ -> print_string (string_of_int grid.(i).(j));
    done;
    print_string "\n";
  done;
  print_string "\n";
  print_string "Liste d'action : \n";
  print_string " 1 - Avancer . \n";
  print_string " 2 - Laisser son tour . \n";
  print_string " 3 - Attaquer . \n";
  print_string " 4 - Exit . \n";
  print_string "\n"
;; 

let clear_grid grid target pos_players player nbc nbl =
  for i = 0 to nbl - 1 do
    for j = 0 to nbc - 1 do
      if grid.(i).(j) = player then
        (grid.(i).(j) <- (-1);
         grid.(fst target).(snd target) <- player;
         Hashtbl.replace pos_players player ((fst target) , (snd target))
        )
    done;
  done
;;

let player_move_to grid target player pos_players hst_players nbc nbl =
  let origin = Hashtbl.find pos_players player in 
  let path = List.tl (List.hd (Option.to_list (find_path origin target grid))) in
  let rec player_move_to_aux grid target player pos_players path nbc nbl nb_mov =  
    nb_mov := 0;
    match path with 
    | [] -> print_string "arrivée à la distionation ";
        print_string "\n"
    | x :: s -> 
        nb_mov := !nb_mov + 1 ;
        clear_grid grid x pos_players player nbc nbl;
        display_grid grid nbc nbl;
        alter_pm_player hst_players player !nb_mov;
        player_move_to_aux grid target player pos_players s nbc nbl nb_mov
  in
  player_move_to_aux grid target player pos_players path nbc nbl (ref 0)
;;

let display_informations_of_players nb_players pos_players car_players =
  for i = 1 to nb_players do
    Printf.printf "player %d (%d,%d) : Force : %d , Vie : %d , PA : %d , PM : %d , Attaque : { Dmg : %d , Range : %d , PA : %d } \n" i (snd (Hashtbl.find pos_players i)) (fst (Hashtbl.find pos_players i)) ((Hashtbl.find car_players i).force) ((Hashtbl.find car_players i).vie) ((Hashtbl.find car_players i).pa) ((Hashtbl.find car_players i).pm) ((Hashtbl.find car_players i).attaque.dmg) ((Hashtbl.find car_players i).attaque.range) ((Hashtbl.find car_players i).attaque.pa);
  done
;;

let pm_of_players_is_not_null nb_player car_players =
  let i = ref 1 in
  while((Hashtbl.find car_players !i).pm) = 0 && !i <= nb_player do
    i := !i + 1
  done;
  if !i = nb_player then 
    false
  else
    true
;;

let distance_attaque pos_p1 pos_p2 =
  if (fst pos_p1) = (fst pos_p2) then
    abs ((snd pos_p1) - (snd pos_p2))
  else if (snd pos_p1) = (snd pos_p2) then
    abs ((fst pos_p1) - (fst pos_p2))
  else
    0
;;

let delete_player_in_grid grid player nbc nbl =
  for i = 1 to (nbl - 1) do
    for j = 0 to (nbc - 1) do
      if grid.(i).(j) = player then 
        (grid.(i).(j) <- (-1))
      else
        ()
    done;
  done
;;

let p1_attaque_p2 p1 p2 pos_players car_players =
  let pos_p1 = Hashtbl.find pos_players p1 in
  let pos_p2 = Hashtbl.find pos_players p2 in
  if (distance_attaque pos_p1 pos_p2) <=  ((Hashtbl.find car_players p1).attaque.range) && ((Hashtbl.find car_players p1).force) >= 1 && ((Hashtbl.find car_players p1).vie) >= 1 && ((Hashtbl.find car_players p2).vie) >= 1 then
    (Hashtbl.replace car_players p2 {force = ((Hashtbl.find car_players p2).force) ; vie = (((Hashtbl.find car_players p2).vie) - ((Hashtbl.find car_players p1).attaque.dmg) + (((Hashtbl.find car_players p1).attaque.dmg)/10)) ; pa = ((Hashtbl.find car_players p2).pa) ; pm = ((Hashtbl.find car_players p2).pm) ; attaque = ((Hashtbl.find car_players p2).attaque)};
     Hashtbl.replace car_players p1 {force = ((Hashtbl.find car_players p1).force) ; vie = ((Hashtbl.find car_players p1).vie) ; pa = (((Hashtbl.find car_players p1).pa) - ((Hashtbl.find car_players p1).attaque.pa)) ; pm = ((Hashtbl.find car_players p1).pm) ; attaque = ((Hashtbl.find car_players p1).attaque)})
  else
    ()
;;

let player2_in_h_b_g_d p1 p2 pos_players =
  let pos_p1 = Hashtbl.find pos_players p1 in
  let pos_p2 = Hashtbl.find pos_players p2 in
  (distance_attaque pos_p1 pos_p2) != 0
;;


let initialisation_act_players act_players nb_p = 
  for i = 1 to nb_p do
    Hashtbl.add act_players i [];
  done;
;;


let change_action_of_players act pos_act ply act_players =
 if (List.length (Hashtbl.find act_players ply)) = 0 then
  (Hashtbl.add act_players ply ((string_of_int act) :: (Hashtbl.find act_players ply));
  Hashtbl.add act_players ply (("|") :: (Hashtbl.find act_players ply));
  Hashtbl.add act_players ply ((string_of_int (snd pos_act)) :: (Hashtbl.find act_players ply));
  Hashtbl.add act_players ply (("|") :: (Hashtbl.find act_players ply));
  Hashtbl.add act_players ply ((string_of_int (fst pos_act)) :: (Hashtbl.find act_players ply)))
 else
  (Hashtbl.add act_players ply ((";") :: (Hashtbl.find act_players ply));
  Hashtbl.add act_players ply ((string_of_int act) :: (Hashtbl.find act_players ply));
  Hashtbl.add act_players ply (("|") :: (Hashtbl.find act_players ply));
  Hashtbl.add act_players ply ((string_of_int (snd pos_act)) :: (Hashtbl.find act_players ply));
  Hashtbl.add act_players ply (("|") :: (Hashtbl.find act_players ply));
  Hashtbl.add act_players ply ((string_of_int (fst pos_act)) :: (Hashtbl.find act_players ply)))
;;

let rec display_lst_car lst = 
  match lst with 
  |[] -> print_string "\n"
  |x :: s -> print_string x;
      display_lst_car s
;;


let main =
    print_string "Addr : ";
    let addr = read_line() in
    print_string "\n";
    print_string "Port : ";
    let port = read_int() in
    print_string "\n";
    let inc , outc = connect addr port in
    client_server inc outc
  (*print_string "You are welcome in playergame console . \n";
  print_string "choisis le nombre de colonnes de ta grille : ";
  let nbc = read_int() in
  print_string "\n";
  print_string "choisis le nombre de lignes de ta grille : ";
  let nbl = read_int() in
  print_string "\n";
  print_string "choisis le nombre de players pour équipe 1 : ";
  let nb_p1 = read_int()in
  print_string "\n";
  print_string "choisis le nombre de players pour équipe 2 : ";
  let nb_p2 = read_int()in
  print_string "\n";
  let nb_p = nb_p1 + nb_p2 in
  let pos_players = Hashtbl.create nb_p in
  let board = initialize_grid nbc nbl in
  let car = Hashtbl.create nb_p in
  let c_player = Hashtbl.create nb_p in
  let act_players = Hashtbl.create nb_p in
  initialisation_act_players act_players nb_p;
  initialisation_car_player c_player nb_p; 
  initialise_alea_player_in_grid board pos_players nb_p nbc nbl;
  initialisation_car_player car nb_p;
  let lst_car = ref [] in 
  lst_map_to_car board nbc nbl lst_car;
  while pm_of_players_is_not_null nb_p c_player do
    for i = 1 to nb_p do
      let f = ref true in
      while !f do
        f := true;
        if ((Hashtbl.find c_player i).pm >= 0) || ((Hashtbl.find c_player i).pa >= 0) then
          (Printf.printf "C'est le tour de joueur %d \n" i;
           display_informations_of_players nb_p pos_players c_player;
           display_grid board nbc nbl;
           let n_choix = read_int() in
           if n_choix = 1 then
             if (Hashtbl.find c_player i).pm > 0 then
               (let target_x = ref 0 in
                let target_y = ref 0 in
                print_string "Target_x : ";
                target_x := read_int();
                print_string "\n";
                print_string "Target_y : ";
                target_y := read_int();
                print_string "\n";
                while ((board.(!target_y).(!target_x)) > 0) || (((Hashtbl.find c_player i).pm) - ((List.length (List.hd (Option.to_list (find_path (Hashtbl.find pos_players i) (!target_y,!target_x) board)))) - 1)  < 0) do
                  print_string "Target_x : ";
                  target_x := read_int();
                  print_string "\n";
                  print_string "Target_y : ";
                  target_y := read_int();
                  print_string "\n";
                done; 
                player_move_to board (!target_y,!target_x) i pos_players c_player nbc nbl;
                change_act_of_players 1 (!target_x,!target_y) i act_players)
             else
               ()
           else if n_choix = 3 then
             (if (Hashtbl.find c_player i).pa > 0 then
                (let p2 = ref 0 in
                 print_string "Player que vous voulez attaquer : ";
                 p2 := read_int();
                 print_string "\n";
                 if player2_in_h_b_g_d i !p2 pos_players then
                   (p1_attaque_p2 i !p2 pos_players c_player;
                    change_act_of_players 3 (Hashtbl.find pos_players !p2) i act_players;
                    if ((Hashtbl.find c_player !p2).vie) < 1 then
                      (delete_player_in_grid board !p2 nbc nbl)
                    else
                      ()))
              else
                ())
           else if n_choix = 2 then
             (f := false;
              ())
           else
             (display_lst_car (Hashtbl.find act_players 1);
              exit 0))
        else 
          ()
      done;
    done;
  done;
  print_string "La partie est terminée ! \n"*)