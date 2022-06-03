open Traduction
open IA


type attaque = {dmg : int; range : int; pa : int} ;;
type c_player = {force : int ; vie : int ; pa : int ; pm : int ; x :int ; y : int ; attaque : attaque} ;;

let connect (address: string) port =
  let addr = Unix.inet_addr_of_string address in
  let inet_addr = Unix.ADDR_INET (addr, port) in
  Unix.open_connection inet_addr

let shutdown = Unix.shutdown_connection

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

let display_informations_of_players nb_players car_players =
  for i = 1 to nb_players do
    Printf.printf "player %d : Force : %d , Vie : %d , PA : %d , PM : %d , X : %d , Y : %d , Attaque : { Dmg : %d , Range : %d , PA : %d } \n" i ((Hashtbl.find car_players i).force) ((Hashtbl.find car_players i).vie) ((Hashtbl.find car_players i).pa) ((Hashtbl.find car_players i).pm) ((Hashtbl.find car_players i).x) ((Hashtbl.find car_players i).y) ((Hashtbl.find car_players i).attaque.dmg) ((Hashtbl.find car_players i).attaque.range) ((Hashtbl.find car_players i).attaque.pa);
  done
;;

let car_perso p car_player = 
  let l = String.split_on_char ';' p in
  let rec print_perso_aux l i p1 p2=
    if i < List.length l then
      begin
        if (i mod 2) == 0 then
          begin
            let truc = String.split_on_char '|' (List.nth l i) in
            Hashtbl.add car_player p1 {force =int_of_string  (List.nth truc 0); vie =int_of_string  (List.nth truc 1); pa =int_of_string  (List.nth truc 2); pm =int_of_string  (List.nth truc 3) ;x = int_of_string (List.nth (String.split_on_char '*' (List.nth truc 4)) 0) ; y = int_of_string (List.nth (String.split_on_char '*' (List.nth truc 4)) 1); attaque = {dmg =int_of_string  (List.nth truc 5); range =int_of_string  (List.nth truc 6); pa =int_of_string  (List.nth truc 7)}}; 
            print_perso_aux l (i+1) (p1+1) p2
          end
        else
          begin
            let truc = String.split_on_char '|' (List.nth l i) in
            Hashtbl.add car_player p2 {force = int_of_string (List.nth truc 0); vie = int_of_string (List.nth truc 1); pa = int_of_string (List.nth truc 2); pm = int_of_string (List.nth truc 3) ; x = int_of_string (List.nth (String.split_on_char '*' (List.nth truc 4)) 0); y = int_of_string (List.nth (String.split_on_char '*' (List.nth truc 4)) 1) ; attaque = {dmg =int_of_string  (List.nth truc 5); range =int_of_string  (List.nth truc 6); pa =int_of_string  (List.nth truc 7)}}; 
            print_perso_aux l (i+1) p1 (p2+1)
          end
      end
  in 
  print_perso_aux l 0 1 4
;;


let clear_grid grid target hst_players player nbc nbl =
  for i = 0 to nbl - 1 do
    for j = 0 to nbc - 1 do
      if grid.(i).(j) = (player-1) then
         (
         grid.(i).(j) <- (-1);
         grid.(fst target).(snd target) <- (player-1);
         Hashtbl.replace hst_players player {force = ((Hashtbl.find hst_players player).force) ; vie = ((Hashtbl.find hst_players player).vie) ; pa = ((Hashtbl.find hst_players player).pa) ; pm = (((Hashtbl.find hst_players player).pm)) ; x = (fst target) ; y = (snd target); attaque = ((Hashtbl.find hst_players player).attaque)};
        )
    done;
  done
;;

let alter_pm_player hst_players player valeur =
  Hashtbl.replace hst_players player {force = ((Hashtbl.find hst_players player).force) ; vie = ((Hashtbl.find hst_players player).vie) ; pa = ((Hashtbl.find hst_players player).pa) ; pm = (((Hashtbl.find hst_players player).pm) - (valeur)) ; x = ((Hashtbl.find hst_players player).x) ; y = ((Hashtbl.find hst_players player).y); attaque = ((Hashtbl.find hst_players player).attaque)}
;;

let player_move_to grid target player hst_players nbc nbl =
  let origin = ((Hashtbl.find hst_players player).x,(Hashtbl.find hst_players player).y) in 
  let path = List.tl (List.hd (Option.to_list (find_path origin target grid))) in
  let rec player_move_to_aux grid target player hst_players path nbc nbl nb_mov =  
    nb_mov := 0;
    match path with 
    | [] -> print_string "arrivée à la distionation ";
        print_string "\n"
    | x :: s -> 
        nb_mov := !nb_mov + 1 ;
        clear_grid grid x hst_players player nbc nbl;
        display_grid grid nbc nbl;
        alter_pm_player hst_players player !nb_mov;
        player_move_to_aux grid target player hst_players s nbc nbl nb_mov
  in
  player_move_to_aux grid target player hst_players path nbc nbl (ref 0)
;;

let initialisation_act_players act_players nb_p = 
  for i = 1 to nb_p do
    Hashtbl.add act_players i [];
  done;
;;

let change_act_of_players act pos_act ply act_players =
  if (List.length (Hashtbl.find act_players ply)) = 0 then
    ( 
      Hashtbl.add act_players ply ((string_of_int act) :: (Hashtbl.find act_players (ply))); 
      Hashtbl.add act_players ply (("|") :: (Hashtbl.find act_players (ply)));
      Hashtbl.add act_players ply ((string_of_int (fst pos_act)) :: (Hashtbl.find act_players (ply)));
      Hashtbl.add act_players ply (("|") :: (Hashtbl.find act_players (ply)));
      Hashtbl.add act_players ply ((string_of_int (snd pos_act)) :: (Hashtbl.find act_players (ply)));
    )
  else
    (Hashtbl.add act_players ply ((";") :: (Hashtbl.find act_players (ply))); 
     Hashtbl.add act_players ply ((string_of_int (fst pos_act)) :: (Hashtbl.find act_players (ply)));
     Hashtbl.add act_players ply (("|") :: (Hashtbl.find act_players (ply)));
     Hashtbl.add act_players ply ((string_of_int (snd pos_act)) :: (Hashtbl.find act_players (ply))); 
     Hashtbl.add act_players ply (("|") :: (Hashtbl.find act_players (ply)));
     Hashtbl.add act_players ply ((string_of_int act) :: (Hashtbl.find act_players (ply)));
    )
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

let p1_attaque_p2 p1 p2 car_players =
  let pos_p1 = (((Hashtbl.find car_players p1).x),((Hashtbl.find car_players p1).y)) in
  let pos_p2 = (((Hashtbl.find car_players p2).x),((Hashtbl.find car_players p2).y)) in
  if (distance_attaque pos_p1 pos_p2) <=  ((Hashtbl.find car_players p1).attaque.range) && ((Hashtbl.find car_players p1).force) >= 1 && ((Hashtbl.find car_players p1).vie) >= 1 && ((Hashtbl.find car_players p2).vie) >= 1 then
    (Hashtbl.replace car_players p2 {force = ((Hashtbl.find car_players p2).force) ; vie = (((Hashtbl.find car_players p2).vie) - ((Hashtbl.find car_players p1).attaque.dmg) + (((Hashtbl.find car_players p1).attaque.dmg)/10)) ; pa = ((Hashtbl.find car_players p2).pa) ; pm = ((Hashtbl.find car_players p2).pm) ; x = ((Hashtbl.find car_players p2).x); y = ((Hashtbl.find car_players p2).y); attaque = ((Hashtbl.find car_players p2).attaque)};
     Hashtbl.replace car_players p1 {force = ((Hashtbl.find car_players p1).force) ; vie = ((Hashtbl.find car_players p1).vie) ; pa = (((Hashtbl.find car_players p1).pa) - ((Hashtbl.find car_players p1).attaque.pa)) ; pm = ((Hashtbl.find car_players p1).pm) ; x = ((Hashtbl.find car_players p2).x); y = ((Hashtbl.find car_players p2).y) ;attaque = ((Hashtbl.find car_players p1).attaque)})
  else
    ()
;;

let player2_in_h_b_g_d p1 p2 hst_players =
  let pos_p1 = (((Hashtbl.find hst_players p1).x),((Hashtbl.find hst_players p1).y))in
  let pos_p2 = (((Hashtbl.find hst_players p2).x),((Hashtbl.find hst_players p2).y)) in
  (distance_attaque pos_p1 pos_p2) != 0
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


let client_server inc outc =
    (*let inc, outc = connect "92.89.116.186" 6566 in*)
    output_string outc "Ouksili\n";
    flush outc;
    output_string outc "50|50|10|5|5*5|10|5|5\n";
    flush outc;
    output_string outc "50|50|10|5|5*5|10|5|5\n";
    flush outc;
    output_string outc "50|50|10|5|5*5|10|5|5\n";
    flush outc;
    let msg = ref (input_line inc) in
    print_endline (Printf.sprintf "Received from server: \"%s\"" !msg);
    let msg2 = ref (input_line inc) in
    print_endline (Printf.sprintf "Received from server: \"%s\"" !msg2);
    let msg3 = ref(input_line inc) in
    print_endline (Printf.sprintf "Received from server: \"%s\"" !msg3);
    let num_player = ref (int_of_string !msg) in
    let cplayer = Hashtbl.create 6 in
    let act_players = Hashtbl.create 6 in
    initialisation_act_players act_players 6;
    let pos_players = Hashtbl.create 6 in
    let board = (fst (str_to_map !msg2)) in
    Printf.printf "C'est le tour de joueur %s \n" !msg;
    car_perso !msg3 cplayer;
    display_informations_of_players 6 cplayer;
    display_grid (fst (str_to_map !msg2)) (fst (snd (str_to_map !msg2))) (snd (snd (str_to_map !msg2)));
    while pm_of_players_is_not_null 6 cplayer do
      let f = ref true in
      while !f do
    let nbl = nbr_lgn (list_car !msg2) in
    let nbc = nbr_cl (list_car !msg2) in 
        f := true;
        if ((Hashtbl.find cplayer (!num_player+1)).pm >= 0) || ((Hashtbl.find cplayer (!num_player+1)).pa >= 0) then
          (
    let n_choix = read_int() in
    if n_choix = 1 then
       (if (Hashtbl.find cplayer (!num_player+1)).pm > 0 then
           (let target_x = ref 0 in
                let target_y = ref 0 in
                print_string "Target_x : ";
                target_x := read_int();
                print_string "\n";
                print_string "Target_y : ";
                target_y := read_int();
                print_string "\n";
                while ((board.(!target_y).(!target_x)) > 0) || (((Hashtbl.find cplayer (!num_player+1)).pm) - ((List.length (List.hd (Option.to_list (find_path ((Hashtbl.find cplayer (!num_player+1)).x,(Hashtbl.find cplayer (!num_player+1)).y) (!target_x,!target_y) board)))) - 1)  < 0) do
                print_string "Target_x : ";
                target_x := read_int();
                print_string "\n";
                print_string "Target_y : ";
                target_y := read_int();
                print_string "\n";
                done;
                player_move_to board (!target_y,!target_x) (!num_player+1) cplayer (fst (snd (str_to_map !msg2))) (snd (snd (str_to_map !msg2)));
                change_act_of_players 2 (!target_x,!target_y) (!num_player+1) act_players;
                )
       else 
          ())
          else if n_choix = 3 then
         (if (Hashtbl.find cplayer (!num_player+1)).pa > 0 then
                (let p2 = ref 0 in
                 print_string "Player que vous voulez attaquer : ";
                 p2 := read_int();
                 print_string "\n";
                 if player2_in_h_b_g_d (!num_player) !p2 pos_players then
                   (p1_attaque_p2 (!num_player) !p2 cplayer;
                    change_act_of_players 1 (((Hashtbl.find cplayer (!num_player+1)).x),((Hashtbl.find cplayer (!num_player+1)).y)) (!num_player+1) act_players;
                    if ((Hashtbl.find cplayer !p2).vie) < 1 then
                      (delete_player_in_grid board !p2 nbc nbl)
                    else
                      ()))
                      else
           ())
    else if n_choix = 2 then
         (f := false;
              let str = ((String.concat "" (List.rev (Hashtbl.find act_players ((int_of_string !msg)+1)))))^"\n" in
              output_string outc (str);
              flush outc;
              print_string str;
              initialisation_act_players act_players 6;
              msg := (input_line inc);
              print_endline (Printf.sprintf "Received from server: \"%s\"" !msg);
              msg2 := (input_line inc);
              print_endline (Printf.sprintf "Received from server: \"%s\"" !msg2);
              msg3 := (input_line inc);
              print_endline (Printf.sprintf "Received from server: \"%s\"" !msg3);
            )
           else
             (exit 0))
      done;
  done;
   print_string "La partie est terminée ! \n";
    shutdown inc