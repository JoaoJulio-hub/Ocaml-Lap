(* Maze module body *)
(* LAP (AMD 2023) *)

(* 
Student 1: 61610 Jo�o J�lio
Student 2: 62942 Rodrigo Freitas

Comment:

?????????????????????????
?????????????????????????
?????????????????????????
?????????????????????????
?????????????????????????
?????????????????????????

*)

(*
0123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789
	100 columns
*)


(* COMPILATION - How Mooshak builds this module:
		ocamlc -c Maze.mli Maze.ml
*)



(* AUXILIARY GENERAL FUNCTIONS - you can add more *)

(* Sorted lists with no repetitions *)
(* precondition for all the list arguments:
		isCanonical l && isCanonical l1 && isCanonical l2 *)
(* postcondition for all the list results: isCanonical result *)

let rec removeDups z = (* pre: z sorted *)
	match z with
	| [] -> []
	| [x] -> [x]
	| x::y::xs -> (if x = y then [] else [x])@ removeDups (y::xs)
;;

let canonize z = (* sort and remove duplicates *)
	removeDups (List.sort compare z)
;;

let isCanonical z = (* check if sorted and with no duplicates *)
	z = (canonize z)
;;

let belongs v l =
	List.mem v l
;;

let length =
	List.length
;;

let filter =
	List.filter
;;

let exists =
	List.exists
;;
s
let for_all =
	List.for_all
;;

let partition =
	List.partition
;;

let contained l1 l2 =
	for_all (fun x -> belongs x l2) l1
;;

let union l1 l2 =
	canonize (l1@l2)

let inter l1 l2 =
	filter (fun x -> belongs x l2) l1
;;

let diff l1 l2 =
	filter (fun a -> not (belongs a l2)) l1
;;

let map f l =
	canonize (List.map f l)
;;

let merge l =
	canonize (List.flatten l)
;;

let flatMap f l =
	merge (List.map f l)
;;

let showi l =
	let li = List.map string_of_int l in
	let body = String.concat "," li in
		Printf.printf "[%s]\n" body
;;

let showp l =
	let li = List.map (fun (a,b) -> Printf.sprintf "(%d,%d)" a b) l in
	let body = String.concat "," li in
		Printf.printf "[%s]\n" body
;;

let rev l =
	List.rev l
;;

(* TYPES & CONSTANTS *)

type room = int
type rooms = room list

type path = room list
type island = room list

type passage = room * room
type passages = passage list

type maze = {
    rooms: rooms;
	  entrances: rooms;
    exits: rooms;
    passages: passages
}

let _NO_PATH = []


(* SOME EXAMPLES - you can add more *)

let myMaze = {
    rooms = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13];
	  entrances = [1;4;11];
    exits = [6;12];
    passages = [(1,2);(1,4);(1,5);(2,5);(3,6);(4,5);(4,7);
				(5,6);(5,8);(6,9);(7,8);(8,9);(10,11);(11,12)]
};;

let loopMaze = {
    rooms = [1;2;3;4];
	  entrances = [1];
    exits = [4];
    passages = [(1,2);(2,3);(3,4);(4,1)]
};;

(* FUNCTION isValid *)

(* isValid auxiliary function *)
let rec compareRoomsWithPass r p =
	match p with
    | [] -> true
    | (x,y)::xs -> if (belongs x r || belongs y r) then
        compareRoomsWithPass r xs
      else
        false
;;

let isValid m =
		(isCanonical m.rooms) && (isCanonical m.passages) && (isCanonical m.entrances) &&
		(length m.rooms > 0) && (length m.passages > 0) && (length m.entrances > 0) && (length m.exits > 0)
		&& (compareRoomsWithPass m.rooms m.passages && contained m.entrances m.rooms) && (contained m.exits m.rooms)
		&& (for_all (fun x -> x >= 0) m.rooms)
;;

(* FUNCTION makeLineMaze *)

let makeLineMaze a b =
    let rec createRooms i l =
        if i <= b then
            createRooms (i + 1) (i :: l)
        else
            rev l
    in let rec createPassages i l =
        if i < b then
            createPassages (i + 1) ((i, i + 1) :: l)
        else
            rev l
		in
    {
        rooms = createRooms 1 [];
        entrances = [a];
        exits = [b];
        passages = createPassages 1 [];
    }
;;

(* FUNCTION combine *)

let combine m1 m2 = 
	{
		rooms = union m1.rooms m2.rooms;
		entrances = union m1.entrances m2.entrances;
		exits = diff (union m1.exits m2.exits) (union m1.entrances m2.entrances);
		passages = union m1.passages m2.passages ;
	};;

(* FUNCTION next *)

(* next auxiliary function *)
let rec roomPassagesNext p r =
	match p with
   | [] -> []
   | (x,y)::xs -> if (x = r) then
       y::roomPassagesNext xs r
     else
        roomPassagesNext xs r
;;

let next m r = 
	roomPassagesNext m.passages r
;;

(* FUNCTION next2 *)

(* next2 auxiliary function *)
let rec roomPassagesNext2 p r =
	match r with
   | [] -> []
   | x::xs -> union (roomPassagesNext p x) (roomPassagesNext2 p xs)
;;

let next2 m r = 
	roomPassagesNext2 m.passages r
;;

(* FUNCTION prev *)

(* prev auxiliary function*)

let rec oneStepConnection p r = 
	match p with
   | [] -> []
   | (x,y)::xs -> if (y = r) then
       x::oneStepConnection xs r
     else
        oneStepConnection xs r
;;

let prev m r = 
	oneStepConnection m.passages r
;;

(* FUNCTION adjacent *)

let adjacent m r = 
	union (next m r) (prev m r)
;;

(* FUNCTION reachable *)

(* reachable auxiliary function*)

(* reachable from a room *)
let rec allReachableRoomsFromRoom m lr = 
	match lr with
   | [] -> []
   | x::xs -> union (union ([x]) (allReachableRoomsFromRoom m (next m x))) (allReachableRoomsFromRoom m xs)
;;

(*reachable from all the entrances*)
let rec allReachableRooms m e = 
	match e with
   | [] -> []
   | x::xs -> union [x] (union (allReachableRoomsFromRoom m (next m x)) (allReachableRooms m xs))
;;

let reachable m = 
	allReachableRooms m m.entrances
;;

(* FUNCTION solitary *)

let solitary m = 
	 let rec sol rooms =
    match rooms with
    | [] -> []
    | x::xs -> if adjacent m x = [] then x::sol xs
                            else sol xs in sol m.rooms
;;

(* FUNCTION islands *)

(* FUNCTION islands *)

let islands m =
  (* Gets the rest of the rooms that belong to an island *)
  let rec restOfTheIsland visitedRooms notVisitedRooms =
    match notVisitedRooms with
    | [] -> []
    | x::xs ->
        let connections = adjacent m x in
        let allVisitedRooms = union visitedRooms connections in
        if (contained connections visitedRooms)
          (* If the connections between a room have already been visited,
           "add" the current room to the list of rooms and go to the next 
					 room in the list of not visited rooms and add its connections to the list
           of in the island *)
          then union [x] (restOfTheIsland visitedRooms xs)
        else
          (* If not, add the room to the list of rooms in the island,
             go to the next room in the list of not visited rooms and add
             its connections, and finally add the connections of the
             current room *)
          union (union [x] (restOfTheIsland allVisitedRooms xs))
                (restOfTheIsland allVisitedRooms connections)
  in
  (* Get the island that each room belongs *)
  let rec unifyIslands rooms = 
    match rooms with
    | [] -> []
    | x::xs ->
        let initialConnections = adjacent m x in
        (* Union of the first room with its connections and the rest of
           the rooms in the island *)
        (union (union [x] initialConnections)
               (restOfTheIsland (union [x] initialConnections) initialConnections))
        ::(unifyIslands xs)
  in
  canonize (unifyIslands m.rooms)
;;

(* FUNCTION shortest *)

let shortest m = 
  if(m.passages = [] || m.exits = []) then _NO_PATH
  else
     let rec explore_path current_room = (* Paths that end in exists only *)
        if (belongs current_room m.exits) then [[current_room]]
        else
          let next_rooms = next m current_room in
          merge (map (fun r -> map (fun path -> current_room :: path) (explore_path  r)) next_rooms)
     in let paths = merge (map (fun entrance -> explore_path entrance)  m.entrances)  in
     let rec compareLength p = (* Find the shortest of the paths *)
        match p with
        	| [] -> _NO_PATH
          | [l] -> l
          | x::y::xs -> let l1 = length x in let l2 =length y 
												in if(l1<l2) then compareLength (x::xs)
											  else compareLength (y::xs)
     in compareLength paths
;;

(* FUNCTION paths *)

let paths m = 
	if(m.passages = [] || m.exits = []) then _NO_PATH
  else
		 let rec explore_path_exits current_room = (* Paths that end in exits *)
        if (belongs current_room m.exits) then [[current_room]]
        else
          let next_rooms = next m current_room in
          merge (map (fun r -> map (fun path -> current_room :: path) (explore_path_exits  r)) next_rooms)
     in let path_exits = merge (map (fun entrance -> explore_path_exits entrance)  m.entrances) 
		 in let rec explore_path_leafs current_room = (* Paths that end in leafs, even the ones that go trough an entrace and keep going*)
        if ((next m current_room )= [] ) then [[current_room]]
        else
          let next_rooms = next m current_room in
          merge (map (fun r -> map (fun path -> current_room :: path) (explore_path_leafs  r)) next_rooms)
     in let path_leafs = merge (map (fun entrance -> explore_path_leafs entrance)  m.entrances) 
		 in union path_exits path_leafs (* Union of all the paths *)
;;



(* FUNCTION hasLoop *)

let hasLoop m = 

    let rec checkAllPathsFromRoom r l =
        match l with
        | [] -> false
        | x::xs -> if belongs r (next m x) then true (*If already passed by that room*)
                   else if (next m x )= [] then false (*If leaf node means that the path did not enter a loop*)
                   else (checkAllPathsFromRoom r xs) || checkAllPathsFromRoom r (next m x) (*Keep checking the remain paths from that start in room r*)
    in let rec checkPathsFromRooms r = (*Check if all paths from a room have any loop*)
        match r with
        | [] -> false
        | x::xs -> (checkAllPathsFromRoom x (next m x)) || (checkPathsFromRooms xs)
    in checkPathsFromRooms m.rooms
;;

(* FUNCTION shortest2 *)

let shortest2 m = 
	if(m.passages = [] || m.exits = []) then _NO_PATH
  else
	  let rec explore_path visited_rooms current_room = 
			(* Paths that end in exits only, and know we store a 
		the visited rooms so that we can keep track if we enter a loop *)
				if  (belongs current_room visited_rooms) then []
    		else if (belongs current_room m.exits) then [[current_room]]
    		else
      		let new_visited_rooms = visited_rooms @ [current_room] in
      		let next_rooms = next m current_room in
      		merge (map (fun r -> map (fun path -> current_room :: path) (explore_path new_visited_rooms r)) next_rooms)
    in let paths = merge (map (fun entrance ->  (explore_path [] entrance)) m.entrances) in
	  let rec compareLength p = (* Find the shortest of the paths *)
        match p with
        	| [] -> _NO_PATH
          | [l] -> l
          | x::y::xs -> let l1 = length x in let l2 =length y 
												in if(l1<l2) then compareLength (x::xs)
											  else compareLength (y::xs)
     in compareLength paths
;;
