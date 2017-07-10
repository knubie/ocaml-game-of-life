(* ======= Util ======= *)

let (<<) f g x = f(g(x));;

(* ======= Point ======= *)

type point = { x: int; y: int; }

let point_neighbors point =
  [
    { x = (point.x - 1); y = (point.y - 1) };
    { x = (point.x - 1); y = point.y };
    { x = (point.x - 1); y = (point.y + 1) };

    { x = point.x; y = (point.y - 1) };
    { x = point.x; y = (point.y + 1) };

    { x = (point.x + 1); y = (point.y - 1) };
    { x = (point.x + 1); y = point.y };
    { x = (point.x + 1); y = (point.y + 1) };
  ]

(* ======= Cell ======= *)

type cell = Alive of point
          | Dead of point

let point_of = function
  | Alive p -> p
  | Dead p  -> p

let is_alive = function
  | Alive _ -> true
  | Dead _  -> false

let neighbors cells cell =
  List.filter (fun c -> List.mem (point_of c) (point_neighbors (point_of cell))) cells

let next_cell cells cell =
  match cell with
  | Alive _ -> if List.mem (List.length (List.filter is_alive (neighbors cells cell))) [2; 3]
    then Alive (point_of cell)
    else Dead (point_of cell)
  | Dead _ -> if (List.length (List.filter is_alive (neighbors cells cell))) = 3
    then Alive (point_of cell)
    else Dead (point_of cell)

let my_cells = 
  [
    Dead { x = 0; y = 0 };
    Dead { x = 1; y = 0 };
    Dead { x = 2; y = 0 };

    Alive { x = 0; y = 1 };
    Alive { x = 1; y = 1 };
    Alive { x = 2; y = 1 };

    Dead { x = 0; y = 2 };
    Dead { x = 1; y = 2 };
    Dead { x = 2; y = 2 };
  ]

let my_cell = List.nth my_cells 4

(* Test if my_cell should be alive on the next iteration *)
let () = print_endline (string_of_bool (is_alive (next_cell my_cells my_cell)))
