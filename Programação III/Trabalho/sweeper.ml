open Random

type cell =
  | Empty
  | Mine
  | MarkedMine
  | Stepped of int

type board = cell array array

type game_state = {
  mutable board: board;
  mutable size: int;
  mutable mines: int;
  mutable mine_positions: (int * int) list;
}

let initialize_board n = Array.make_matrix n n Empty

let valid_coords size r c = r >= 0 && r < size && c >= 0 && c < size

let count_neighbors board size r c =
  let directions = [(-1, -1); (-1, 0); (-1, 1); (0, -1); (0, 1); (1, -1); (1, 0); (1, 1)] in
  List.fold_left (fun acc (dr, dc) ->
    let nr, nc = r + dr, c + dc in
    if valid_coords size nr nc && board.(nr).(nc) = Mine then acc + 1 else acc
  ) 0 directions

let add_mine state r c =
  if valid_coords state.size r c then (
    state.board.(r).(c) <- Mine;
    state.mine_positions <- (r, c) :: state.mine_positions
  )

let place_random_mines state k =
  let rec place remaining =
    if remaining > 0 then (
      let r, c = (Random.int state.size, Random.int state.size) in
      if state.board.(r).(c) <> Mine then (
        add_mine state r c;
        place (remaining - 1)
      ) else place remaining
    )
  in place k

let command_empty state n =
  state.board <- initialize_board n;
  state.size <- n;
  state.mines <- 0;
  state.mine_positions <- [];
  print_endline "ok"

let command_random state k =
  if k < state.size * state.size then (
    place_random_mines state k;
    state.mines <- k;
    print_endline "ok"
  )

let command_mine state r c =
  let r, c = r - 1, c - 1 in
  if valid_coords state.size r c && state.board.(r).(c) = Empty then (
    add_mine state r c;
    state.mines <- state.mines + 1;
    print_endline "ok"
  )

let command_step state r c =
  let r, c = r - 1, c - 1 in
  if valid_coords state.size r c then (
    match state.board.(r).(c) with
    | Mine -> print_endline "boom"
    | Empty ->
      let n = count_neighbors state.board state.size r c in
      state.board.(r).(c) <- Stepped n;
      Printf.printf "count %d\n" n
    | _ -> ()
  )

let command_mark state r c =
  let r, c = r - 1, c - 1 in
  if valid_coords state.size r c && state.board.(r).(c) = Empty then (
    state.board.(r).(c) <- MarkedMine;
    print_endline "ok"
  )

let command_done state =
  let all_marked =
    List.for_all (fun (r, c) -> state.board.(r).(c) = MarkedMine) state.mine_positions
  in
  if all_marked then (
    print_endline "ok";
    false
  ) else (
    print_endline "fail";
    false
  )

let command_dump state =
  Array.iter (fun row ->
    print_string "(";
    Array.iter (function
      | Empty -> print_char ' '
      | Mine -> print_char ' '
      | MarkedMine -> print_char '#'
      | Stepped n -> print_char (char_of_int (n + 48))
    ) row;
    print_endline ")"
  ) state.board;
  true

let minesweeper () =
  let state = {
    board = [|[||]|];
    size = 0;
    mines = 0;
    mine_positions = [];
  } in
  let rec loop () =
    match read_line () with
    | exception End_of_file -> false
    | line ->
      let tokens = String.split_on_char ' ' line in
      (match tokens with
      | ["empty"; n] -> command_empty state (int_of_string n); loop ()
      | ["random"; k] -> command_random state (int_of_string k); loop ()
      | ["mine"; r; c] -> command_mine state (int_of_string r) (int_of_string c); loop ()
      | ["step"; r; c] -> command_step state (int_of_string r) (int_of_string c); loop ()
      | ["mark"; r; c] -> command_mark state (int_of_string r) (int_of_string c); loop ()
      | ["done"] -> command_done state
      | ["dump"] -> command_dump state && loop ()
      | _ -> failwith "Invalid command")
  in
  loop ()