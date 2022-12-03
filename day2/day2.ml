open Core

type move = Rock | Paper | Scissors
type outcome = Win | Lose | Draw

let piece_score = function Rock -> 1 | Paper -> 2 | Scissors -> 3
let outcome_score = function Lose -> 0 | Draw -> 3 | Win -> 6

let part1 lines =
  let parse_line line =
    let chars = String.to_list line in
    let their_move =
      List.nth_exn chars 0 |> function
      | 'A' -> Rock
      | 'B' -> Paper
      | 'C' -> Scissors
      | _ -> assert false
    in
    let your_move =
      List.nth_exn chars 2 |> function
      | 'X' -> Rock
      | 'Y' -> Paper
      | 'Z' -> Scissors
      | _ -> assert false
    in
    (your_move, their_move)
  in
  let play ~your_move ~their_move =
    match (your_move, their_move) with
    | Rock, Scissors -> Win
    | Paper, Rock -> Win
    | Scissors, Paper -> Win
    | Rock, Rock -> Draw
    | Paper, Paper -> Draw
    | Scissors, Scissors -> Draw
    | _, _ -> Lose
  in
  let play_line line =
    parse_line line |> fun (your_move, their_move) ->
    outcome_score (play ~your_move ~their_move) + piece_score your_move
  in
  List.map ~f:play_line lines |> List.fold ~init:0 ~f:( + ) |> string_of_int

let part2 lines =
  let parse_line line =
    let chars = String.to_list line in
    let their_move =
      List.nth_exn chars 0 |> function
      | 'A' -> Rock
      | 'B' -> Paper
      | 'C' -> Scissors
      | _ -> assert false
    in
    let outcome =
      List.nth_exn chars 2 |> function
      | 'X' -> Lose
      | 'Y' -> Draw
      | 'Z' -> Win
      | _ -> assert false
    in
    (their_move, outcome)
  in
  let determine_move their_move outcome =
    match (their_move, outcome) with
    | Rock, Win -> Paper
    | Rock, Draw -> Rock
    | Rock, Lose -> Scissors
    | Paper, Win -> Scissors
    | Paper, Draw -> Paper
    | Paper, Lose -> Rock
    | Scissors, Win -> Rock
    | Scissors, Draw -> Scissors
    | Scissors, Lose -> Paper
  in
  let play_line line =
    parse_line line |> fun (their_move, outcome) ->
    outcome_score outcome + piece_score (determine_move their_move outcome)
  in
  List.map ~f:play_line lines |> List.fold ~init:0 ~f:( + ) |> string_of_int

let solvercmd summary solver =
  Command.basic ~summary
    Command.Let_syntax.(
      let%map_open filename = anon ("filename" %: string) in
      fun () -> In_channel.read_lines filename |> solver |> print_endline)

let command =
  Command.group ~summary:"Advent of Code 2022"
    [
      ("part1", solvercmd "Solve Part 1" part1);
      ("part2", solvercmd "Solve Part 2" part2);
    ]

let () = Command_unix.run command
