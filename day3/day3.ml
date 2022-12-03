open Core

let to_prio_int = function
  | x when Char.(x < 'a') -> Char.(to_int x - to_int 'A') + 27
  | x -> Char.(to_int x - to_int 'a') + 1

let part1 lines =
  let f line =
    let xs = String.to_list line in
    let ls, rs = List.split_n xs (List.length xs / 2) in
    let ls', rs' =
      (Set.of_list (module Char) ls, Set.of_list (module Char) rs)
    in
    Set.inter ls' rs' |> Set.choose_exn |> to_prio_int
  in
  List.map ~f lines |> List.fold ~init:0 ~f:( + ) |> string_of_int

let part2 lines =
  List.fold ~init:[ [] ]
    ~f:(fun acc line ->
      let f x = String.to_list x |> Set.of_list (module Char) in
      match acc with
      | [ _; _; _ ] :: _ -> [ f line ] :: acc
      | hs :: ts -> (f line :: hs) :: ts
      | _ -> failwith "unreachable")
    lines
  |> List.map ~f:(function
       | [ x; y; z ] ->
           Set.inter x y |> Set.inter z |> Set.choose_exn |> to_prio_int
       | _ -> failwith "unreachable")
  |> List.fold ~init:0 ~f:( + ) |> string_of_int

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
