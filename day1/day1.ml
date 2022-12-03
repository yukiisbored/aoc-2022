open Core

let group_and_sum lines =
  List.fold ~init:[ [] ]
    ~f:(fun acc line ->
      match line with
      | "" -> [] :: acc
      | _ -> (int_of_string line :: List.hd_exn acc) :: List.tl_exn acc)
    lines
  |> List.map ~f:(List.fold_left ~init:0 ~f:( + ))

let part1 lines =
  group_and_sum lines
  |> List.max_elt ~compare:Int.compare
  |> Option.value_exn |> string_of_int

let part2 lines =
  let sorted =
    group_and_sum lines |> List.sort ~compare:Int.compare |> List.rev
  in
  List.take sorted 3 |> List.fold_left ~init:0 ~f:( + ) |> string_of_int

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