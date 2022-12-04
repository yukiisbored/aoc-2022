open Core

let parse line =
  let f x =
    let b, t = String.lsplit2_exn ~on:'-' x in
    (int_of_string b, int_of_string t)
  in
  String.lsplit2_exn ~on:',' line |> fun (x, y) -> (f x, f y)

let part1 lines =
  let f line =
    parse line |> fun ((b1, t1), (b2, t2)) ->
    Int.((b1 >= b2 && t1 <= t2) || (b2 >= b1 && t2 <= t1))
  in
  List.map ~f lines |> List.count ~f:Fn.id

let part2 lines =
  let f line =
    parse line |> fun ((b1, t1), (b2, t2)) -> Int.(b1 <= t2 && b2 <= t1)
  in
  List.map ~f lines |> List.count ~f:Fn.id

let command =
  let sub summary solver =
    Command.basic ~summary
      Command.Let_syntax.(
        let%map_open filename = anon ("filename" %: string) in
        fun () -> In_channel.read_lines filename |> solver |> printf "%d\n")
  in
  Command.group ~summary:"Advent of Code 2022"
    [ ("part1", sub "solve part 1" part1); ("part2", sub "solve part 2" part2) ]

let () = Command_unix.run command
