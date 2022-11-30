open Sympatch

let parse_command_line () =
  let usage_msg = "sympatch -pNUM SRC-DIR DST-DIR < PATCH-FILE" in
  if Array.length Sys.argv < 4 then failwith usage_msg
  else
    let p_num = Sys.argv.(1) in
    let src_dir = Sys.argv.(2) in
    let dst_dir = Sys.argv.(3) in
    let patch = In_channel.input_all stdin in
    (p_num, src_dir, dst_dir, patch)

let () =
  let p_num, src_dir, dst_dir, patch = parse_command_line () in
  let p_num = String.sub p_num 2 (String.length p_num - 2) |> int_of_string in
  let patch = Patch.of_string patch in
  src_dir |> Fs.of_path |> Patch.patch patch p_num |> Fs.instantiate dst_dir
