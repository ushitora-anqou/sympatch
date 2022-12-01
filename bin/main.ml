open Sympatch

let parse_command_line () =
  let usage_msg = "sympatch -p NUM SRC-DIR DST-DIR < PATCH-FILE" in
  let p_num = ref 0 in
  let opt = ref false in
  let input_files = ref [] in
  let files_to_copy = ref [] in
  let speclist =
    [
      ("-p", Arg.Set_int p_num, "p");
      ("-opt", Arg.Set opt, "Remove redundant links");
      ( "-c",
        Arg.String (fun x -> files_to_copy := x :: !files_to_copy),
        "Copy these files instead of making symlinks to them. (e.g., \
         .gitignore)" );
    ]
  in
  let anon_fun filename = input_files := filename :: !input_files in
  Arg.parse speclist anon_fun usage_msg;
  if List.length !input_files <> 2 then
    failwith "Invalid command-line arguments. Check usage by `-help`."
  else
    let files = List.rev !input_files in
    let src_dir = List.nth files 0 in
    let dst_dir = List.nth files 1 in
    let patch = In_channel.input_all stdin in
    (!p_num, !opt, src_dir, dst_dir, patch, !files_to_copy)

let () =
  try
    let p_num, opt, src_dir, dst_dir, patch, files_to_copy =
      parse_command_line ()
    in
    let patch = Patch.of_string patch in
    src_dir |> Fs.of_path
    |> Patch.patch ~opt patch p_num
    |> Fs.instantiate ~files_to_copy dst_dir
  with Failure s ->
    Printf.fprintf stderr "%s\n" s;
    exit 1
