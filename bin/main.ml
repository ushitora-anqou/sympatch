open Sympatch

let () =
  if Array.length Sys.argv < 3 then failwith "Usage: sympatch FILE SRC-DIR"
  else
    let fpath = Sys.argv.(1) in
    let src = Sys.argv.(2) in
    let patch = Patch.read_patch_file fpath in
    let fs = Fs.of_path src in
    Fs.show fs |> print_endline;
    let fs = Patch.patch patch 1 fs in
    Fs.show fs |> print_endline
