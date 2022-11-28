open Sympatch

let () =
  let usage_msg = "sympatch -pNUM DIR < PATCH-FILE" in
  if Array.length Sys.argv < 3 then failwith usage_msg
  else
    let p_num =
      let s = Sys.argv.(1) in
      String.sub s 2 (String.length s - 2) |> int_of_string
    in
    let src = Sys.argv.(2) in
    let patch = Patch.of_string (In_channel.input_all stdin) in
    let fs = Fs.of_path src in
    Fs.show fs |> print_endline;
    let fs = Patch.patch patch p_num fs in
    Fs.show fs |> print_endline
