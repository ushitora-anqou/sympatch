open Utils

type t = { diffs : Patch.t list }

let apply_hunks (old : string) (d : Patch.t) =
  let open Patch in
  let old = String.split_on_char '\n' old in
  let i, lines =
    d.hunks
    |> List.fold_left
         (fun (i, acc) hunk ->
           let prefix = List.sub i hunk.mine_start old in
           (hunk.mine_start + hunk.mine_len, acc @ prefix @ hunk.their))
         (0, [])
  in
  let lines = lines @ List.sub i (List.length old) old in
  let lines =
    match (d.mine_no_nl, d.their_no_nl) with
    | false, true -> (
        match List.rev lines with "" :: tl -> List.rev tl | _ -> lines)
    | true, false -> lines @ [ "" ]
    | _ -> lines
  in
  String.concat "\n" lines

let patch_one (d : Patch.t) (n : int) (fs : Fs.t) =
  let open Patch in
  let open Fs in
  let suffix name =
    name |> String.split_on_char '/' |> List.shift_n n |> snd
    |> String.concat "/"
  in
  match d.operation with
  | Rename_only (old_name, new_name) ->
      let target, fs = cut (suffix old_name) fs in
      replace (suffix new_name) target fs
  | Edit name ->
      let renewed = apply_hunks (Fs.get_content (suffix name) fs) d in
      replace (suffix name) (File renewed) fs
  | Rename (old_name, new_name) ->
      let renewed, fs =
        try
          let renewed = apply_hunks (Fs.get_content (suffix old_name) fs) d in
          let _, fs = cut (suffix old_name) fs in
          (renewed, fs)
        with Not_found ->
          let renewed = apply_hunks "" d in
          (renewed, fs)
      in
      replace (suffix new_name) (File renewed) fs
  | Delete _name -> (*snd (cut (suffix name) fs)*) assert false
  | Create _name ->
      (*
      let content =
        match d.hunks with
        | [ h ] ->
            let lines = if d.their_no_nl then h.their else h.their @ [ "" ] in
            String.concat "\n" lines
        | _ -> assert false
      in
      replace (suffix name) (File content) fs
      *)
      assert false

let patch (p : t) (n : int) (fs : Fs.t) : Fs.t =
  List.fold_left (fun fs d -> patch_one d n fs) fs p.diffs
  |> Fs.remove_empty_files_and_dirs |> Fs.replace_unchanged_dir_with_link

let read_patch_file (fpath : string) : t =
  let ic = open_in_bin fpath in
  let raw = In_channel.input_all ic in
  close_in ic;
  { diffs = Patch.to_diffs raw }
