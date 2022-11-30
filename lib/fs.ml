open Utils

type t =
  | Link of string
  | File of string
  | Dir of (string option * (string * t) list)
[@@deriving show]

let empty = Dir (None, [])

let get path t =
  let rec aux = function
    | [], n -> n
    | name :: names, Dir (_, entries) -> aux (names, List.assoc name entries)
    | _ -> raise Not_found
  in
  aux (String.split_on_char '/' path, t)

exception Dir_found

let get_content path t =
  match get path t with
  | Link fpath ->
      let ic = open_in_bin fpath in
      let content = In_channel.input_all ic in
      close_in ic;
      content
  | File content -> content
  | Dir _ -> raise Dir_found

let rec equal l r =
  match (l, r) with
  | Link l, Link r | File l, File r -> l = r
  | Dir (_, l), Dir (_, r) ->
      let l = List.sort compare l in
      let r = List.sort compare r in
      List.combine l r
      |> List.for_all (fun ((l1, l2), (r1, r2)) -> l1 = r1 && equal l2 r2)
  | _ -> false

let of_path (path : string) : t =
  let rec aux dir =
    let entries =
      Sys.readdir dir
      |> Array.map (fun name ->
             let p = Filename.concat dir name in
             let s = Unix.stat p in
             let v =
               match s.st_kind with
               | S_REG ->
                   (* Regular file *)
                   Link p
               | S_DIR -> (* Directory *) aux p
               | _ -> failwithf "Unimplemented: %s" p
             in
             (name, v))
      |> Array.to_list
    in
    Dir (Some dir, entries)
  in
  aux path

let replace (path : string) (t' : t) (t : t) =
  let rec aux = function
    | [], _ -> t'
    | name :: names, Dir (_, entries) ->
        let rec aux' acc = function
          | [] -> (* add *) List.rev ((name, aux (names, Dir (None, []))) :: acc)
          | (name', child) :: entries when name = name' ->
              List.rev_append acc ((name, aux (names, child)) :: entries)
          | entry :: entries -> aux' (entry :: acc) entries
        in
        Dir (None, aux' [] entries)
    | _ -> raise Not_found
  in
  aux (String.split_on_char '/' path, t)

(*
forall path:string, t:t,
let f, s = cut path t in
assert (equal t (replace path f s))
 *)
let cut (path : string) (t : t) =
  let rec aux = function
    | [], t' -> (t', None)
    | name :: names, Dir (_, entries) ->
        let rec aux' acc = function
          | [] -> raise Not_found
          | (name', child) :: entries when name = name' -> (
              match aux (names, child) with
              | t', None -> (t', Some (List.rev_append acc entries))
              | t', Some n ->
                  (t', Some (List.rev_append acc ((name, n) :: entries))))
          | entry :: entries -> aux' (entry :: acc) entries
        in
        let t', entries = aux' [] entries in
        (t', Some (Dir (None, Option.get entries)))
    | _ -> raise Not_found
  in
  let t', n = aux (String.split_on_char '/' path, t) in
  (t', Option.get n)

let remove_empty_files_and_dirs fs =
  let rec aux n =
    match n with
    | Link _ -> Some n
    | File "" -> None
    | File _ -> Some n
    | Dir (link, entries) ->
        let entries' =
          entries
          |> List.filter_map (fun entry ->
                 aux (snd entry) |> Option.map (fun s -> (fst entry, s)))
        in
        if entries' = [] then None
        else Some (Dir ((if entries = entries' then link else None), entries'))
  in
  match aux fs with None -> failwith "empty" | Some x -> x

let replace_unchanged_dir_with_link fs =
  let rec aux n =
    match n with
    | Link _ -> n
    | File _ -> n
    | Dir (None, entries) ->
        Dir (None, entries |> List.map (fun (f, s) -> (f, aux s)))
    | Dir (Some link, _) -> Link link
  in
  aux fs

let symlink_rel src dst =
  (* FIXME: Use Unix.symlink for better portability? *)
  exec_cmd "ln" [ "-sr"; src; dst ] |> ignore

let mkdir_p path =
  (* FIXME: Use FilePath? *)
  exec_cmd "mkdir" [ "-p"; path ] |> ignore

let instantiate (dst : string) (fs : t) : unit =
  let rec aux dst = function
    | Link src -> symlink_rel src dst
    | File content ->
        let oc = open_out_bin dst in
        output_string oc content;
        close_out oc
    | Dir (_, entries) ->
        mkdir_p dst;
        entries
        |> List.iter (fun (name, tree) -> aux (Filename.concat dst name) tree)
  in
  aux dst fs
