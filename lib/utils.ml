let debug_printf f =
  Printf.ksprintf
    (fun s ->
      if 1 = 1 then (
        Printf.fprintf stderr "%s\n" s;
        flush stderr))
    f

let failwithf f = Printf.ksprintf (fun s -> failwith s) f

let enumerate l =
  let tbl = Hashtbl.create 10 in
  l
  |> List.iter (fun elm ->
         if not (Hashtbl.mem tbl elm) then
           Hashtbl.add tbl elm (Hashtbl.length tbl));
  tbl

let changed_wd wd f =
  let open Unix in
  let org = getcwd () in
  chdir wd;
  f |> Fun.protect ~finally:(fun () -> chdir org)

type process_status = PSUnix of Unix.process_status | PSTimeout

let string_of_process_status = function
  | PSTimeout -> "Command execution timeout"
  | PSUnix unix_status -> (
      match unix_status with
      | Unix.WEXITED 0 -> "Command execution success"
      | res -> (
          let open Printf in
          match res with
          | Unix.WEXITED n -> sprintf "Command execution failed (WEXITED %d)" n
          | Unix.WSIGNALED n ->
              sprintf "Command execution failed by signal (WSIGNALED %d)" n
          | Unix.WSTOPPED n ->
              sprintf "Command execution failed by signal (WSTOPPED %d)" n))

exception
  ExecCmdFailure of {
    status : process_status;
    wd : string;
    params : string list;
    err : string;
    out : string;
  }

let exec_cmd ?(timeout = None) ?(may_fail = false) ?(wd = "") prog args =
  let f () =
    debug_printf "exec_cmd: %s %s" prog (args |> String.concat " ");
    (* Execute command *)
    let params = prog :: args in
    let ic_out, oc_in, ic_err =
      Unix.open_process_args_full (List.hd params) (Array.of_list params)
        (Unix.environment ())
    in
    (* Set timeout if specified *)
    let is_timeout = ref false in
    let prev_handler =
      timeout
      |> Option.map (fun seconds ->
             let open Sys in
             let pid = Unix.process_full_pid (ic_out, oc_in, ic_err) in
             let handler _ =
               is_timeout := true;
               Unix.kill pid sigterm
             in
             let h = signal sigalrm (Signal_handle handler) in
             Unix.alarm seconds |> ignore;
             h)
    in
    (fun () ->
      (* Wait for the command to finish (or abort) *)
      let out = In_channel.input_all ic_out in
      let err = In_channel.input_all ic_err in
      let res = Unix.close_process_full (ic_out, oc_in, ic_err) in
      match res with
      | WEXITED 0 -> (0, out)
      | WEXITED c when may_fail -> (c, out)
      | unix_status ->
          debug_printf "exec_cmd failed";
          let status = if !is_timeout then PSTimeout else PSUnix unix_status in
          let exc = ExecCmdFailure { status; wd; params; err; out } in
          debug_printf "exec_cmd failed: %s" (Printexc.to_string exc);
          raise exc)
    |> Fun.protect ~finally:(fun () ->
           let open Sys in
           prev_handler |> Option.iter (set_signal sigalrm))
  in
  if wd = "" then f () else changed_wd wd f

let exec_cmd_opt ?(wd = "") prog args =
  try exec_cmd ~wd prog args |> snd |> Option.some
  with ExecCmdFailure _ -> None

type datetime = { y : int; mo : int; d : int; h : int; mi : int; s : int }
[@@deriving show]

let datetime_to_string (t : datetime) =
  Printf.sprintf "%d/%02d/%02d %02d:%02d:%02d" t.y t.mo t.d t.h t.mi t.s

let now () =
  let t = Unix.(time () |> localtime) in
  {
    y = t.tm_year + 1900;
    mo = t.tm_mon + 1;
    d = t.tm_mday;
    h = t.tm_hour;
    mi = t.tm_min;
    s = t.tm_sec;
  }

let marshal_to_file fpath data =
  let oc = open_out_bin fpath in
  Marshal.to_channel oc data [];
  close_out oc

(* The returned type should be explicitly specified,
   e.g., (unmarshal_from_file fpath : some_data_t) *)
let unmarshal_from_file fpath =
  let ic = open_in_bin fpath in
  let data = Marshal.from_channel ic in
  close_in ic;
  data

let compose f g x = f (g x)
let ( let* ) x f = Option.bind x f
let join_paths paths = paths |> List.fold_left Filename.concat ""
let ex_to_result f = try Ok (f ()) with e -> Error e
let ex_raised f = ex_to_result f |> Result.is_error

module List = struct
  include List

  let fold_lefti f a l =
    List.fold_left
      (fun (c, acc) x ->
        let acc = f c acc x in
        (c + 1, acc))
      (0, a) l

  let sub i j l =
    assert (0 <= i && i <= j && j <= List.length l);
    l
    |> fold_lefti (fun c acc x -> if i <= c && c < j then x :: acc else acc) []
    |> snd |> List.rev

  let fold_left01n v0 f1 fn = function
    | [] -> v0
    | x :: xs -> List.fold_left fn (f1 x) xs

  let cross_filter_map f l1 l2 =
    let rec aux acc xs ys =
      match (xs, ys) with
      | [], _ -> acc
      | _ :: xs', [] -> aux acc xs' l2
      | x :: _, y :: ys' -> (
          match f x y with
          | None -> aux acc xs ys'
          | Some res -> aux (res :: acc) xs ys')
    in
    aux [] l1 l2 |> List.rev

  let iota n =
    let rec f acc = function 0 -> acc | n -> f ((n - 1) :: acc) (n - 1) in
    f [] n

  let cross l1 l2 =
    let rec aux acc xs ys =
      match (xs, ys) with
      | [], _ -> acc
      | _ :: xs', [] -> aux acc xs' l2
      | x :: _, y :: ys' -> aux ((x, y) :: acc) xs ys'
    in
    aux [] l1 l2 |> List.rev

  let shift_n n =
    let rec aux acc n = function
      | rest when n = 0 -> (List.rev acc, rest)
      | x :: xs -> aux (x :: acc) (n - 1) xs
      | [] -> failwith "The length of the list is not enough for shift_n"
    in
    aux [] n

  let split_block num_blocks l =
    let block_size =
      let l = float_of_int (List.length l) in
      let n = float_of_int num_blocks in
      l /. n |> Float.floor |> int_of_float
    in
    let rem = List.length l - (num_blocks * block_size) in
    let rec aux acc l = function
      | 0, _ -> List.rev acc
      | n, 0 ->
          let block, rest = shift_n block_size l in
          aux (block :: acc) rest (n - 1, 0)
      | n, r ->
          let block, rest = shift_n (block_size + 1) l in
          aux (block :: acc) rest (n - 1, r - 1)
    in
    aux [] l (num_blocks, rem)

  (* Check if l1 \subeq l2 *)
  let is_subset l1 l2 =
    (* FIXME: More efficient implementation? *)
    l1 |> List.for_all (fun x -> List.mem x l2)
end
