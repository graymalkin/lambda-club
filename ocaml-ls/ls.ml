let user_of_uid uid =
  let pwuid = Unix.getpwuid uid in
  pwuid.Unix.pw_name

let group_of_gid gid =
  let grgid = Unix.getgrgid gid in
  grgid.Unix.gr_name

let char_of_type = function
    Unix.S_REG -> '-'
  | Unix.S_DIR -> 'd'
  | Unix.S_CHR -> 'c'
  | Unix.S_LNK -> 'l'
  | Unix.S_BLK -> 'b'
  | Unix.S_SOCK -> 's'
  | Unix.S_FIFO -> 'p'

let pp_perm fmt perms =
  let triad r w x =
    Format.sprintf "%c%c%c"
      (if r then 'r' else '-')
      (if w then 'w' else '-')
      (if x then 'x' else '-')
  in

  let u_r = (perms land 0o0400) > 0 in
  let u_w = (perms land 0o0200) > 0 in
  let u_x = (perms land 0o0100) > 0 in

  let g_r = (perms land 0o0040) > 0 in
  let g_w = (perms land 0o0020) > 0 in
  let g_x = (perms land 0o0010) > 0 in

  let o_r = (perms land 0o0004) > 0 in
  let o_w = (perms land 0o0002) > 0 in
  let o_x = (perms land 0o0001) > 0 in

  Printf.fprintf fmt "%s%s%s"
    (triad u_r u_w u_x)
    (triad g_r g_w g_x)
    (triad o_r o_w o_x)

let print_long_form file =
  let s = Unix.stat file in
  Printf.printf "%c%a %d %s %s %-d\t%f %s\n"
    (char_of_type s.Unix.st_kind)
    pp_perm s.Unix.st_perm
    s.Unix.st_nlink
    (user_of_uid s.Unix.st_uid)
    (group_of_gid s.Unix.st_gid)
    s.Unix.st_size
    s.Unix.st_mtime
    (Filename.basename file)

let rec ls recursive file =
  let s = Unix.stat file in
  match s.Unix.st_kind with
  | Unix.S_REG ->
    if Options.list_all () || String.get (Filename.basename file) 0 <> '.' then
      (
        if Options.long_form () then
          print_long_form file
        else
          Printf.printf "%s\n" file
      )
  | Unix.S_DIR when recursive ->
    let d_handle = Unix.opendir file in
    (
      try
        while true do
          try
            ls false (Unix.readdir d_handle)
          with Unix.Unix_error (Unix.ENOENT, _, s)->
            Printf.printf "ls: cannot access '%s': %s\n" s (Unix.error_message Unix.ENOENT)
        done
      with End_of_file -> ()
    )
  | Unix.S_DIR ->
    if Options.long_form () then
      print_long_form file
    else
      Printf.printf "%s\n" file
  | _ -> ()


let () = Options.parse_args (ls true)
