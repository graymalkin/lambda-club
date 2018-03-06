let list_all_val = ref false
let list_all () = !list_all_val

let long_form_val = ref false
let long_form () = !long_form_val

let command_spec =
  Arg.align Arg.[
      "-a", Set list_all_val,
      "  show all files"
    ; "-l", Set long_form_val,
      "  print long form listing"
    ]

let usage = "ls [OPTION]... [FILE]..."

let parse_args f =
  Arg.parse command_spec f usage
