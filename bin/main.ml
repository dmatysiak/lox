(*
   let run source =
   let tokens = Scanner.scan_tokens in
   List.iter (fun token -> print_endline token) tokens
*)

let run = print_endline

let run_prompt _ =
  while true do
    print_string "> ";
    let input = read_line () in
    run input
  done
;;

let run_file filename =
  let channel = open_in filename in
  try
    let line = really_input_string channel (in_channel_length channel) in
    run line;
    flush stdout;
    close_in channel
  with
  | e ->
    close_in_noerr channel;
    raise e
;;

let () =
  match Array.length Sys.argv with
  | 0 -> run_prompt ()
  | 2 -> run_file (Array.get Sys.argv 1)
  | _ ->
    print_endline "Usage: lox [script]";
    exit 64
;;
