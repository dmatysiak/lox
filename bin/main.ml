open Lox

(*
   let run source =
   let tokens = Scanner.scan_tokens in
   List.iter (fun token -> print_endline token) tokens
*)

let trim s =
  let n = String.length s in
  if n > 0 && s.[n-1] = '\n' then
    String.sub s 0 (n-1)
  else
    s
;;

let empty_input s =
  let s' = trim s in
  String.length s' = 0
;;

let run line =
  if empty_input line then
    print_string line
  else
    let tokens = Lexer.tokenize line in
    List.iter print_endline tokens
;;

let run_prompt _ =
  while true do
    print_string "> ";
    try
      let input = read_line () in
      run input
    with
    | End_of_file ->
      exit 0
    | e ->
      raise e
  done
;;

let run_file filename =
  let channel = open_in filename in
  try
    let line = really_input_string channel (in_channel_length channel) in
    run line;
    flush stdout;
    close_in channel;
  with
  | e ->
    close_in_noerr channel;
    raise e
;;

let () =
  match Array.length Sys.argv with
  | 1 -> run_prompt ()
  | 2 ->
    run_file (Array.get Sys.argv 1);
    exit 0
  | _ ->
    print_endline "Usage: lox [script]";
    exit 64
;;
