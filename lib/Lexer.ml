open Lexer_types

let first_char s =
  try
    Some s.[0]
  with
  | _ -> None
;;

let rest_chars s =
  let rem_length = String.length s - 1 in
  if rem_length > 1 then
    Some (String.sub s 1 rem_length )
  else
    None
;;

let adjust_line c line =
  match c with
  | Some '\n' -> line + 1
  | Some _ -> line
  | _ -> line
;;

let adjust_column c col =
  match c with
  | Some '\n' -> 0
  | Some _ -> col + 1
  | _ -> col
;;

let advance (Cursor { input ; line ; column; _ }) =
  let curr_char = Lox_option.( input >>= fun s -> first_char s ) in
  let rest_chars = Lox_option.( input >>= fun s -> rest_chars s ) in
  Cursor
  { input = rest_chars
  ; curr_char = curr_char
  ; line = adjust_line curr_char line
  ; column = adjust_column curr_char column
  }
;;

let peek (Cursor { input ; _ }) =
  Lox_option.( input >>= fun s -> first_char s )
;;

let rec seek_next_token (Cursor {curr_char ; input ; _ } as pos_info) =
  match curr_char, input with
  | None, Some _ -> seek_next_token (advance pos_info)
  | Some c, Some _ ->
    (match c with
     | (' ' | '\r' | '\t') -> seek_next_token (advance pos_info)
     | _ -> pos_info)
  | Some _, None -> pos_info
  | None, None -> pos_info
;;

let rec seek_newline (Cursor {curr_char ; input ; _ } as pos_info) =
  match curr_char, input with
  | None, Some _ -> seek_newline (advance pos_info)
  | Some '\n', Some _ -> advance pos_info
  | _, _ -> pos_info
;;

let chars_to_string cso =
  match cso with
  | Some cs ->
    Some
      (cs
       |> List.map (fun c -> Printf.sprintf "%c" c)
       |> String.concat "")
  | None -> None
;;

let mk_token (Cursor { line ; column ; _ }) lexeme_chars literal_chars token_type =
  Ok (Token { token_type = token_type
            ; lexeme = chars_to_string lexeme_chars
            ; literal = chars_to_string literal_chars
            ; line = line
            ; column = column
            })
;;

let mk_error (Cursor { line ; column ; _ }) msg =
  Error(LexerError { msg = msg
                   ; line = line
                   ; column = column
                   ; where = None
                   })
;;

let match_bang pos_info =
  let bang_char = '!' in
  match (peek pos_info) with
  | Some '=' ->
    let next_pos_info = advance (advance pos_info) in
    (next_pos_info, mk_token pos_info (Some [bang_char; '=']) None BangEqual)
  | Some _ ->
    let next_pos_info = advance pos_info in
    (next_pos_info, mk_token pos_info (Some [bang_char]) None Bang)
  | None -> (pos_info, mk_token pos_info None None Eof)
;;

let match_equality pos_info c tok_ty_0 tok_ty_1 =
  match (peek pos_info) with
  | Some '=' ->
    let next_pos_info = advance (advance pos_info) in
    (next_pos_info, mk_token pos_info (Some [c; '=']) None tok_ty_0)
  | Some _ ->
    let next_pos_info = advance pos_info in
    (next_pos_info, mk_token pos_info (Some [c]) None tok_ty_1)
  | None -> (pos_info, mk_token pos_info None None Eof)
;;

let match_slash pos_info =
  let slash_char = '/' in
  match (peek pos_info) with
  | Some '/' ->
    let next_pos_info = seek_newline pos_info in
    (next_pos_info, mk_token pos_info (Some [slash_char; '/']) None Comment)
  | Some _ ->
    let next_pos_info = advance pos_info in
    (next_pos_info, mk_token pos_info (Some [slash_char]) None Slash)
  | None -> (pos_info, mk_token pos_info None None Eof)
;;

let match_string_literal pos_info =
  let rec seek_end_of_string init_pos_info (Cursor {curr_char ; input ; _ } as pos_info) string_chars =
    match curr_char, input with
    | None, Some _ -> (pos_info, mk_error init_pos_info "Uninitiated string")
    | Some '"', _ ->
      let contents = List.rev string_chars in
      (advance pos_info, mk_token init_pos_info (Some contents) (Some (List.concat [['"'] ; contents ; ['"']])) String)
    | Some c, Some _ -> seek_end_of_string init_pos_info (advance pos_info) (c :: string_chars)
    | Some _, None -> (pos_info, mk_error pos_info "Unterminated string")
    | None, None -> (pos_info, mk_token pos_info None None Eof)
  in
  seek_end_of_string pos_info (advance pos_info) []
;;

let match_tok (Cursor { curr_char ; _ } as pos_info) =
  match curr_char with
  | Some c ->
    (match c with
     (* single character *)
     | '(' -> (advance pos_info, mk_token pos_info (Some [c]) None LeftParen)
     | ')' -> (advance pos_info, mk_token pos_info (Some [c]) None RightParen)
     | '{' -> (advance pos_info, mk_token pos_info (Some [c]) None LeftBrace)
     | '}' -> (advance pos_info, mk_token pos_info (Some [c]) None RightBrace)
     | ',' -> (advance pos_info, mk_token pos_info (Some [c]) None Comma)
     | '.' -> (advance pos_info, mk_token pos_info (Some [c]) None Dot)
     | '-' -> (advance pos_info, mk_token pos_info (Some [c]) None Minus)
     | '+' -> (advance pos_info, mk_token pos_info (Some [c]) None Plus)
     | ';' -> (advance pos_info, mk_token pos_info (Some [c]) None Semicolon)
     | '*' -> (advance pos_info, mk_token pos_info (Some [c]) None Star)
     (* multiple character *)
     | '!' -> match_bang pos_info
     | '=' -> match_equality pos_info c EqualEqual Equal
     | '<' -> match_equality pos_info c LessEqual Less
     | '>' -> match_equality pos_info c GreaterEqual Greater
     | '/' -> match_slash pos_info
     (* literals *)
     | '"' -> match_string_literal pos_info
     (* unrecognized character *)
     | unrecognized -> (pos_info, mk_error pos_info (Printf.sprintf "Unrecognized character '%c'" unrecognized)))
  | None -> (pos_info, mk_token pos_info None None Eof)
;;

let rec scan_token (Cursor { input ; curr_char ; _ } as pos_info) tokens =
  match curr_char, input with
  (* uninitialized *)
  | None, Some _ ->
    let next_pos_info = seek_next_token pos_info in
    scan_token next_pos_info tokens
  (* initialized; input remaining *)
  | Some _, Some _ ->
    let next_pos_info = seek_next_token pos_info in
    let next_pos_info, token = match_tok next_pos_info in
    scan_token next_pos_info (token :: tokens)
  (* initialized; end of input *)
  | Some _, None ->
    let (_, token) = match_tok pos_info in
    List.rev(token :: tokens)
  (* invalid; this should never happen *)
  | None, None -> raise (Failure "Empty input")
;;

let mk_init_cursor s = Cursor { input = Some s
                              ; curr_char = None
                              ; line = 0
                              ; column = 0
                              }
;;

let tokenize line = scan_token (mk_init_cursor line) []
;;
