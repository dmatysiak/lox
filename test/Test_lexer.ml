open OUnit2
open Lox.Lexer
open Lox.Lexer_types


(*
 * Assertion functions
 *)
let assert_equal_tokens src expected =
  let actual = tokenize src in
  assert_equal ~printer:show_tokens expected actual
;;


(*
 * Lexer helper function tests
 *)
let test_string_partition_first _ =
  let input = "foo" in
  let expected = Some 'f' in
  let actual = first_char input in
  assert_equal expected actual
;;

let test_string_partition_first_empty _ =
  let input = "" in
  let expected = None in
  let actual = first_char input in
  assert_equal expected actual
;;

let test_peek _ =
  let input = Cursor
                { input = Some "oo bar baz"
                ; curr_char = Some 'f'
                ; line = 1
                ; column = 5
                } in
  let expected = Some 'o' in
  let actual = peek input in
  assert_equal expected actual
;;

let test_string_partition_rest _ =
  let input = "foo" in
  let expected = Some "oo" in
  let actual = rest_chars input in
  assert_equal expected actual
;;

let test_string_partition_rest_empty _ =
  let input = "" in
  let expected = None in
  let actual = rest_chars input in
  assert_equal expected actual
;;

let test_advance_midline _ =
  let cur
    = Cursor
        { input = Some "oo bar baz"
        ; curr_char = Some 'f'
        ; line = 1
        ; column = 5
        } in
  let expected
    = Cursor
        { input = Some "o bar baz"
        ; curr_char = Some 'o'
        ; line = 1
        ; column = 6
        } in
  let actual = advance cur in
  assert_equal ~printer:show_cursor expected actual
;;

let test_advance_newline _ =
  let cur
    = Cursor
        { input = Some "\nfoo bar baz"
        ; curr_char = Some 'f'
        ; line = 1
        ; column = 5
        } in
  let expected
    = Cursor
        { input = Some "foo bar baz"
        ; curr_char = Some '\n'
        ; line = 2
        ; column = 0
        } in
  let actual = advance cur in
  assert_equal ~printer:show_cursor expected actual
;;

let test_next_whitespace _ =
  let cur =
    Cursor
      {  input = Some "foo bar baz"
       ; curr_char = None
       ; line = 1
       ; column = 1
      } in
  let expected =
    Cursor
      {  input = Some "bar baz"
       ; curr_char = Some ' '
       ; line = 1
       ; column = 5
      } in
  let actual = seek_next_whitespace cur in
  assert_equal ~printer:show_cursor expected actual
;;

let test_next_whitespace_started _ =
  let cur =
    Cursor
      {  input = Some "oo bar baz"
       ; curr_char = Some 'f'
       ; line = 1
       ; column = 2
      } in
  let expected =
    Cursor
      {  input = Some "bar baz"
       ; curr_char = Some ' '
       ; line = 1
       ; column = 5
      } in
  let actual = seek_next_whitespace cur in
  assert_equal ~printer:show_cursor expected actual
;;

let test_next_token _ =
  let cur =
    Cursor
      {  input = Some "foo bar baz"
       ; curr_char = None
       ; line = 1
       ; column = 1
      } in
  let expected =
    Cursor
      {  input = Some "oo bar baz"
       ; curr_char = Some 'f'
       ; line = 1
       ; column = 2
      } in
  let actual = seek_next_token cur in
  assert_equal ~printer:show_cursor expected actual
;;

let test_next_token_midtoken _ =
  let cur =
    Cursor
      {  input = Some "oo bar baz"
       ; curr_char = Some 'f'
       ; line = 1
       ; column = 2
      } in
  let expected =
    Cursor
      {  input = Some "ar baz"
       ; curr_char = Some 'b'
       ; line = 1
       ; column = 6
      } in
  let actual = seek_next_token cur in
  assert_equal ~printer:show_cursor expected actual
;;

let test_seek_newline _ =
  let cur =
    Cursor
      {  input = Some "foo bar\nbaz"
       ; curr_char = None
       ; line = 1
       ; column = 1
      } in
  let expected =
    Cursor
      {  input = Some "baz"
       ; curr_char = Some '\n'
       ; line = 2
       ; column = 0
      } in
  let actual = seek_newline cur in
  assert_equal ~printer:show_cursor expected actual
;;

let test_char_to_string _ =
  let inp = Some ['h'; 'e'; 'l'; 'l'; 'o'] in
  let expected = Some "hello" in
  let actual = chars_to_string inp in
  assert_equal expected actual
;;

let test_match_string_literal_empty _ =
  let inp = Cursor
              {  input = Some "\""
               ; curr_char = Some '"'
               ; line = 2
               ; column = 2
              } in
  let expected_cursor = Cursor
                          {  input = None
                           ; curr_char = None
                           ; line = 2
                           ; column = 3
                          } in
  let expected_result = mk_token inp
                          (Some [])
                          (Some ['"'; '"'])
                          (String) in
  let (actual_cursor, actual_result) = match_string_literal inp in
  assert_equal ~printer:show_tokens [expected_result] [actual_result];
  assert_equal ~printer:show_cursor expected_cursor actual_cursor
;;

(* exception OhNo of string *)

(* let test_match_string_literal_happy _ = *)
(*   let inp = Cursor *)
(*               {  input = Some "foo baz\"" *)
(*                ; curr_char = Some '"' *)
(*                ; line = 2 *)
(*                ; column = 2 *)
(*               } in *)
(*   let expected_cursor = Cursor *)
(*                           {  input = None *)
(*                            ; curr_char = None *)
(*                            ; line = 2 *)
(*                            ; column = 2 *)
(*                           } in *)
(*   let expected_result = mk_token inp *)
(*                           (Some ['f'; 'o'; 'o'; ' '; 'b'; 'a'; 'r']) *)
(*                           (Some ['"'; 'f'; 'o'; 'o';' '; 'b'; 'a'; 'r'; '"']) *)
(*                           (String) in *)
(*   let (actual_cursor, actual_result) = match_string_literal inp in *)
(*   let default_tok = Token {token_type = String; *)
(*                            lexeme = Some "hihihi"; *)
(*                            literal = Some "jjeje"; *)
(*                            line = 1; *)
(*                            column = 0} in *)
(*   let (Token {token_type = tt1; *)
(*               lexeme = lex1; *)
(*               literal = lit1; *)
(*               line = li1; *)
(*               column = co1}) = Result.value actual_result ~default:default_tok in *)
(*   let (Token {token_type = tt2; *)
(*               lexeme = lex2; *)
(*               literal = lit2; *)
(*               line = li2; *)
(*               column = co2}) = Result.value expected_result ~default:default_tok in *)

(*   if lex1 = lex2 *)
(*   then assert_equal lex1 lex2 *)
(*   else raise (OhNo "oh no!") *)
(*
  assert_equal tt1 tt2;
  assert_equal ~printer:(fun v -> Printf.sprintf "%S" (show_heh v)) lex1 lex2;
  assert_equal ~printer:(fun v -> Printf.sprintf "%S" (show_heh v)) lit1 lit2;
  assert_equal ~printer:(fun v -> Printf.sprintf "%d" v) li1 li2;
  assert_equal ~printer:(fun v -> Printf.sprintf "%d" v) co1 co2;

  assert_equal ~printer:show_token_result expected_result actual_result;
  assert_equal ~printer:show_cursor expected_cursor actual_cursor
 *)
;;

(*
 * Lexer unit tests *
 *)
let test_scanner _ =
  let source = "\"foo\"" in
  let expected = [] in
  assert_equal_tokens source expected
;;


let suite =
  "Lexer tests"
  >::: [ (* Helper tests *)
      "first_char" >:: test_string_partition_first
       ; "first_char on empty" >:: test_string_partition_first_empty
       ; "peek" >:: test_peek
       ; "rest_char" >:: test_string_partition_rest
       ; "rest_char on empty" >:: test_string_partition_rest_empty
       ; "advance midline" >:: test_advance_midline
       ; "advance newline" >:: test_advance_newline
       ; "next token" >:: test_next_token
       ; "next token midtoken" >:: test_next_token_midtoken
       ; "next whitespace" >:: test_next_whitespace
       ; "next whitespace started" >:: test_next_whitespace_started
       ; "next newline" >::  test_seek_newline
       ; "chars to string" >:: test_char_to_string
       (* Lexer tests*)
       ; "string literal empty" >:: test_match_string_literal_empty
       (* ; "string literal happy" >:: test_match_string_literal_happy *)
       ; "scanner" >:: test_scanner
       ]
;;

let () = run_test_tt_main suite
