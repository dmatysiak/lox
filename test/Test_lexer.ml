open OUnit2
open Lox.Lexer
open Lox.Lexer_types

let assert_equal_tokens src expected =
  let actual = tokenize src in
  assert_equal ~printer:show_tokens expected actual

let test_scanner _ =
  let source = "\"foo\"" in
  let expected = [] in
  assert_equal_tokens source expected
;;

let suite =
  "Lexer tests"
  >::: [ "Scanner" >:: test_scanner
       ]
;;

let () = run_test_tt_main suite
