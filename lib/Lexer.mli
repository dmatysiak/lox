open Lexer_types

val first_char : string -> char option
val rest_chars : string -> string option
val adjust_line : char option -> int -> int
val adjust_column : char option -> int -> int
val advance : cursor -> cursor
val peek : cursor -> char option
val seek_next_whitespace : cursor -> cursor
val seek_next_token : cursor -> cursor
val seek_newline : cursor -> cursor
val chars_to_string : char list option -> string option
val mk_token :
  cursor ->
  char list option ->
  char list option ->
  token_type -> (token, 'a) result
val mk_error :
  cursor -> string -> ('a, error) result
val match_bang :
  cursor ->
  cursor * (token, 'a) result
val match_equality :
  cursor ->
  char ->
  token_type ->
  token_type ->
  cursor * (token, 'a) result
val match_slash :
  cursor ->
  cursor * (token, 'a) result
val match_string_literal :
  cursor ->
  cursor *
  (token, error) result
val match_tok :
  cursor ->
  cursor *
  (token, error) result
val scan_token :
  cursor ->
  (token, error) result list ->
  (token, error) result list
val mk_init_cursor : string -> cursor
val tokenize :
  string -> (token, error) result list
