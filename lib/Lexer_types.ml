type token_type
  = LeftParen
  | RightParen
  | LeftBrace
  | RightBrace
  | Comma
  | Dot
  | Minus
  | Plus
  | Semicolon
  | Slash
  | Star
  | Bang
  | BangEqual
  | Equal
  | EqualEqual
  | Greater
  | GreaterEqual
  | Less
  | LessEqual
  | Identifier
  | String
  | Number
  | And
  | Class
  | Else
  | False
  | Fun
  | For
  | If
  | Nil
  | Or
  | Print
  | Return
  | Super
  | This
  | True
  | Var
  | While
  | Comment
  | Eof
[@@deriving show]

type line = int
[@@deriving show]
;;

type column = int
[@@deriving show]
;;

type cursor =
  Cursor of { input : string option
            ; curr_char : char option
            ; line : line
            ; column : column
            }
[@@deriving show]
;;

type token =
  Token of { token_type : token_type
           ; lexeme : string option
           ; literal : string option
           ; line : line
           ; column : column
           }
[@@deriving show]
;;

type error =
    LexerError of { msg : string
                  ; line : line
                  ; column : column
                  ; where : string option
                  }
[@@deriving show]
;;

type tokens = (token, error) result list
[@@deriving show]
;;
