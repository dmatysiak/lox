
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
  | Eof

type error =
  { line : int
  ; where : string option
  ; msg : string
  }


type 'a token =
  { tok_type : token_type
  ; lexeme : string
  ; literal : 'a
  ; line : int
  }

let tokenize line = line
