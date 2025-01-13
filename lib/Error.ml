type error =
  { line : int
  ; where : string option
  ; msg : string
  }

let report { line = line
           ; where = maybe_where
           ; msg = msg
           } =
  match maybe_where with
  | Some where ->
    Printf.eprintf "[line %d] Error%s: %s\n" line where msg
  | _ ->
    Printf.eprintf "[line %d] Error: %s\n" line msg

let error line msg =
  report { line = line
         ; where = None
         ; msg = msg }
