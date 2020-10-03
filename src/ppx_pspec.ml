open Ppxlib
open Ppxlib.Ast_helper

let getenv s = try Sys.getenv s with Not_found -> ""

let expander ~loc ~path:_ = function
  | PStr [{ pstr_desc =
              Pstr_eval ({ pexp_loc  = loc;
                           pexp_desc = Pexp_constant (Pconst_string (sym, (None | Some ""))); _ }, _);
            _
          }] ->
    Exp.constant ~loc @@ Pconst_string (getenv sym, None)
  | _ ->
    Location.raise_errorf ~loc "[%%pspec] accepts a string, e.g., [%%pspec \"1111 0000\"]"

let extension =
  Context_free.Rule.extension
    (Extension.declare "pspec" Expression Ast_pattern.(__) expander)

let () =
  Ppxlib.Driver.register_transformation ~rules:[extension] "ppx_pspec"
