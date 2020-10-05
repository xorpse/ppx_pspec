open Base
open Ppxlib
module SP = Ppx_pspec_runtime

let expander ~loc ~path:_ = function
  | PStr [{ pstr_desc =
              Pstr_eval ({ pexp_loc  = loc;
                           pexp_desc = Pexp_constant (Pconst_string (sym, (None | Some ""))); _ }, _);
            _
          }] ->
    let ex = Result.ok_or_failwith @@ SP.Parser.parse_string sym in
    let names = Map.keys ex.extractors in
    let matcher =
      [%expr Bitvec.equal (Bitvec.of_string [%e Ast_builder.Default.estring ~loc (Bitvec.to_string ex.fix)]) bv]
    in
    let applyf = List.fold_right
        names
        ~init:[%expr f]
        ~f:(fun n accf -> [%expr [%e accf ] [%e Ast_builder.Default.evar ~loc n ]])
    in
    let extractedf = Map.fold_right
        ex.extractors
        ~init:applyf
        ~f:(fun ~key:name ~data:ext accf -> [%expr let [%p Ast_builder.Default.pvar ~loc name] = [%e Ast_builder.Default.evar ~loc ("__pspec_transf_" ^ name)]
                                                     (Bitvec.extract bv ~lo:[%e Ast_builder.Default.eint ~loc ext.start ] ~hi:[%e Ast_builder.Default.eint ~loc (Option.value ~default:ex.modulus ext.last)]) in [%e accf]])
    in
    let innerf = List.fold_left
        names
        ~init:[%expr if [%e matcher] then [%e extractedf ] else None ]
        ~f:(fun acc name -> [%expr fun [%p Ast_builder.Default.pvar ~loc ("__pspec_transf_" ^ name)] -> [%e acc] ])
    in
    [%expr fun bv f -> [%e innerf]]
  | _ ->
    Location.raise_errorf ~loc "[%%pspec] accepts a string, e.g., [%%pspec \"1111 0000\"]"

let extension =
  Context_free.Rule.extension
    (Extension.declare "pspec" Expression Ast_pattern.(__) expander)

let () =
  Ppxlib.Driver.register_transformation ~rules:[extension] "ppx_pspec"
