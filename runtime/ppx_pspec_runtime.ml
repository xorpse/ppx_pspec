open! Angstrom
open Base
open Base.Continue_or_stop

type byte = char

module Bitvec = struct
  include Bitvec

  let of_bits = concat 1

  let of_bools =
    Fn.compose of_bits (List.map ~f:bool)

end

module List = struct
  include List

  let rec append_n n v vs =
    if Int.(n > 0) then append_n Int.(pred n) v (v :: vs) else vs
end

module Modifier = struct
  type t = Prefix | Suffix of string * int
  [@@deriving compare, equal]
end

module Length = struct
  type t = Variable | Fixed of int
  [@@deriving compare, equal]
end

module Order = struct
  type t = Lsb | Msb
  [@@deriving compare, equal]

  let is_lsb = equal Lsb
  let is_msb = equal Msb
end

module Fixed = struct
  type t = Skip | Bit of bool
  [@@deriving compare, equal]

  let bit b = Bit b
  let bits c order =
    let rec go n a i =
      if n <= 0 then a
      else go (Int.pred n) (bit (i land 1 = 1) :: a) (i lsr 1)
    in
    let bits = go 8 [] (Char.to_int c) in
    if Order.(equal order Lsb) then List.rev bits else bits
end

module Directive = struct
  type t =
    | Directive of {
        stop     : bool;
        symbol   : string;
        location : Length.t;
    }
    | Fixed of Fixed.t
  [@@deriving compare, equal]

  let length = function
    | Fixed _ -> 1
    | Directive { location = Length.Fixed v ; _ } -> v
    | _ -> 0
end

module Extractor = struct
  type t = {
    start : int;
    last  : int option;
  }
  [@@deriving compare, equal]

  let make ?last start = {
    start; last
  }
end

module Spec = struct
  type t = {
    length     : Length.t;
    order      : Order.t;
    directives : Directive.t list;
    modifier   : Modifier.t option;
    mask       : Bitvec.t;
    fix        : Bitvec.t;
    modulus    : int;
    extractors : Extractor.t Map.M(String).t;
  }
  [@@deriving compare, equal]

  let make ?modifier ~length ~order ~directives =
    let fmt = if Order.is_msb order then
        List.rev directives
      else
        directives
    in
    let _, modulus = match length with
      | Length.Fixed size -> true, size
      | Length.Variable ->
        false,
        List.fold_until
          ~init:0
          ~f:(fun acc v ->
              let len = Directive.length v in
              if len > 0 then Continue (len + acc) else Stop acc)
          ~finish:Fn.id
          directives
    in
    let (fix, mask, _, _, extractors) = List.fold
        fmt
        ~init:([], [], 0, 0, Map.empty (module String))
        ~f:(fun (fix, mask, count, i, extr) -> function
            | Directive.Fixed Fixed.Skip ->
              (Bitvec.zero :: fix, Bitvec.zero :: mask, count + 1, i + 1, extr)
            | Directive.Fixed (Fixed.Bit b) ->
              (Bitvec.bool b :: fix, Bitvec.one :: mask, count + 1, i + 1, extr)
            | Directive.Directive { symbol; location = Length.Variable; _ } ->
              let fix, mask, count =
                if count < modulus then
                  let diff = modulus - count in
                  List.append_n diff Bitvec.zero fix,
                  List.append_n diff Bitvec.zero mask,
                  modulus
                else
                  fix, mask, count
              in
              (fix, mask, count, i, Map.add_exn extr ~key:symbol ~data:(Extractor.make i))
            | Directive.Directive { stop; symbol; location = Length.Fixed loc } ->
              let i = if stop && Order.is_lsb order then i - loc else i in
              let sta = i in
              let sto = i + loc in
              let fix, mask, count =
                if not stop then
                  List.append_n loc Bitvec.zero fix,
                  List.append_n loc Bitvec.zero mask,
                  count + loc
                else
                  fix, mask, count
              in
              let i = sto in
              let i = if stop && Order.is_msb order then i - loc else i in
              let extr = Map.add_exn extr ~key:symbol ~data:(Extractor.make sta ~last:sto) in
              (fix, mask, count, i, extr)
          )
    in
    let fix = Bitvec.concat 1 @@ List.rev fix in
    let mask = Bitvec.concat 1 @@ List.rev mask in
    { length; order; directives; modifier; fix; mask; modulus; extractors }
end
